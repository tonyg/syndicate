#lang racket/base
;; A structure (and process!) for matching supply to demand via observation of interests.

(require racket/set)
(require racket/match)
(require "core.rkt")
(require "drivers/timer.rkt")
(require "../syndicate/pretty.rkt")

(provide (except-out (struct-out demand-matcher) demand-matcher)
	 (rename-out [make-demand-matcher demand-matcher])
	 demand-matcher-update
	 spawn-demand-matcher
         on-claim)

;; A DemandMatcher keeps track of demand for services based on some
;; Projection over a Trie, as well as a collection of functions
;; that can be used to increase supply in response to increased
;; demand, or handle a sudden drop in supply for which demand still
;; exists.
(struct demand-matcher (demand-spec             ;; CompiledProjection
                        supply-spec             ;; CompiledProjection
                        demand-spec-arity       ;; Natural
                        supply-spec-arity       ;; Natural
			increase-handler        ;; ChangeHandler
			decrease-handler        ;; ChangeHandler
                        current-demand          ;; (Setof (Listof Any))
                        current-supply)         ;; (Setof (Listof Any))
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print s [p (current-output-port)])
     (pretty-print-demand-matcher s p))])

;; A ChangeHandler is a ((Constreeof Action) Any* -> (Constreeof Action)).
;; It is called with an accumulator of actions so-far-computed as its
;; first argument, and with a value for each capture in the
;; DemandMatcher's projection as the remaining arguments.

;; ChangeHandler
;; Default handler of unexpected supply decrease.
(define (default-decrease-handler state . removed-captures)
  state)

(define (make-demand-matcher demand-spec supply-spec increase-handler decrease-handler)
  (demand-matcher demand-spec
                  supply-spec
                  (projection-arity demand-spec)
                  (projection-arity supply-spec)
                  increase-handler
                  decrease-handler
                  (set)
                  (set)))

;; DemandMatcher (Constreeof Action) SCN -> (Transition DemandMatcher)
;; Given a SCN from the environment, projects it into supply and
;; demand increase and decrease sets. Calls ChangeHandlers in response
;; to increased unsatisfied demand and decreased demanded supply.
(define (demand-matcher-update d s new-scn)
  (match-define (demand-matcher demand-spec
                                supply-spec
                                demand-arity
                                supply-arity
                                inc-h
                                dec-h
                                demand
                                supply) d)
  (define new-demand (trie-project/set #:take demand-arity (scn-trie new-scn) demand-spec))
  (define new-supply (trie-project/set #:take supply-arity (scn-trie new-scn) supply-spec))
  (define added-demand (set-subtract new-demand demand))
  (define removed-demand (set-subtract demand new-demand))
  (define added-supply (set-subtract new-supply supply))
  (define removed-supply (set-subtract supply new-supply))

  (when (not added-demand) (error 'demand-matcher "Wildcard demand of ~v:\n~a"
                                  demand-spec
                                  (trie->pretty-string (scn-trie new-scn))))
  (when (not added-supply) (error 'demand-matcher "Wildcard supply of ~v:\n~a"
                                  supply-spec
                                  (trie->pretty-string (scn-trie new-scn))))

  (set! supply (set-union supply added-supply))
  (set! demand (set-subtract demand removed-demand))

  (for [(captures (in-set removed-supply))]
    (when (set-member? demand captures) (set! s (apply dec-h s captures))))
  (for [(captures (in-set added-demand))]
    (when (not (set-member? supply captures)) (set! s (apply inc-h s captures))))

  (set! supply (set-subtract supply removed-supply))
  (set! demand (set-union demand added-demand))

  (transition (struct-copy demand-matcher d [current-demand demand] [current-supply supply]) s))

;; Behavior :> (Option Event) DemandMatcher -> (Transition DemandMatcher)
;; Handles events from the environment. Only cares about routing-updates.
(define (demand-matcher-handle-event e d)
  (match e
    [(? scn? s)
     (demand-matcher-update d '() s)]
    [_ #f]))

;; Any* -> (Constreeof Action)
;; Default handler of unexpected supply decrease.
;; Ignores the situation.
(define (unexpected-supply-decrease . removed-captures)
  '())

;; Projection Projection (Any* -> (Constreeof Action)) [(Any* -> (Constreeof Action))] -> Action
;; Spawns a demand matcher actor.
(define (spawn-demand-matcher demand-spec
                              supply-spec
			      increase-handler
			      [decrease-handler unexpected-supply-decrease]
                              #:name [name #f]
			      #:meta-level [meta-level 0])
  (define d (make-demand-matcher (prepend-at-meta demand-spec meta-level)
                                 (prepend-at-meta supply-spec meta-level)
				 (lambda (acs . rs) (cons (apply increase-handler rs) acs))
				 (lambda (acs . rs) (cons (apply decrease-handler rs) acs))))
  (spawn #:name name
         demand-matcher-handle-event
	 d
         (scn/union (subscription (projection->pattern demand-spec) #:meta-level meta-level)
                    (subscription (projection->pattern supply-spec) #:meta-level meta-level)
                    (advertisement (projection->pattern supply-spec) #:meta-level meta-level))))

;; (Trie (Option (Setof (Listof Value))) ... -> (Option (Constreeof Action)))
;;    Trie Projection ...
;; -> Action
;; Spawns a process that observes the given projections. Any time the
;; environment's interests change in a relevant way, calls
;; check-and-maybe-spawn-fn with the aggregate interests and the
;; projection results. If check-and-maybe-spawn-fn returns #f,
;; continues to wait; otherwise, takes the action(s) returned, and
;; quits.
(define (on-claim #:timeout-msec [timeout-msec #f]
                  #:on-timeout [timeout-handler (lambda () '())]
                  #:name [name #f]
                  check-and-maybe-spawn-fn
                  base-interests
                  . projections)
  (define timer-id (gensym 'on-claim))
  (define (on-claim-handler e state)
    (match e
      [(scn new-aggregate)
       (define projection-results
         (map (lambda (p) (trie-project/set #:take (projection-arity p) new-aggregate p))
              projections))
       (define maybe-spawn (apply check-and-maybe-spawn-fn
                                  new-aggregate
                                  projection-results))
       (if maybe-spawn
           (quit maybe-spawn)
           #f)]
      [(message (timer-expired (== timer-id) _))
       (quit (timeout-handler))]
      [_ #f]))
  (list
   (when timeout-msec (message (set-timer timer-id timeout-msec 'relative)))
   (spawn #:name name
          on-claim-handler
          (void)
          (scn/union base-interests
                     (assertion-set-union*
                      (map (lambda (p) (subscription (projection->pattern p))) projections))
                     (subscription (timer-expired timer-id ?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pretty-print-demand-matcher s [p (current-output-port)])
  (match-define (demand-matcher demand-spec
                                supply-spec
                                _demand-arity
                                _supply-arity
                                _increase-handler
                                _decrease-handler
                                current-demand
                                current-supply)
    s)
  (fprintf p "DEMAND MATCHER:\n")
  (fprintf p " - demand-spec: ~v\n" demand-spec)
  (fprintf p " - supply-spec: ~v\n" supply-spec)
  (fprintf p " - demand: ~v\n" current-demand)
  (fprintf p " - supply: ~v\n" current-supply))
