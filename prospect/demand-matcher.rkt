#lang racket/base
;; A structure (and process!) for matching supply to demand via observation of interests.

(require racket/set)
(require racket/match)
(require "core.rkt")
(require (only-in "route.rkt" matcher-project matcher-key-set))
(require "drivers/timer.rkt")

(provide (except-out (struct-out demand-matcher) demand-matcher)
	 (rename-out [make-demand-matcher demand-matcher])
	 demand-matcher-update
	 spawn-demand-matcher
	 on-claim)

;; A DemandMatcher keeps track of demand for services based on some
;; Projection over a Matcher, as well as a collection of functions
;; that can be used to increase supply in response to increased
;; demand, or handle a sudden drop in supply for which demand still
;; exists.
(struct demand-matcher (demand-spec             ;; CompiledProjection
                        supply-spec             ;; CompiledProjection
			increase-handler        ;; ChangeHandler
			decrease-handler        ;; ChangeHandler
                        current-demand          ;; (Setof (Listof Any))
                        current-supply)         ;; (Setof (Listof Any))
	#:transparent)

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
                  increase-handler
                  decrease-handler
                  (set)
                  (set)))

;; DemandMatcher (Constreeof Action) Patch -> (Transition DemandMatcher)
;; Given a Patch from the environment, projects it into supply and
;; demand increase and decrease sets. Calls ChangeHandlers in response
;; to increased unsatisfied demand and decreased demanded supply.
(define (demand-matcher-update d s p)
  (match-define (demand-matcher demand-spec supply-spec inc-h dec-h demand supply) d)
  (define added-demand (matcher-key-set (matcher-project (patch-added p) demand-spec)))
  (define removed-demand (matcher-key-set (matcher-project (patch-removed p) demand-spec)))
  (define added-supply (matcher-key-set (matcher-project (patch-added p) supply-spec)))
  (define removed-supply (matcher-key-set (matcher-project (patch-removed p) supply-spec)))

  (when (not added-demand)
    (error 'demand-matcher "Wildcard demand of ~v:\n~a"
           demand-spec
           (matcher->pretty-string (patch-added p))))
  (when (not added-supply)
    (error 'demand-matcher "Wildcard supply of ~v:\n~a"
           supply-spec
	   (matcher->pretty-string (patch-added p))))

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
    [(? patch? p)
     (demand-matcher-update d '() p)]
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
			      #:meta-level [meta-level 0])
  (define d (make-demand-matcher (compile-projection (prepend-at-meta demand-spec meta-level))
                                 (compile-projection (prepend-at-meta supply-spec meta-level))
				 (lambda (acs . rs) (cons (apply increase-handler rs) acs))
				 (lambda (acs . rs) (cons (apply decrease-handler rs) acs))))
  (spawn demand-matcher-handle-event
	 d
         (sub (projection->pattern demand-spec) #:meta-level meta-level)
         (sub (projection->pattern supply-spec) #:meta-level meta-level)
         (pub (projection->pattern supply-spec) #:meta-level meta-level)))

;; (Matcher (Option (Setof (Listof Value))) ... -> (Option (Constreeof Action)))
;;    Matcher Projection ...
;; -> Action
;; Spawns a process that observes the given projections. Any time the
;; environment's interests change in a relevant way, calls
;; check-and-maybe-spawn-fn with the aggregate interests and the
;; projection results. If check-and-maybe-spawn-fn returns #f,
;; continues to wait; otherwise, takes the action(s) returned, and
;; quits.
(define (on-claim #:timeout-msec [timeout-msec #f]
                  #:on-timeout [timeout-handler (lambda () '())]
                  check-and-maybe-spawn-fn
                  base-interests
                  . projections)
  (define timer-id (gensym 'on-claim))
  (list
   (when timeout-msec (message (set-timer timer-id timeout-msec 'relative)))
   (spawn (lambda (e current-aggregate)
	    (match e
              [(? patch? p)
               (define new-aggregate (update-interests current-aggregate p))
	       (define projection-results
		 (map (lambda (p) (matcher-key-set (matcher-project new-aggregate p))) projections))
	       (define maybe-spawn (apply check-and-maybe-spawn-fn
					  new-aggregate
					  projection-results))
               (if maybe-spawn
                   (quit maybe-spawn)
                   (transition new-aggregate '()))]
	      [(message (timer-expired (== timer-id) _))
               (quit (timeout-handler))]
	      [_ #f]))
          (matcher-empty)
          (patch base-interests (matcher-empty))
          (patch-seq* (map projection->pattern projections))
          (sub (timer-expired timer-id ?)))))
