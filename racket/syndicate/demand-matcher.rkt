#lang racket/base
;; A structure (and process!) for matching supply to demand via observation of interests.

(require racket/set)
(require racket/match)
(require "core.rkt")
(require "drivers/timer.rkt")
(require "pretty.rkt")
(require "support/hash.rkt")

(provide (except-out (struct-out demand-matcher) demand-matcher)
	 (rename-out [make-demand-matcher demand-matcher])
	 demand-matcher-update
	 spawn-demand-matcher)

;; A DemandMatcher keeps track of demand for services based on some
;; Projection over a Trie, as well as a collection of functions
;; that can be used to increase supply in response to increased
;; demand, or handle a sudden drop in supply for which demand still
;; exists.
(struct demand-matcher (demand-spec               ;; CompiledProjection
                        supply-spec               ;; CompiledProjection
                        demand-spec-arity         ;; Natural
                        supply-spec-arity         ;; Natural
                        start-task                ;; TaskCallback
                        on-task-exit              ;; TaskCallback
                        current-demand            ;; (Setof (Listof Any))
                        current-supply            ;; (Setof (Listof Any))
                        task-supervisor           ;; TaskSupervisor
                        supervision-states)       ;; (Hashtable (Listof Any) Any)
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print s [p (current-output-port)])
     (pretty-print-demand-matcher s p))])

;; A TaskCallback is a ((Constreeof Action) Any* -> (Constreeof Action)).
;; It is called with an accumulator of actions so-far-computed as its
;; first argument, and with a value for each capture in the
;; DemandMatcher's projection as the remaining arguments.

;; A TaskRunState describes demand for or supply of some task at the
;; time of taking a transition where some aspect of the supervisor's
;; relationship to a particular task has changed, and is one of
;;  - 'low, meaning no demand or supply exists
;;  - 'rising, demand or supply has just appeared
;;  - 'high, demand or supply exists
;;  - 'falling, demand or supply has just disappeared

;; A TaskSupervisor is a
;; ((Listof Any) TaskRunState TaskRunState Any TaskCallback TaskCallback (Constreeof Action)
;;  -> (Transition Any))

(struct task-state (supply-increase-expected? supply-decrease-expected?) #:prefab)

(define default-initial-supervision-state (task-state #f #f))

;; TaskSupervisor. See ../../doc/demand-matcher.md.
(define (default-task-supervisor captures
                                 demand-state
                                 supply-state
                                 supervision-state
                                 start-task
                                 on-task-exit
                                 actions0)
  (match-define (task-state old-i old-d) (or supervision-state default-initial-supervision-state))
  (let*-values (((new-i) old-i)
                ((new-d) old-d)
                ((actions) actions0)

                ((new-i actions) (if (and (not old-i)
                                          (or (eq? demand-state 'rising)
                                              (and (eq? demand-state 'high)
                                                   (or (eq? supply-state 'low)
                                                       (eq? supply-state 'falling)))))
                                     (if (and (eq? demand-state 'high)
                                              (not old-d))
                                         (values new-i (apply on-task-exit actions captures))
                                         (values #t (apply start-task actions captures)))
                                     (values new-i actions)))
                ((new-i) (if (eq? supply-state 'rising)
                             #f
                             new-i))
                ((new-d) (if (and (eq? demand-state 'falling)
                                  (or (eq? supply-state 'rising)
                                      (eq? supply-state 'high)
                                      old-i))
                             #t
                             new-d))
                ((new-d) (if (eq? supply-state 'falling)
                             #f
                             new-d)))
    (transition (and (or new-i new-d)
                     (task-state new-i new-d))
                actions)))

(define (default-on-task-exit s . captures)
  (log-error "demand-matcher: Unexpected drop in supply ~v" captures)
  s)

(define (make-demand-matcher demand-spec supply-spec start-task
                             [on-task-exit default-on-task-exit]
                             #:task-supervisor [task-supervisor default-task-supervisor])
  (demand-matcher demand-spec
                  supply-spec
                  (projection-arity demand-spec)
                  (projection-arity supply-spec)
                  start-task
                  on-task-exit
                  (set)
                  (set)
                  task-supervisor
                  (hash)))

(define (ensure-non-wild s kind spec direction t)
  (when (not s)
    (error 'demand-matcher
           "Wildcard ~a of ~v ~a:\n~a"
           kind
           spec
           direction
           (trie->pretty-string t))))

(define (compute-state tasks added-tasks removed-tasks captures)
  (define present? (set-member? tasks captures))
  (define changing? (or (set-member? added-tasks captures)
                        (set-member? removed-tasks captures)))
  (match* (present? changing?)
    [(#f #f) 'low]
    [(#f #t) 'rising]
    [(#t #f) 'high]
    [(#t #t) 'falling]))

;; DemandMatcher (Constreeof Action) Patch -> (Transition DemandMatcher)
;; Given a Patch from the environment, projects it into supply and
;; demand increase and decrease sets. Calls ChangeHandlers in response
;; to increased unsatisfied demand and decreased demanded supply.
(define (demand-matcher-update d actions0 p)
  (match-define (demand-matcher demand-spec
                                supply-spec
                                demand-arity
                                supply-arity
                                start-task
                                on-task-exit
                                demand
                                supply
                                task-supervisor
                                supervision-states) d)
  (define-values (added-demand removed-demand)
    (patch-project/set #:take demand-arity p demand-spec))
  (define-values (added-supply removed-supply)
    (patch-project/set #:take supply-arity p supply-spec))

  (ensure-non-wild added-demand 'demand demand-spec 'added (patch-added p))
  (ensure-non-wild added-supply 'supply supply-spec 'added (patch-added p))
  (ensure-non-wild removed-demand 'demand demand-spec 'removed (patch-removed p))
  (ensure-non-wild removed-supply 'supply supply-spec 'removed (patch-removed p))

  ;; Though the added and removed sets of patches are always disjoint,
  ;; *after projection* this may not hold. Cancel out any overlaps.
  (let ((overlap (set-intersect added-demand removed-demand)))
    (set! added-demand (set-subtract added-demand overlap))
    (set! removed-demand (set-subtract removed-demand overlap)))
  (let ((overlap (set-intersect added-supply removed-supply)))
    (set! added-supply (set-subtract added-supply overlap))
    (set! removed-supply (set-subtract removed-supply overlap)))

  (define all-changing-tasks
    (set-union added-demand removed-demand added-supply removed-supply))

  (define-values (new-supervision-states actions)
    (for/fold [(supervision-states supervision-states) (actions actions0)]
              [(captures (in-set all-changing-tasks))]
      (define demand-state (compute-state demand added-demand removed-demand captures))
      (define supply-state (compute-state supply added-supply removed-supply captures))
      (match-define (transition new-supervision-state new-actions)
        (task-supervisor captures
                         demand-state
                         supply-state
                         (hash-ref supervision-states captures #f)
                         start-task
                         on-task-exit
                         actions))
      (values (hash-set/remove supervision-states captures new-supervision-state)
              (cons actions new-actions))))

  (transition (struct-copy demand-matcher d
                           [current-demand
                            (set-subtract (set-union demand added-demand) removed-demand)]
                           [current-supply
                            (set-subtract (set-union supply added-supply) removed-supply)]
                           [supervision-states new-supervision-states])
              actions))

;; Behavior :> (Option Event) DemandMatcher -> (Transition DemandMatcher)
;; Handles events from the environment. Only cares about routing-updates.
(define (demand-matcher-handle-event e d)
  (match e
    [(? patch? p)
     (demand-matcher-update d '() p)]
    [_ #f]))

;; Projection Projection (Any* -> (Constreeof Action)) [(Any* -> (Constreeof Action))] -> Action
;; Spawns a demand matcher actor.
(define (spawn-demand-matcher demand-spec
                              supply-spec
                              start-task
                              [on-task-exit #f]
                              #:task-supervisor [task-supervisor default-task-supervisor]
                              #:name [name #f]
			      #:meta-level [meta-level 0])
  (define d (make-demand-matcher (prepend-at-meta demand-spec meta-level)
                                 (prepend-at-meta supply-spec meta-level)
				 (lambda (acs . rs) (cons (apply start-task rs) acs))
                                 (if on-task-exit
                                     (lambda (acs . rs) (cons (apply on-task-exit rs) acs))
                                     default-on-task-exit)
                                 #:task-supervisor task-supervisor))
  (spawn #:name name
         demand-matcher-handle-event
	 d
         (patch-seq (sub (projection->pattern demand-spec) #:meta-level meta-level)
                    (sub (projection->pattern supply-spec) #:meta-level meta-level)
                    (pub (projection->pattern supply-spec) #:meta-level meta-level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pretty-print-demand-matcher s [p (current-output-port)])
  (match-define (demand-matcher demand-spec
                                supply-spec
                                _demand-arity
                                _supply-arity
                                _start-task
                                _on-task-exit
                                current-demand
                                current-supply
                                _task-supervisor
                                supervision-states)
    s)
  (fprintf p "DEMAND MATCHER:\n")
  (fprintf p " - demand-spec: ~v\n" demand-spec)
  (fprintf p " - supply-spec: ~v\n" supply-spec)
  (fprintf p " - demand: ~v\n" current-demand)
  (fprintf p " - supply: ~v\n" current-supply)
  (fprintf p " - supervision-states: ~v\n" supervision-states))
