#lang racket

(provide (all-defined-out))

(require (only-in racket/hash hash-union))
(require racket/generator)

(module+ test
  (require rackunit)
  (require "test-utils.rkt"))

;; -------------------------------------------------------------------------
;; Role Type Data Definitions

;; a FacetName is a symbol

;; a T is one of
;;   - (Role FacetName (Listof EP)), also abbreviated as just Role
;;   - (Spawn Role)
;;   - (Sends τ)
;;   - (Realizes τ)
;;   - (Stop FacetName Body)
(struct Role (nm eps) #:prefab)
(struct Spawn (ty) #:prefab)
(struct Sends (ty) #:prefab)
(struct Realizes (ty) #:prefab)
(struct Stop (nm body) #:prefab)

;; a EP is one of
;;   - (Reacts D Body), describing an event handler
;;   - (Shares τ), describing an assertion
;;   - (Know τ), describing an internal assertion
(struct Reacts (evt body) #:prefab)
(struct Shares (ty) #:prefab)
(struct Know (ty) #:prefab)

;; a Body describes actions carried out in response to some event, and
;; is one of
;;   - T
;;   - (Listof Body)
;;   - (Branch (Listof Body))
(struct Branch (arms) #:prefab)

;; a D is one of
;;   - (Asserted τ), reaction to assertion
;;   - (Retracted τ), reaction to retraction
;;   - (Message τ), reaction to message
;;   - (Know τ), reaction to internal assertion
;;   - (Forget τ), reaction to internal retraction
;;   - (Realize τ), reaction to internal message
;;   - StartEvt, reaction to facet startup
;;   - StopEvt, reaction to facet shutdown
;;   - DataflowEvt, reaction to field updates
(struct Asserted (ty) #:prefab)
(struct Retracted (ty) #:prefab)
(struct Message (ty) #:prefab)
(struct Forget (ty) #:prefab)
(struct Realize (ty) #:prefab)
(define StartEvt 'Start)
(define StopEvt 'Stop)
(define DataflowEvt 'Dataflow)
;; Any -> Bool
;; recognize DataflowEvt
(define (DataflowEvt? x)
  (equal? x DataflowEvt))

;; a D+ is a D with StartEvt and StopEvt replaced with variants that name the
;; specified facet,
;;   - (StartOf FacetName)
;;   - (StopOf FacetName)
(struct StartOf (fn) #:prefab)
(struct StopOf (fn) #:prefab)

;; NOTE: because I'm adding D+ after writing a bunch of code using only D,
;; expect inconsistencies in signatures and names

;; a τ is one of
;;   - (U (Listof τ))
;;   - (Struct StructName (Listof τ ...))
;;   - (Observe τ)
;;   - (List τ)
;;   - (Set τ)
;;   - (Hash τ τ)
;;   - ⋆
;;   - (Base Symbol)
;;   - (internal-label Symbol τ)
(struct U (tys) #:prefab)
(struct Struct (nm tys) #:prefab)
(struct Observe (ty) #:prefab)
(struct List (ty) #:prefab)
(struct Set (ty) #:prefab)
(struct Hash (ty-k ty-v) #:prefab)
(struct Mk⋆ () #:prefab)
(struct internal-label (actor-id ty) #:prefab)
;; TODO this might be a problem when used as a match pattern
(define ⋆ (Mk⋆))
(struct Base (name) #:prefab)
(define Int (Base 'Int))
(define String (Base 'String))
(define Bool (Base 'Bool))
(define Symbol (Base 'Symbol))

;; a StructName is a Symbol

;; --------------------------------------------------------------------------
;; Derived Types

;; τ (Listof EP) -> EP
(define (During assertion eps)
  (define facet-name (gensym 'during-inner))
  (Reacts (Asserted assertion)
          (Role facet-name
                (cons (Reacts (Retracted assertion)
                              (Stop facet-name '()))
                      eps))))

;; -----------------------------------------------------------------------------
;; Compiling Roles to state machines

;; a RoleGraph is a
;;   (role-graph StateName (Hashof StateName State))
;; describing the initial state and the behavior in each state.
(struct role-graph (st0 states) #:transparent)

;; a State is a (state StateName (Hashof D+ (Setof Transition)) (Setof τ))
(struct state (name transitions assertions) #:transparent)

;; a StateName is a (Setof FacetName)
;; let's assume that all FacetNames are unique

;; a Transition is a (transition (Listof TransitionEffect) StateName)
(struct transition (effs dest) #:transparent)
;; a TransitionEffect is one of
;;   - (send τ)
;;   - (realize τ)
(struct send (ty) #:transparent)
(struct realize (ty) #:transparent)

;; a TransitionDesc is a (Hashof D+ (Setof (Listof RoleEffect)), describing the
;; possible ways an event (+/- of an assertion) can alter the facet tree.
;; It always includes the keys (StartOf FacetName) and (StopOf FacetName).
(define (txn-desc0 fn) (hash (StartOf fn) (set) (StopOf fn) (set)))

;; a RoleEffect is one of
;;   - (start RoleName)
;;   - (stop RoleName)
;;   - (send τ)
;;   - (realize τ)
(struct start (nm) #:transparent)
(struct stop (nm) #:transparent)

;; a FacetTree is a
;;   (facet-tree (Hashof FacetName (Listof FacetName))
;;               (Hashof FacetName (U #f FacetName)))
;; describing the potential immediate children of each facet
;; and each facet's parent.
;; For roles that spawn multiple actors, a FacetTree is in fact a forest. The
;; parent of a root facet is #f
(struct facet-tree (down up) #:transparent)


;; RoleGraph -> Nat
(define (role-graph-size rg)
  (for/sum ([st (in-hash-values (role-graph-states rg))])
    (define edge-count (for/sum ([txns (in-hash-values (state-transitions st))])
                         (set-count txns)))
    (add1 edge-count)))

;; Role -> RoleGraph
;; in each state, the transitions will include the reactions of the parent
;; facet(s)
(define (compile role)
  (define labeled-role (label-internal-events role))
  ;; roles# : (Hashof FacetName TransitionDesc)
  (define roles# (describe-roles labeled-role))
  ;; ft : FacetTree
  (define ft (make-facet-tree role))
  ;; assertion# : (Hashof FacetName (Setof τ))
  (define assertion# (all-roles-assertions (enumerate-roles labeled-role)))
  (let loop ([work-list (list (set (Role-nm role)))]
             [states (hash)])
    (match work-list
      [(cons current more)
       (define agg-txn
         (for/fold ([agg (hash)])
                   ([nm (in-set current)])
           (define txns (hash-ref roles# nm))
           (hash-union agg txns #:combine combine-effect-sets)))
       (define (build-transitions D effs)
         (for*/set ([eff* (in-set effs)]
                    [txn (in-set (apply-effects eff* current ft roles#))]
                    ;; TODO - why?
                    ;; filter effect-free self-loops
                    #:unless (and (empty? (transition-effs txn))
                                  (equal? (transition-dest txn) current)))
           txn))
       (define transitions
         (for*/hash ([(D effs) (in-hash agg-txn)]
                     ;; TODO - why was this here?
                     ;; I feel like apply-affects was trying to handle start/stop things
                     ;; #:unless (start/stop-evt? D)
                     [txns (in-value (build-transitions D effs))]
                     #:unless (set-empty? txns))
           (values D txns)))
       (define assertions (assertions-in-state current assertion#))
       (define new-work
         (for*/list ([txns (in-hash-values transitions)]
                     [txn (in-set txns)]
                     [st (in-value (transition-dest txn))]
                     #:unless (equal? st current)
                     #:unless (hash-has-key? states st))
           st))
       (loop (append more new-work)
             (hash-set states current (state current transitions assertions)))]
      ['()
       (role-graph (set (Role-nm role)) states)])))

;; StateName (Hashof FacetName (Setof τ)) -> (Setof τ)
(define (assertions-in-state sn assertion#)
  (for/fold ([assertions (set)])
            ([facet-name (in-set sn)])
    (set-union assertions (hash-ref assertion# facet-name (set)))))

(module+ test
  (test-case
      "facets created in OnStart handled properly"
    (define strt
      '(Role (x)
         (Reacts OnStart
                 (Role (y)
                   (Shares (Hi))
                   (Reacts (Asserted (Bye))
                           (Stop y))))))
    (define r (parse-T strt))
    (define rg (run/timeout (thunk (compile r))))
    (check-true (role-graph? rg))
    (match-define (role-graph st0 st#) rg)
    (check-true (hash-has-key? st# (set 'x 'y)))))

;; a DetectedCylce is a (detected-cycle StateName (Listof TraversalStep)), as in
;;   (detected-cycle start steps)
;; where path represents the sequences of states containing a cycle,
(struct detected-cycle (start steps) #:transparent)

;; a TraversalStep is a (traversal-step D StateName)
;; representing a state transition along an edge matching D to a destination state
(struct traversal-step (evt dest) #:transparent)

;; RoleGraph Role -> (U RoleGraph DetectedCycle)
;; "Optimize" the given role graph with respect to internal events.
;; The resulting graph will have transitions of only external events.
(define (compile/internal-events rg)
  (match-define (role-graph st0 orig-st#) rg)
  ;; doing funny business with state (set) here
  (define orig-st#+ (hash-set orig-st# (set) (state (set) (hash) (set))))

  ;; a WorkItem is a
  ;;   (work-item TraversalStep (Listof TraversalStep) D+ (Listof D+) (Listof TransitionEffect))
  ;; such as (work-item from path/r to by with effs), where
  ;;   - from is the origin state for this chain of events
  ;;   - path/r is the list of states in the path to this point, *after* from, in reverse
  ;;     (meaning that all of these transitions are due to *internal* events)
  ;;   - to is the current state that has been reached
  ;;   - by is the external event that kicked off this sequence
  ;;   - with is a list of pending events to be processed
  ;;   - effs are the external effects emitted on this path
  (struct work-item (from path/r to by with effs) #:transparent)

  (let/ec fail
    (define (walk work visited st#)
      (match work
        ['()
         (define mt-txns (hash-ref (hash-ref st# (set)) StartEvt))
         (define new-st0
           (cond
             [(and (= (set-count mt-txns) 1)
                   (empty? (transition-effs (set-first mt-txns))))
              (transition-dest (set-first mt-txns))]
             [else
              (set)]))
         (define states
           (for/hash ([(sn txns) (in-hash st#)]
                      ;; handle empty state below
                      #:unless (set-empty? sn))
             (define old-assertions (state-assertions (hash-ref orig-st#+ sn)))
             (define new-assertions (external-assertions old-assertions))
             (values sn (state sn txns new-assertions))))
         (when (set-empty? new-st0)
           (error 'compile-internal-events "not able to remove initial start event"))
         (when (target-of-transition? (set) st#)
           ;; get rid of the empty state unless some other state transitions to it
           (set! states (hash-set states (set) (state (set) (hash) (set)))))
         (role-graph new-st0 states)]
        [(cons (work-item from path/r to by with effs) more-work)
         (match-define (traversal-step last-evt cur-st) to)
         (define prev (if (empty? path/r) from (traversal-step-dest (first path/r))))
         (define prev-assertions (state-assertions (hash-ref orig-st#+ prev)))
         (match-define (state _ txn# cur-assertions) (hash-ref orig-st#+ cur-st))
         (define new-state-changes (route-internal prev-assertions
                                                   cur-assertions))
         (define state-changes* (for/list ([D (in-set new-state-changes)]
                                           #:when (for/or ([D/actual (in-hash-keys txn#)])
                                                    (D<:? D D/actual)))
                                  D))
         (define started (for*/list ([fn (in-set (set-subtract cur-st prev))]
                                     [D (in-value (StartOf fn))]
                                     #:when (hash-has-key? txn# D))
                           D))
         (define stopped (for*/list ([fn (in-set (set-subtract prev cur-st))]
                                     [D (in-value (StopOf fn))]
                                     #:when (hash-has-key? txn# D))
                           D))
         (define new-events (append started stopped state-changes*))

         ;; (Listof D+) -> (Listof WorkItem)
         ;; Try to dispatch the first relevant pending event, which yields a
         ;; collection of work items based on its effects
         (define (pending-evts->work-items pending-evts)
           (define pending/first-relevant
             (dropf pending-evts
                    (lambda (evt)
                      (not
                       (for/or ([D (in-hash-keys txn#)])
                         ;; TODO - think I want non-empty intersection instead of subtyping
                         (and (D<:? evt D)
                              ;; don't want dataflow edges to gobble up all events
                              (implies (DataflowEvt? D) (DataflowEvt? evt))))))))
           (match pending/first-relevant
             ['()
              '()]
             [(cons evt more-pending)
              (define path/r+ (cons to path/r))
              (for*/list ([(D ts) (in-hash txn#)]
                          #:when (D<:? evt D)
                          #:when (implies (DataflowEvt? D) (DataflowEvt? evt))
                          [t (in-set ts)])
                (match-define (transition more-effs dest) t)
                (check-for-cycle! from path/r+ evt dest fail)
                (define-values (internal-effs external-effs)
                  (partition-transition-effects more-effs))
                (work-item from
                           path/r+
                           (traversal-step evt dest)
                           by
                           (append more-pending internal-effs)
                           (append effs external-effs)))]))

         ;; NOTE: knowledge of scheduling used here
         (define pending*
           (for*/list ([schedule (in-permutations new-events)]
                       [evts (in-value (append with schedule))]
                       [df? (in-list (if (hash-has-key? txn# DataflowEvt)
                                         (list #t #f)
                                         (list #f)))])
             (if df? (cons DataflowEvt evts) evts)))
         (define induced-work (map pending-evts->work-items pending*))
         (define induced-work* (remove-duplicates (flatten induced-work)))
         (cond
           [(ormap empty? induced-work)
            ;; this is the end of some path
            (define visited+ (set-add visited cur-st))
            (define new-paths-work
              (for*/list (#:unless (set-member? visited cur-st)
                          [(D txns) (in-hash txn#)]
                          #:when (external-evt? D)
                          #:unless (equal? D DataflowEvt)
                          [t (in-set txns)])
                (match-define (transition es dst) t)
                (define-values (internal-effs external-effs)
                  (partition-transition-effects es))
                (work-item cur-st '() (traversal-step D dst) D internal-effs external-effs)))
            (define new-st# (update-path st# from cur-st by effs))
            (walk (append more-work induced-work* new-paths-work) visited+ new-st#)]
           [else
            (walk (append more-work induced-work*) visited st#)])]))
    (walk (list (work-item (set) '() (traversal-step StartEvt st0) StartEvt '() '()))
          (set)
          (hash))))

;; (Listof TraceStep) D StateName (DetectedCycle -> X) -> (U X Void)
;; the path is in reverse, and the final step is the pair evt/dest;
;; so their is a cycle if the sequence from the first occurrence of
;; dest in the path matches the sequence from the second occurrence to
;; the first.
(define (check-for-cycle! from path/r evt dest fail)
  ;; TraceStep -> Bool
  (define (same-state? step) (equal? dest (traversal-step-dest step)))

  ;; (Listof TraceStep) -> (Values (Listof TraceStep) (Listof TraceStep))
  (define (split-at-same-state steps) (splitf-at steps (compose not same-state?)))

  (define-values (loop1 trail) (split-at-same-state path/r))
  (when (cons? trail)
    (match-define (cons prior-last trail2) trail)
    (define-values (loop2 trail3) (split-at-same-state trail2))
    (define last-step (traversal-step evt dest))
    (when (and (cons? trail3)
               (equal? (cons last-step loop1)
                       (cons prior-last loop2)))
      (fail (detected-cycle from (reverse (cons last-step path/r)))))))

(module+ test
  (test-case
      "most minimal functionality for removing internal events"
    ;; manager role has basically nothing to it
    (define m (compile manager))
    (define i (compile/internal-events m))
    (check-true (role-graph? i))
    (check-true (simulates?/rg i m))
    (check-true (simulates?/rg m i))
    ;; this isn't necessarily *needed*, but nice to know
    (check-equal? i m))
  (test-case
      "removing internal events on more involved role"
    ;; because it doesn't use any internal events, it should be unaffected
    (define tmr (parse-T task-runner-ty))
    (define tm (compile tmr))
    (define tmi (compile/internal-events tm))
    (check-true (role-graph? tmi))
    ;; I'm not exactly sure how the two should be related via simulation :S
    (check-true (simulates?/rg tmi tm)))
  (test-case
      "detect a simple internal event cycle"
    (define cyclic
      '(Role (x)
             (Reacts (Realize Int)
                     (Realizes Int))
             (Reacts OnStart
                     (Realizes Int))))
    (define r (parse-T cyclic))
    (define rg (compile r))
    (define i (run/timeout (thunk (compile/internal-events rg))))
    (check-true (detected-cycle? i))
    (check-match i
                 (detected-cycle
                  (== (set))
                  (list (traversal-step 'Start (== (set 'x)))
                        (traversal-step (StartOf 'x) (== (set 'x)))
                        (traversal-step (Realize (internal-label _ (== Int))) (== (set 'x)))
                        (traversal-step (Realize (internal-label _ (== Int))) (== (set 'x)))))))
  (test-case
      "interesting internal start event"
    (test-case
        "facets created in OnStart handled properly"
      (define strt
        '(Role (x)
               (Reacts OnStart
                       (Role (y)
                             (Shares (Hi))
                             (Reacts (Asserted (Bye))
                                     (Stop y))))))
      (define r (parse-T strt))
      (define rg (run/timeout (thunk (compile r))))
      (check-true (role-graph? rg))
      (define rgi (run/timeout (thunk (compile/internal-events rg))))
      (check-true (role-graph? rgi))
      (match-define (role-graph st0 st#) rgi)
      (check-equal? st0 (set 'x 'y))
      (check-true (hash-has-key? st# (set 'x 'y)))
      (define xy-txns (state-transitions (hash-ref st# (set 'x 'y))))
      (check-equal? xy-txns (hash (Asserted (Struct 'Bye '()))
                                  (set (transition '() (set 'x)))))
      (check-true (hash-has-key? st# (set 'x)))
      (define x-txns (state-transitions (hash-ref st# (set 'x))))
      (check-equal? x-txns (hash))))
  (test-case
      "remove internal effects from transitions"
    (define role
      (Role 'x
            (list (Reacts (Asserted Int)
                          (list (Realizes String)
                                (Sends Int)
                                (Role 'y (list)))))))
    (define rg (run/timeout (thunk (compile role))))
    (check-true (role-graph? rg))
    (define rgi (run/timeout (thunk (compile/internal-events rg))))
    (check-true (role-graph? rgi))
    (define state# (role-graph-states rgi))
    (check-true (hash-has-key? state# (set 'x)))
    (define txn# (state-transitions (hash-ref state# (set 'x))))
    (check-equal? txn#
                  (hash (Asserted Int) (set (transition (list (send Int)) (set 'x 'y))))))

  (test-case
      "regression: type equality instead of subtyping on internal transitions"
    (define desc '(Role (x)
                        (Know (Tuple Int))
                        (Reacts (Know (Tuple ★/t))
                                (Role (y)))))
    (define role (run/timeout (thunk (parse-T desc))))
    (define rg (run/timeout (thunk (compile role))))
    (check-true (role-graph? rg))
    (define rgi (run/timeout (thunk (compile/internal-events rg))))
    (check-true (role-graph? rgi))
    (check-match rgi
                 (role-graph (== (set 'x 'y))
                             (hash-table ((== (set 'x 'y)) (state (== (set 'x 'y))
                                                                  (== (hash))
                                                                  (== (set)))))))))

;; (Setof τ) (Setof τ) -> (Setof D)
;; Subtyping-based assertion routing (*not* intersection - TODO)
(define (route-internal prev current)
  ;; note that messages are handled separately, don't need to worry about them
  ;; here
  (define old-interests (internal-interests prev))
  (define old-matches (matching-interests old-interests prev))
  (define new-interests (internal-interests current))
  (define new-matches (matching-interests new-interests current))
  (define appeared (label-assertions (assertion-delta new-matches old-matches) Know))
  (define disappeared (label-assertions (assertion-delta old-matches new-matches) Forget))
  (define appearing-interests (assertion-delta new-interests old-interests))
  (define newly-relevant (label-assertions (matching-interests appearing-interests current) Know))
  (set-union appeared disappeared newly-relevant))

(module+ test
  (test-case
      "test routing"
    (define interest (internal-label 'x
                                     (parse-τ '(Observe
                                                (SlotAssignment (ReqID (Bind (Tuple Int Symbol))
                                                                       (Bind Symbol))
                                                                Discard)))))
    (define request (internal-label 'x
                                    (parse-τ '(SlotAssignment (ReqID (Tuple Int Symbol) Symbol)
                                                              (Bind Symbol)))))
    (define expected (set (Know request)))
    (check-equal? (route-internal (set interest) (set interest request))
                  expected)
    (check-equal? (route-internal (set request) (set interest request))
                  expected)
    (check-equal? (route-internal (set) (set interest request))
                  expected))
  (test-case
      "test routing internally-labeled assertions"
    (define interest (internal-label 'x
                                     (parse-τ '(Observe
                                                (SlotAssignment (ReqID (Bind (Tuple Int Symbol))
                                                                       (Bind Symbol))
                                                                Discard)))))
    (define request (internal-label 'x
                                    (parse-τ '(SlotAssignment (ReqID (Tuple Int Symbol) Symbol)
                                                              (Bind Symbol)))))
    (define expected (set (Know request)))
    (check-equal? (route-internal (set interest) (set interest request))
                  expected)
    (check-equal? (route-internal (set request) (set interest request))
                  expected)
    (check-equal? (route-internal (set) (set interest request))
                  expected)))

;; (Setof τ) -> (Setof τ)
;; the type of interests in a set
(define (internal-interests as)
  (for/set ([a (in-set as)]
            #:when (and (internal-label? a)
                        (Observe? (internal-label-ty a))))
    (internal-label (internal-label-actor-id a)
                    (Observe-ty (internal-label-ty a)))))

;; (Setof τ) (Setof τ) -> (Setof τ)
;; The assertions in as that have a matching (supertype) interest in is
(define (matching-interests is as)
  (for/set ([a (in-set as)]
            #:when (contains-supertype? is a))
    a))

;; (Setof τ) τ -> Bool
;; does the set contain a type that is a supertype of a?
(define (contains-supertype? as a)
  (for/or ([x (in-set as)])
    (<:? a x)))

;; (Setof τ) (Setof τ) -> (Setof τ)
;; Computes as1 - as2, up to suptyping, applying xform to each element
(define (assertion-delta as1 as2)
  (for/set ([a1 (in-set as1)]
            #:unless (contains-supertype? as2 a1))
    a1))

;; (Setof τ) (τ -> X) -> (Setof X)
;; apply a procedure to each assertion in a set
(define (label-assertions as f)
  (for/set ([a (in-set as)])
    (f a)))

;; (Setof τ) -> (Setof τ)
;; remove all internal-label assertions in a set
(define (external-assertions assertions)
  (for/set ([a (in-set assertions)]
            #:unless (internal-label? a))
    a))

;; (Hashof StateName (Hashof D (Setof Transition)))
;; StateName
;; StateName
;; D
;; (Listof TransitionEffects)
;; -> (Hashof StateName (Hashof D (Setof Transition)))
;; record an edge between from and to based on the given event and emitting some effects
(define (update-path st# from to by effs)
  (cond
    [(and (equal? from to)
          (empty? effs))
     st#]
    [else
     (define txn (transition effs to))
     ;; make sure to is in the hash
     (define st#+to (hash-update st# to values (hash)))
     (hash-update st#+to
                  from
                  (lambda (txn#)
                    (hash-update txn#
                                 by
                                 (lambda (txns)
                                   (set-add txns txn))
                                 (set)))
                  (hash))]))

;; (Listof (TransitionEffect)) -> (Values (Listof D) (Listof TransitionEffect))
;; partition the internal and external effects, translating realize effects to
;; Realize events along the way
(define (partition-transition-effects effs)
  (define-values (internals externals) (partition realize? effs))
  (define (realize->Realize e) (Realize (realize-ty e)))
  (values (map realize->Realize internals)
          externals))

;; D -> Bool
;; test if D corresponds to an external event (assertion, message)
(define (external-evt? D)
  (match D
    [(or (Asserted _)
         (Retracted _)
         (Message _))
     #t]
    ;; TODO - hacky
    [(== DataflowEvt)
     #t]
    [_
     #f]))

;; D+ -> Bool
;; test if D corresponds to Start or Stop event
(define (start/stop-evt? D)
  (or (equal? D StartEvt)
      (equal? D StopEvt)
      (StartOf? D)
      (StopOf? D)))

;; StateName (Hashof StateName (Hashof D+ (Setof Transition))) -> Bool
;; do any of the transitions go to `sn`?
(define (target-of-transition? sn st#)
  (for*/or ([txn# (in-hash-values st#)]
            [txns (in-hash-values txn#)]
            [txn (in-set txns)])
    (equal? sn (transition-dest txn))))

(module+ test
  (test-case
      "compile seller"
    (define rg (compile seller))
    (check-true (role-graph? rg))
    (match-define (role-graph sn0 seller#) rg)
    (check-equal? sn0 (set 'seller))
    (check-true (hash-has-key? seller# (set 'seller)))
    (check-true (hash-has-key? seller# (set 'seller 'fulfill)))
    (check-equal? (list->set (hash-keys seller#))
                  (set (set 'seller 'fulfill)
                       (set 'seller)))
    (define st0 (hash-ref seller# (set 'seller)))
    (define transitions (state-transitions st0))
    (define quote-request
      (Observe (book-quote String ⋆)))
    (check-true (hash-has-key? transitions (Asserted quote-request)))
    (check-equal? (hash-ref transitions (Asserted quote-request))
                  (set (transition '() (set 'seller 'fulfill)))))
  (test-case
      "compile role that quits"
    (define r
      (Role 'x
            (list (Reacts (Asserted Int)
                          (Stop 'x '())))))
    (define rg (compile r))
    (check-true (role-graph? rg))
    (match-define (role-graph sn0 state#) rg)
    (check-equal? sn0
                  (set 'x))
    (check-true (hash-has-key? state# (set)))
    (check-true (hash-has-key? state# (set 'x)))
    (define state0 (hash-ref state# (set 'x)))
    (define transitions (state-transitions state0))
    (check-true (hash-has-key? transitions (Asserted Int)))
    (check-equal? (hash-ref transitions (Asserted Int))
                  (set (transition '() (set)))))

  (test-case
      "leader-revised should have a quote/poll cycle"
    (define rg (compile leader-revised))
    (check-true (role-graph? rg))
    (match-define (role-graph sn0 state#) rg)
    (check-true (hash? state#))
    (check-true (hash-has-key? state# (set 'get-quotes)))
    (define gq-st (hash-ref state# (set 'get-quotes)))
    (check-true (state? gq-st))
    (match-define (state _ gq-transitions _) gq-st)
    (define bq (Asserted (book-quote String Int)))
    (check-true (hash? gq-transitions))
    (check-true (hash-has-key? gq-transitions bq))
    (define txns (hash-ref gq-transitions bq))
    (check-true (set? txns))
    (define dests (for/set ([t (in-set txns)])
                    (transition-dest t)))
    (check-true (set? txns))
    (check-true (set-member? dests (set 'get-quotes 'poll-members)))
    (check-true (hash-has-key? state# (set 'get-quotes 'poll-members)))
    (define gqpm-st (hash-ref state# (set 'get-quotes 'poll-members)))
    (check-true (state? gqpm-st))
    (match-define (state _ gqpm-transitions _) gqpm-st)
    (define bi (Asserted (book-interest String String ⋆)))
    (check-true (hash? gqpm-transitions))
    (check-true (hash-has-key? gqpm-transitions bi))
    (define txns2 (hash-ref gqpm-transitions bi))
    (check-true (set? txns2))
    (define dests2 (for/set ([t (in-set txns2)])
                     (transition-dest t)))
    (check-true (set? dests2))
    (check-true (set-member? dests2 (set 'get-quotes))))

  (test-case
      "simplified poll should quit"
    (define poll/simpl
      (Role
       'poll-members
       (list
        (Reacts
         (Asserted (book-interest String String ⋆))
         (list
          (Branch
           (list
            (Stop 'poll-members
                  (Branch (list
                           (Stop 'get-quotes (list))
                           (list))))
            (list))))))))
    (define transition# (describe-role poll/simpl))
    (check-true (hash? transition#))
    (define bi (Asserted (book-interest String String ⋆)))
    (check-true (hash-has-key? transition# bi))
    (define effs (hash-ref transition# bi))
    (check-true (set? effs))
    (check-true (set-member? effs (list (stop 'poll-members))))
    )
  (test-case
      "Body->effects of simplified poll"
    (define poll/simpl/body
         (Stop 'poll-members
               (Branch (list
                        (Stop 'get-quotes (list))
                        (list)))))
    (define effs (Body->effects poll/simpl/body))
    (check-true (set? effs))
    (check-true (set-member? effs (list (stop 'poll-members) (stop 'get-quotes))))
    (check-true (set-member? effs (list (stop 'poll-members)))))
  (test-case
      "Body->effects of even more simplified poll"
    (define poll/simpl/body/simpl
      (Branch (list
               (Stop 'get-quotes (list))
               (list))))
    (define effs (Body->effects poll/simpl/body/simpl))
    (check-true (set? effs))
    (check-equal? effs
                  (set (list (stop 'get-quotes)) (list)))))

;; Role -> FacetTree
(define (make-facet-tree role)
  (let loop (;; the work list contains pairs describing the immediate parent and a thing to analyze
             [work (list (cons #f role))]
             [downs (hash)]
             [ups (hash)])
    (match work
      ['()
       (facet-tree downs ups)]
      [(cons (cons parent T) rest)
       (match T
         [(or (Sends _)
              (Realizes _))
          (loop rest downs ups)]
         [(Spawn role)
          (loop (cons (cons #f role) rest) downs ups)]
         [(Role nm eps)
          ;; record the parent/child relationship
          (define downs2 (hash-update downs
                                      parent
                                      ((curry cons) nm)
                                      (list)))
          (define downs3 (hash-set downs2 nm (list)))
          (define ups2 (hash-set ups nm parent))
          (define more-work*
            (for/list ([ep (in-list eps)]
                       #:when (Reacts? ep))
              (match-define (Reacts _ body) ep)
              (map ((curry cons) nm) (Body->actions body))))
          (loop (apply append rest more-work*)
                downs3
                ups2)]
         [(Stop target body)
          (define new-parent (hash-ref ups target #f))
          (define more-work
            (for/list ([k (in-list (Body->actions body))])
              (cons new-parent k)))
          (loop (append rest more-work)
                downs
                ups)])])))

(module+ test
  (test-case
      "basic spawn functionality"
    (define seller (parse-T real-seller-ty))
    (define seller+spawn (Role 'start (list (Reacts StartEvt (Spawn seller)))))
    (define rg (run/timeout (thunk (compile seller+spawn))))
    (check-true (role-graph? rg))
    (define rgi (compile/internal-events rg))
    (check-true (role-graph? rgi))
    (define srg (compile seller))
    (check-true (run/timeout (thunk (simulates?/rg rg rg))))
    (check-false (run/timeout (thunk (simulates?/rg rg srg))))
    (check-true (run/timeout (thunk (simulates?/rg srg rg))))
    (check-true (run/timeout (thunk (simulates?/rg rgi srg))))
    (check-true (run/timeout (thunk (simulates?/rg srg rgi)))))
  (test-case
      "internal events across spawns"
    (define role
      (Role 'start
            (list
             (Know Int)
             (Reacts StartEvt
                     (Spawn (Role 'spawned
                                  (list (Reacts (Know Int)
                                                (Role 'know (list))))))))))
    (define rg (run/timeout (thunk (compile role))))
    (check-true (role-graph? rg))
    (define rgi (run/timeout (thunk (compile/internal-events rg))))
    (check-true (role-graph? rgi))
    (define state-names (hash-keys (role-graph-states rgi)))
    (for ([sn (in-list state-names)])
      ;; that facet shouldn't be reachable
      (check-false (set-member? sn 'know)))))

;; Body -> (Listof T)
;; extract the list of all Role, Stop, and Spawn types from a Body
(define (Body->actions body)
  (match body
    [(? list?)
     (apply append (map Body->actions body))]
    [(Branch arms)
     (apply append (map Body->actions arms))]
    [T (list T)]))

(module+ test
  (test-case
      "Body->actions Branch"
    (define body (Branch
                  (list
                   (Stop 'leader
                         (Role 'announce
                               (list
                                (Shares (Struct 'book-of-the-month String)))))
                   (Stop 'poll (list)))))
    (check-equal? (Body->actions body)
                  (list (Stop 'leader
                              (Role 'announce
                                    (list
                                     (Shares (Struct 'book-of-the-month String)))))
                        (Stop 'poll (list))))))

(module+ test
  (test-case
      "manager facet tree (one facet)"
    (define ft (make-facet-tree manager))
    (check-true (facet-tree? ft))
    (match-define (facet-tree downs ups) ft)
    (check-equal? (hash-ref downs #f)
                  (list 'account-manager))
    (check-equal? (hash-ref downs 'account-manager)
                  (list))
    (check-equal? (hash-ref ups 'account-manager)
                  #f))
  (test-case
      "seller facet tree (two facets)"
    (define ft (make-facet-tree seller))
    (check-true (facet-tree? ft))
    (match-define (facet-tree downs ups) ft)
    (check-equal? (hash-ref downs #f)
                  (list 'seller))
    (check-equal? (hash-ref downs 'seller)
                  (list 'fulfill))
    (check-equal? (hash-ref ups 'seller)
                  #f)
    (check-equal? (hash-ref ups 'fulfill)
                  'seller)

    (test-case
        "leader-spec facet tree (stops facets)"
      (define ft (make-facet-tree leader-spec))
      (check-true (facet-tree? ft))
      (match-define (facet-tree downs ups) ft)
      (check-equal? (list->set (hash-ref downs #f))
                    (set 'leader 'announce))
      (check-equal? (hash-ref downs 'leader)
                    (list 'poll))
      (check-equal? (hash-ref downs 'poll)
                    (list))
      (check-equal? (hash-ref downs 'announce)
                    (list))
      (check-equal? (hash-ref ups 'leader)
                    #f)
      (check-equal? (hash-ref ups 'announce)
                    #f)
      (check-equal? (hash-ref ups 'poll)
                    'leader))
))

;; FacetName FacetName FacetTree -> Bool
;; determine if the first argument is a child*, or equal to, the second
(define (ancestor? desc ansc ft)
  (cond
    [(equal? desc ansc)
     #t]
    [else
     (define parent (hash-ref (facet-tree-up ft) desc))
     (and parent
          (ancestor? parent ansc ft))]))

;; FacetName FacetName FacetTree -> (U #f Nat)
;; determine if the first argument is a child*, or equal to, the second; if so,
;; return their distance from one another in the tree
(define (ancestor?/dist desc ansc ft)
  (cond
    [(equal? desc ansc)
     0]
    [else
     (define parent (hash-ref (facet-tree-up ft) desc))
     (define ans? (and parent (ancestor?/dist parent ansc ft)))
     (and ans?
          (add1 ans?))]))


(module+ test
  (test-case
      "ancestors in leader-spec facet tree"
    (define ft (make-facet-tree leader-spec))
    (check-true (ancestor? 'leader 'leader ft))
    (check-true (ancestor? 'poll 'leader ft))
    (check-false (ancestor? 'leader 'poll ft))
    (check-false (ancestor? 'announce 'leader ft)))
  (test-case
      "ancestor?/dist in leader-spec facet tree"
    (define ft (make-facet-tree leader-spec))
    (check-equal? (ancestor?/dist 'leader 'leader ft) 0)
    (check-equal? (ancestor?/dist 'poll 'leader ft) 1)
    (check-false (ancestor?/dist 'leader 'poll ft))
    (check-false (ancestor?/dist 'announce 'leader ft))))

;; (Listof RoleEffect)
;; StateName
;; FacetTree
;; (Hashof FacetName TransitionDesc)
;; -> (Setof Transition)
;; determine the state resulting from some effects, given the currently active
;; facets and a description of possible facet locations and behavior.
(define (apply-effects effs st ft txn#)
  (let loop ([st st]
             [effs effs])
    (match effs
      ['()
       (set (transition '() st))]
      [(cons eff rest)
       (match eff
         [(or (send _)
              (realize _))
          (for/set ([txn (in-set (loop st rest))])
            (transition (cons eff (transition-effs txn)) (transition-dest txn)))]
         [(start nm)
          (define st+ (set-add st nm))
          (define start-effs (hash-ref (hash-ref txn# nm) (StartOf nm)))
          (cond
            [(set-empty? start-effs)
             (loop st+ rest)]
            [else
             (for*/set ([eff* (in-set start-effs)]
                        [result (in-set (loop st+ (append rest eff*)))])
               result)])]
         [(stop nm)
          (define children (find-children ft nm st))
          (define st-
            (for/fold ([st st])
                      ([c (in-list children)])
              (set-remove st c)))
          (define-values (final-txns _)
            (for/fold ([txns (set (transition '() st-))]
                       [pending-effs rest])
                      ([f-name (in-list children)])
              (define stop-effs (hash-ref (hash-ref txn# f-name) (StopOf f-name)))
              (define stop-effs+ (if (set-empty? stop-effs)
                                     (set '())
                                     stop-effs))
              (define new-txns
                (for*/set ([txn (in-set txns)]
                           [st (in-value (transition-dest txn))]
                           [effs* (in-set stop-effs+)]
                           [next-txn (in-set (loop st (append pending-effs effs*)))])
                  (transition (append (transition-effs txn)
                                      (transition-effs next-txn))
                              (transition-dest next-txn))))
              (values new-txns '())))
          final-txns])])))

(module+ test
  (test-case
      "bug in apply-effects"
    ;; was dropping everything after the first send or realize effect
    (define txns (apply-effects (list (realize Int) (realize String))
                                (set)
                                (facet-tree (hash) (hash))
                                (hash)))
    (check-equal? txns (set (transition (list (realize Int) (realize String)) (set)))))
  (test-case
      "another bug in apply-effects"
    ;; was duplicating some effects
    (define r #s(Role
                 run-a-round342
                 (#s(Shares
                     #s(Struct
                        RoundT
                        (#s(Base Symbol) #s(Base String) #s(List #s(Base String)))))
                  #s(Reacts
                     Start
                     #s(Role
                        wait364
                        (#s(Reacts
                            #s(Asserted #s(Struct LaterThanT (#s(Base Int))))
                            #s(Branch
                               ((#s(Branch
                                    ((#s(Stop
                                         run-a-round342
                                         (#s(Role
                                             over356
                                             (#s(Shares
                                                 #s(Struct
                                                    ElectedT
                                                    (#s(Base String)
                                                     #s(Base String)))))))))
                                     (#s(Stop
                                         run-a-round342
                                         (#s(Realizes
                                             #s(Struct
                                                StartRoundT
                                                (#s(Set #s(Base String))
                                                 #s(Set #s(Base String)))))))))))
                                ())))))))))
    (define labeled-role (label-internal-events r))
    (define roles# (describe-roles labeled-role))
    (define ft (make-facet-tree r))
    (define current (set 'wait364 'run-a-round342))
    (define eff* (list
                  (stop 'run-a-round342)
                  (realize
                   '#s(internal-label
                       initial31336
                       #s(Struct
                          StartRoundT
                          (#s(Set #s(Base String)) #s(Set #s(Base String))))))))
    (check-equal? (apply-effects eff* current ft roles#)
                  (set (transition
                        (list
                         (realize
                          '#s(internal-label
                              initial31336
                              #s(Struct StartRoundT (#s(Set #s(Base String)) #s(Set #s(Base String)))))))
                        (set))))))

;; FacetTree FacetName (Setof FacetName) -> (List FacetName)
;; return the facets in names that are children of the given facet nm, ordered
;; by their distance (farthest children first etc.)
(define (find-children ft nm names)
  (define relations
    (for*/list ([n (in-set names)]
                [ans? (in-value (ancestor?/dist n nm ft))]
                #:when ans?)
      (list n ans?)))
  (define farthest-to-nearest (sort relations > #:key second))
  (map first farthest-to-nearest))

;; Role -> (Hashof FacetName TransitionDesc)
;; Extract a description of all roles mentioned in a Role
(define (describe-roles role)
  (define all-roles (enumerate-roles role))
  (for/hash ([r (in-list all-roles)])
    (define txn (describe-role r))
    (values (Role-nm r)
            txn)))

;; T -> (Listof Role)
;; Find all nested role descriptions
(define (enumerate-roles t)
  (match t
    [(Role _ eps)
     (define rs
       (for*/list ([ep (in-list eps)]
                   #:when (Reacts? ep)
                   [body (in-value (Reacts-body ep))]
                   [act (in-list (Body->actions body))]
                   [role (in-list (enumerate-roles act))])
         role))
     (cons t rs)]
    [(Stop _ body)
     (for*/list ([act (in-list (Body->actions body))]
                 [role (in-list (enumerate-roles act))])
       role)]
    [(or (Sends _)
         (Realizes _))
     '()]
    [(Spawn r)
     (enumerate-roles r)]))

;; Role -> TransitionDesc
;; determine how the event handlers in a role alter the facet tree
(define (describe-role role)
  (match role
    [(Role nm eps)
     (for/fold ([txns (txn-desc0 nm)])
               ([ep (in-list eps)]
                #:when (Reacts? ep))
       (match-define (Reacts evt body) ep)
       (define effects (Body->effects body))
       (when (equal? StopEvt evt)
         ;; facets that start inside a stop handler will get shutdown.
         (define effects+
           (for/set ([effs* (in-set effects)])
             (define extra-stops
               (for/list ([eff (in-list effs*)]
                        #:when (start? eff))
                 (stop (start-nm eff))))
             (append effs* extra-stops)))
         (set! effects effects+))
       (cond
         [(or (set-empty? effects)
              (equal? effects (set '())))
          txns]
         [else
          (define evt+
            (match evt
              [(== StartEvt)
               (StartOf nm)]
              [(== StopEvt)
               (StopOf nm)]
              [_
               evt]))
          (define (update-effect-set existing)
            (combine-effect-sets effects existing))
          (hash-update txns evt+ update-effect-set (set))]))]))

;; (Setof (Listof X)) (Setof (Listof X)) -> (Setof (Listof X))
;; two separately analyzed sets of effects may combine in any way
(define (combine-effect-sets s1 s2)
  (cond
    [(set-empty? s1)
     s2]
    [(set-empty? s2)
     s1]
    [else
     (for*/set ([e1* (in-set s1)]
                [e2* (in-set s2)])
       (append e1* e2*))]))

(module+ test
  (test-case
      "describe simple role"
    (define desc (describe-roles manager))
    (check-true (hash-has-key? desc 'account-manager))
    (check-equal? (hash-ref desc 'account-manager)
                  (txn-desc0 'account-manager)))
  (test-case
      "describe nested role"
    (define desc (describe-roles seller))
    (check-true (hash-has-key? desc 'seller))
    (check-true (hash-has-key? desc 'fulfill))
    (check-equal? (hash-ref desc 'fulfill)
                  (txn-desc0 'fulfill))
    (define seller-txns (hash-ref desc 'seller))
    (define quote-request
      (Observe (book-quote String ⋆)))
    (check-true (hash-has-key? seller-txns (Asserted quote-request)))
    (check-equal? (hash-ref seller-txns (Asserted quote-request))
                  (set (list (start 'fulfill)))))
  (test-case
      "describe-roles bug"
    (define role (Role 'poll
                       (list
                        (Reacts (Asserted Int)
                                (Branch
                                 (list (Stop 'leader (Role 'announce (list (Shares Int))))
                                       (Stop 'poll (list))))))))
    (define desc (describe-roles role))
    (check-true (hash? desc))
    (check-true (hash-has-key? desc 'poll))
    (define txns (hash-ref desc 'poll))
    (check-true (hash-has-key? txns (Asserted Int)))
    (check-equal? (hash-ref txns (Asserted Int))
                  (set (list (stop 'leader) (start 'announce))
                       (list (stop 'poll)))))
  (test-case
      "leader-spec announce"
    (define desc (describe-roles leader-spec))
    (check-true (hash-has-key? desc 'announce))
    (check-equal? (hash-ref desc 'announce)
                  (txn-desc0 'announce)))
  (test-case
      "leader-spec transitions from {leader,poll} to {leader}"
    (define desc (describe-roles leader-spec))
    (check-true (hash-has-key? desc 'poll))
    (define poll-txns (hash-ref desc 'poll))
    (define evt (Asserted (book-interest String String Bool)))
    (check-true (hash-has-key? poll-txns evt))
    (define effs (hash-ref poll-txns evt))
    (check-true (set-member? effs (list (stop 'poll))))))

;; Body -> (Setof (Listof RoleEffect))
(define (Body->effects body)
  (match body
    ['()
     (set)]
    [(cons b more)
     (define fst (Body->effects b))
     (define later (Body->effects more))
     (cond
       [(set-empty? fst)
        later]
       [(set-empty? later)
        fst]
       [else
        (for*/set ([f (in-set fst)]
                   [l (in-set later)])
          (append f l))])]
    [(Branch (list b ...))
     (for/fold ([agg (set)])
               ([b (in-list b)])
       (define effs (Body->effects b))
       ;; it's important to remember when "do nothing" is one of the alternatives of a branch
       (define effs++
         (if (set-empty? effs)
             (set '())
             effs))
       (set-union agg effs++))]
    [(Role nm _)
     (set (list (start nm)))]
    [(Sends τ)
     (set (list (send τ)))]
    [(Realizes τ)
     (set (list (realize τ)))]
    [(Stop nm more)
     (define effects (Body->effects more))
     (cond
       [(set-empty? effects)
        (set (list (stop nm)))]
       [else
        (for/set ([eff* (in-set effects)])
          (cons (stop nm) eff*))])]
    [(Spawn r)
     (set (list (start (Role-nm r))))]))

(module+ test
  (test-case
      "Body->effects"
    (check-equal? (Body->effects '())
                  (set))
    (check-equal? (Body->effects (Branch '()))
                  (set))
    (check-equal? (Body->effects manager)
                  (set (list (start 'account-manager))))
    (check-equal? (Body->effects (list manager))
                  (set (list (start 'account-manager))))
    (check-equal? (Body->effects (Branch (list manager)))
                  (set (list (start 'account-manager))))
    (check-equal? (Body->effects (list manager client))
                  (set (list (start 'account-manager)
                             (start 'client))))
    (check-equal? (Body->effects (Branch (list manager client)))
                  (set (list (start 'account-manager))
                       (list (start 'client))))
    (check-equal? (Body->effects (list manager
                                       (Branch (list client seller))))
                  (set (list (start 'account-manager) (start 'client))
                       (list (start 'account-manager) (start 'seller)))))
  (test-case
      "Body->effects bug?"
    (define body (Branch
                  (list (Stop 'leader (Role 'announce (list (Shares Int))))
                        (Stop 'poll (list)))))
    (check-equal? (Body->effects body)
                  (set (list (stop 'leader) (start 'announce))
                       (list (stop 'poll))))))

;; Role -> Role
;; label internal events & handlers with actor-unique identifiers
(define (label-internal-events T)
  (let walk ([subj T] ;; T or EP or Body or D+
             [current-actor (gensym 'initial)])
    (define (map-walk s) (walk s current-actor))
    (match subj
      [(Role nm eps)
       (Role nm (map map-walk eps))]
      [(Spawn r)
       (define new-actor-id (gensym (Role-nm r)))
       (Spawn (walk r new-actor-id))]
      [(Realizes ty)
       (Realizes (internal-label current-actor ty))]
      [(Stop nm body)
       (Stop nm (walk body current-actor))]
      [(Reacts D body)
       (define D+ (walk D current-actor))
       (Reacts D+ (walk body current-actor))]
      [(Know ty)
       (Know (internal-label current-actor ty))]
      [(? cons?)
       (map map-walk subj)]
      [(Branch bodies)
       (Branch (map map-walk bodies))]
      [(Forget ty)
       (Forget (internal-label current-actor ty))]
      [(Realize ty)
       (Realize (internal-label current-actor ty))]
      [_
       subj])))

(module+ test
  (test-case "label internal events"
    (define role
      (Role 'start
            (list
             (Know Int)
             (Reacts StartEvt
                     (Spawn (Role 'spawned
                                  (list (Reacts (Know Int)
                                                (Role 'know (list))))))))))
    (define role+ (run/timeout (thunk (label-internal-events role))))
    (check-true (Role? role+))
    (match role+
      [(Role _
            (list
             (Know (internal-label label1 Int))
             (Reacts _
                     (Spawn (Role _
                                  (list (Reacts (Know (internal-label label2 Int))
                                                (Role _ (list)))))))))
       (check-not-equal? label1 label2)]
      [_
       (fail "labelled role didn't match expected structure")])))

;; ---------------------------------------------------------------------------
;; "Simulation"

;; τ τ -> Bool
;; subtyping on basic types
(define (<:? τ1 τ2)
  (cond
    [(eq? τ1 τ2)
     #t]
    [else
     (match (list τ1 τ2)
       [(list _ (== ⋆))
        #t]
       [(list (Base t1) (Base t2))
        (equal? t1 t2)]
       [(list (internal-label l1 t1) (internal-label l2 t2))
        (and (equal? l1 l2)
             (<:? t1 t2))]
       [(list (U τs) _)
        (for/and ([τ (in-list τs)])
          (<:? τ τ2))]
       [(list _ (U τs))
        (for/or ([τ (in-list τs)])
          (<:? τ1 τ))]
       [(list (Observe τ11) (Observe τ22))
        (<:? τ11 τ22)]
       [(list (List τ11) (List τ22))
        (<:? τ11 τ22)]
       [(list (Set τ11) (Set τ22))
        (<:? τ11 τ22)]
       [(list (Hash τk1 τv1) (Hash τk2 τv2))
        (and (<:? τk1 τk2)
             (<:? τv1 τv2))]
       [(list (Struct nm1 τs1) (Struct nm2 τs2))
        (and (equal? nm1 nm2)
             (= (length τs1) (length τs2))
             (for/and ([τ11 (in-list τs1)]
                       [τ22 (in-list τs2)])
               (<:? τ11 τ22)))]
       [_
        #f])]))

;; D D -> Bool
;; subtyping lifted over event descriptions
(define (D<:? D1 D2)
  (match (list D1 D2)
    [(list _ (== DataflowEvt))
     ;; TODO - sketchy, intuition "dataflow can happen at any time", though it
     ;; might actually take the place of multiple transitions
     #t]
    [(list (Asserted τ1) (Asserted τ2))
     (<:? τ1 τ2)]
    [(list (Retracted τ1) (Retracted τ2))
     (<:? τ1 τ2)]
    [(list (Message τ1) (Message τ2))
     (<:? τ1 τ2)]
    [(list (Know τ1) (Know τ2))
     (<:? τ1 τ2)]
    [(list (Forget τ1) (Forget τ2))
     (<:? τ1 τ2)]
    [(list (Realize τ1) (Realize τ2))
     (<:? τ1 τ2)]
    [(list (StartOf fn1) (StartOf fn2))
     (equal? fn1 fn2)]
    [(list (StopOf fn1) (StopOf fn2))
     (equal? fn1 fn2)]
    [(list (== StartEvt) (== StartEvt))
     #t]
    [(list (== StopEvt) (== StopEvt))
     #t]
    [_
     #f]))

;; TransitionEffect TransitionEffect -> Bool
;; subtyping lifted over transition effects
(define (eff<:? e1 e2)
  (match (list e1 e2)
    [(list (send t1) (send t2))
     (<:? t1 t2)]
    [(list (realize t1) (realize t2))
     (<:? t1 t2)]
    [_
     #f]))

;; Role -> (Setof τ)
;; Compute the set of assertions the role contributes (on its own, not
;; considering parent assertions)
(define (role-assertions r)
  (for*/set ([ep (in-list (Role-eps r))]
             [τ? (in-value (EP-assertion ep))]
             #:when τ?)
    τ?))

;; EP -> (U #f τ)
;; the type of assertion and endpoint contributes, otherwise #f for
;; dataflow/start/stop
(define (EP-assertion EP)
  (match EP
    [(Shares τ)
     τ]
    [(Know τ)
     τ]
    [(Reacts D _)
     (match D
       [(or (Asserted τ)
            (Retracted τ)
            (Message τ))
        ;; TODO - this doesn't put ⋆ in where an underlying pattern uses a capture
        (Observe τ)]
       [(or (Know (internal-label id τ))
            (Forget (internal-label id τ))
            (Realize (internal-label id τ)))
        (internal-label id (Observe τ))]
       [_
        #f])]
    [_ #f]))

(module+ test
  (test-case
      "EP-assertion sanity"
  ;; make sure the or pattern above works the way I think it does
  (check-equal? (EP-assertion (Reacts (Asserted Int) #f))
                (Observe Int))
  (check-equal? (EP-assertion (Reacts (Retracted String) #f))
                (Observe String)))
  (test-case "EP-assertion/internal regression"
    (check-equal? (EP-assertion (Reacts (Know (internal-label 'x Int)) '()))
                  (internal-label 'x (Observe Int)))))

;; an Equation is (equiv StateName StateName)
;; INVARIANT: lhs is "implementation", rhs is "specification"
;;
;; a Goal is one of
;;   - Equation
;;   - (one-of (Setof StateMatch))
;;
;; a StateMatch is a (Setof (equiv Transition Transition))
(struct equiv (a b) #:transparent)
(struct one-of (opts) #:transparent)

;; (Setof Transition) (Setof Transition) -> (Setof StateMatch)
;; Create potential edge matchings
;; In each state matching, each element a of the first set (as) is
;; matched with an element b of bs, where each b has at least one state
;; matched with it.
(define (make-combinations as bs)
  (define (all-as? xs)
    (for/and ([a (in-set as)])
      (for/or ([x (in-list xs)])
        (match-define (equiv xa _) x)
        (equal? a xa))))
  (define (all-bs? xs)
    (for/and ([b (in-set bs)])
      (for/or ([x (in-list xs)])
        (match-define (equiv _ xb) x)
        (equal? b xb))))
  (define all-matches
    (for*/list ([a (in-set as)]
                [b (in-set bs)])
      (equiv a b)))
  (define combo-size (max (set-count as) (set-count bs)))
  (for/set ([l-o-m (in-combinations all-matches combo-size)]
            #:when (all-as? l-o-m)
            #:when (all-bs? l-o-m))
    (list->set l-o-m)))

(module+ test
  (test-case
      "potential combinations bug"
    ;; confirmed bug
    (define dests1 (set (set 'A)))
    (define dests2 (set (set 'B) (set 'C)))
    (check-equal? (make-combinations dests1 dests2)
                  (set (set (equiv (set 'A) (set 'B))
                            (equiv (set 'A) (set 'C))))))
  (test-case
      "potential combinations bug"
    (define dests1 (set (set 'B) (set 'C)))
    (define dests2 (set (set 'A)))
    (check-equal? (make-combinations dests1 dests2)
                  (set (set (equiv (set 'B) (set 'A))
                            (equiv (set 'C) (set 'A))))))
  (test-case
      "another combinations bug"
    ;; returning matches with 3 elements
    (define dests1 (set (set 'A) (set 'L)))
    (define dests2  (set (set 'A) (set 'L)))
    (check-equal? (make-combinations dests1 dests2)
                  (set
                   (set (equiv (set 'L) (set 'A)) (equiv (set 'A) (set 'L)))
                   (set (equiv (set 'L) (set 'L)) (equiv (set 'A) (set 'A)))))))

;; Role Role -> Bool
;; determine if the first role acts suitably like the second role.
;; at all times, it is asserting a superset of the second's assertions
;; role1 ~ actual
;; role2 ~ spec
(define (simulates? role1 role2)
  (define rg1 (compile role1))
  (define rg2 (compile role2))
  (simulates?/rg rg1 rg2))

;; RoleGraph Role RoleGraph Role -> Bool
;; determine if the first role acts suitably like the second role.
;; at all times, it is asserting a superset of the second's assertions
;; rg1 ~ actual
;; rg2 ~ spec
;; like simulates?, but take in and use the compiled role graph; the role1 and
;; role2 arguments are just for determining the assertions in each state
;; useful when checking subgraphs
(define (simulates?/rg rg1 rg2)
  (match-define (role-graph st0-1 st#1) rg1)
  (match-define (role-graph st0-2 st#2) rg2)

  ;; Goal (Setof Equation) -> Bool
  (define not-equiv (mutable-set))
  (define (verify goal assumptions)
    (let/ec esc
      (define (return ans)
        (when (and (equiv? goal)
                   (not ans))
          (set-add! not-equiv goal))
        (esc ans))
      (match goal
        [(equiv sn1 sn2)
         (when (set-member? assumptions goal)
           (return #t))
         (when (set-member? not-equiv goal)
           (esc #f))
         (match-define (state _ transitions1 assertions1) (hash-ref st#1 sn1))
         (match-define (state _ transitions2 assertions2) (hash-ref st#2 sn2))
         (unless (assertion-superset? assertions1 assertions2)
           (return #f))
         (define (verify/with-current-assumed g)
           (verify g (set-add assumptions goal)))
         (unless (same-on-specified-events? transitions1
                                            transitions2
                                            sn1
                                            verify/with-current-assumed)
           (return #f))
         (return (same-on-extra-events? transitions1
                                        transitions2
                                        sn2
                                        verify/with-current-assumed))]
        [(one-of matchings)
         (for/or ([matching (in-set matchings)])
           (define matching-hypos
             (for/set ([eq (in-set matching)])
               (match-define (equiv t1 t2) eq)
               (equiv (transition-dest t1) (transition-dest t2))))
           (for/and ([goal (in-set matching)])
             (match-define (equiv (transition effs1 dest1)
                                  (transition effs2 dest2)) goal)
             (cond
               [(effects-subsequence? effs2 effs1)
                (define local-goal (equiv dest1 dest2))
                (define hypotheses
                  (set-remove matching-hypos local-goal))
                (verify local-goal (set-union hypotheses assumptions))]
               [else
                #f])
             ))])))
  (verify (equiv st0-1 st0-2) (set)))

;; Role Role -> Bool
(define (simulates?/report-error impl spec)
  (define impl-rg (compile/internal-events (compile impl)))
  (define spec-rg (compile/internal-events (compile spec)))
  (cond
    [(detected-cycle? impl-rg)
     (printf "Detected Cycle in Implementation!\n")
     (describe-detected-cycle impl-rg)
     #f]
    [(detected-cycle? spec-rg)
     (printf "Detected Cycle in Specification!\n")
     (describe-detected-cycle spec-rg)
     #f]
    [(simulates?/rg impl-rg spec-rg)
     #t]
    [else
     (define trace (find-simulation-counterexample impl-rg spec-rg))
     (print-failing-trace trace impl-rg spec-rg)
     #f]))

;; DetectedCycle -> Void
(define (describe-detected-cycle dc)
  (printf "Initial State: ~a\n" (detected-cycle-start dc))
  (for ([step (in-list (detected-cycle-steps dc))])
    (printf "  :: ~a ==> ~a\n" (D->label (traversal-step-evt step)) (traversal-step-dest step))))

;; a FailingTrace is a (failing-trace (Listof Transition) (Listof Transition) (Listof TraceStep))
(struct failing-trace (impl-path spec-path steps) #:transparent)

;; a TraceStep is one of
;; - (both-step D)
;; - (impl-step D)
;; - (spec-step D)
;; describing either both the spec and the implementation responding to an
;; event, only the implementation, or only the spec
(struct both-step (evt) #:transparent)
(struct impl-step (evt) #:transparent)
(struct spec-step (evt) #:transparent)

;; FailingTrace RoleGraph RoleGraph -> Void
(define (print-failing-trace trace impl-rg spec-rg)
  (match-define (role-graph _ impl-st#) impl-rg)
  (match-define (role-graph _ spec-st#) spec-rg)
  (match-define (failing-trace impl-path spec-path steps) trace)
  (define SEP (make-string 40 #\;))
  (define (print-sep)
    (newline)
    (displayln SEP)
    (newline))
  (let loop ([steps steps]
             [impl-path impl-path]
             [spec-path spec-path]
             ;; because the path might end with an impl-step or spec-step, remember the last
             ;; states we've seen so we can print its assertions at the right time
             [last-spec-state (transition-dest (car spec-path))]
             [last-impl-state (transition-dest (car impl-path))])
    (define (get-spec-dest)
      (transition-dest (car spec-path)))
    (define (get-impl-dest)
      (transition-dest (car impl-path)))
    (match steps
      [(cons step more-steps)
       (print-sep)
       (printf "In response to event:\n")
       (match step
         [(or (both-step D)
              (impl-step D)
              (spec-step D))
          (pretty-print D)])
       (when (or (both-step? step) (impl-step? step))
         (define impl-effs (transition-effs (car impl-path)))
         (printf "Implementation steps to state:\n")
         (pretty-print (get-impl-dest))
         (unless (empty? impl-effs)
           (printf "With Effects:\n")
           (pretty-print impl-effs)))
       (when (empty? more-steps)
         (define impl-final (if (spec-step? step) last-impl-state (get-impl-dest)))
         (printf "Implementation Assertions:\n")
         (pretty-print (state-assertions (hash-ref impl-st# impl-final))))
       (when (or (both-step? step) (spec-step? step))
         (define spec-effs (transition-effs (car spec-path)))
         (printf "Specification steps to state:\n")
         (pretty-print (get-spec-dest))
         (unless (empty? spec-effs)
           (printf "With Effects:\n")
           (pretty-print spec-effs)))
       (when (empty? more-steps)
         (define spec-final (if (impl-step? step) last-spec-state (get-spec-dest)))
         (printf "Specification Assertions:\n")
         (pretty-print (state-assertions (hash-ref spec-st# spec-final))))
       (loop more-steps
             (if (spec-step? step) impl-path (cdr impl-path))
             (if (impl-step? step) spec-path (cdr spec-path))
             (if (impl-step? step) last-spec-state (get-spec-dest))
             (if (spec-step? step) last-impl-state (get-impl-dest)))]
      [_
       (newline)
       (void)])))

;; RoleGraph RoleGraph -> Trace
;; assuming impl-rg does not simulate spec-rg, find a trace of transitions
;; (event + effects + destination assertions) demonstrating different behaviors
(define (find-simulation-counterexample impl-rg spec-rg)
  (match-define (role-graph impl-st0 impl-st#) impl-rg)
  (match-define (role-graph spec-st0 spec-st#) spec-rg)
  ;; inside loop, the each trace field is in reverse
  (let loop ([work (list (failing-trace (list (transition '() impl-st0))
                                        (list (transition '() spec-st0))
                                        (list (both-step StartEvt))))]
             #;[visited (set)])
    (match work
      [(cons (failing-trace impl-path/rev spec-path/rev steps/rev) more-work)
       (match-define (transition impl-effs impl-dest) (car impl-path/rev))
       (match-define (transition spec-effs spec-dest) (car spec-path/rev))
       (define last-step (car steps/rev))
       (cond
         [(or (impl-step? last-step)
              ;; when only the implementation steps, no need to compare effects on transitions
              (and (spec-step? last-step) (empty? spec-effs))
              (effects-subsequence? spec-effs impl-effs))
          ;; cascading conds will help with development and isolating where things go wrong
          (match-define (state _ impl-transition# impl-assertions) (hash-ref impl-st# impl-dest))
          (match-define (state _ spec-transition# spec-assertions) (hash-ref spec-st# spec-dest))
          (cond
            ;; n.b. internal events should be compiled away by now or this wouldn't work
            [(assertion-superset? impl-assertions spec-assertions)
             ;; same effects and same assertions, compare transitions
             ;; TODO: similarity to `same-on-specified-events?`
             (define spec-matching-txns
               (for*/list ([(spec-D spec-txns) (in-hash spec-transition#)]
                           [(impl-D impl-txns) (in-hash impl-transition#)]
                           #:when (D<:? spec-D impl-D)
                           [spec-txn (in-set spec-txns)]
                           [impl-txn (in-set impl-txns)])
                 (failing-trace (cons impl-txn impl-path/rev)
                                (cons spec-txn spec-path/rev)
                                (cons (both-step spec-D) steps/rev))))
             ;; transitions that the spec has but the implementation doesn't respond to
             ;; TODO: similarity to `same-on-extra-events?`
             (define impl-evts (hash-keys impl-transition#))
             (define spec-extra-txns
               (for*/list ([(spec-D spec-txns) (in-hash spec-transition#)]
                           ;; TODO - this more or less assumes that *any* event matching impl-D also matches spec-evt, which I'm not sure is quite right
                           #:unless (for/or ([impl-evt (in-list impl-evts)])
                                      (D<:? impl-evt spec-D))
                           [spec-txn (in-set spec-txns)])
                 (failing-trace impl-path/rev
                                (cons spec-txn spec-path/rev)
                                (cons (spec-step spec-D) steps/rev))))
             ;; TODO: similarity to above code
             ;; transitions that the implementation has that the spec doesn't respond to
             (define spec-evts (hash-keys spec-transition#))
             (define impl-extra-txns
               (for*/list ([(impl-D impl-txns) (in-hash impl-transition#)]
                           ;; TODO - this more or less assumes that *any* event matching impl-D also matches spec-evt, which I'm not sure is quite right
                           #:unless (for/or ([spec-evt (in-list spec-evts)])
                                      (D<:? spec-evt impl-D))
                           [impl-txn (in-set impl-txns)])
                 (failing-trace (cons impl-txn impl-path/rev)
                                spec-path/rev
                                (cons (impl-step impl-D) steps/rev))))
             (loop (append more-work spec-matching-txns spec-extra-txns impl-extra-txns))]
            [else
             ;; states have different assertions
             (failing-trace (reverse impl-path/rev) (reverse spec-path/rev) (reverse steps/rev))])]
         [else
          ;; transitions have different effects
          (failing-trace (reverse impl-path/rev) (reverse spec-path/rev) (reverse steps/rev))])]
      [_
       (error "ran out of work")])))

;; (List Role) -> (Hashof RoleName (Setof τ))
;; map each role's name to the assertions it contributes
(define (all-roles-assertions roles)
  (for/hash ([role (in-list roles)])
    (values (Role-nm role)
            (role-assertions role))))

;; (Setof τ) (Setof τ) -> Bool
;; is as1 a superset of as2?
(define (assertion-superset? as1 as2)
  (for/and ([assertion2 (in-set as2)])
    (for/or ([assertion1 (in-set as1)])
      (<:? assertion2 assertion1))))

;; (Hashof D (Setof Transition))
;; (Hashof D (Setof Transition))
;; (Goal -> Bool) -> Bool
;; Determine if:
;;   for each event D going from sn2,
;;   for each event E, D <: E, going from sn1,
;;   (with the exception of the Dataflow HACK below)
;;   for the set of states X connected to sn2 by D,
;;   for the set of states Y connected to sn1 by E,
;;   it is possible to pair the states of X and Y such that they are in simulation,
;;   as determined by the verify procedure
;;   and the effects on the edge going to Y are a supersequence of the effects
;;   on the edge to Y
;; and:
;; Determine if the events in transitions2 that don't have any match in transitions1, are:
;;   - all effect free
;;   - verify with sn1 matched to each destination
(define (same-on-specified-events? transitions1 transitions2 sn1 verify)
  (for/and ([(D2 edges2) (in-hash transitions2)])
    (define edges1
      (for/fold ([agg (set)])
                ([(D1 txns1) (in-hash transitions1)]
                 #:when (D<:? D2 D1)
                 ;; only consider dataflow events vs. non-dataflow when
                 ;; there is not a dataflow edge in the spec (HACK)
                 #:unless (and (equal? D1 DataflowEvt)
                               (not (equal? D2 DataflowEvt))
                               (hash-has-key? transitions2 D1)))
        (set-union agg txns1)))
    (cond
      [(set-empty? edges1)
       ;; - I think this is right, as long as the current state of the implementation
       ;; matches all states the spec steps to --- unless there are effects on the transition
       (for/and ([txn (in-set edges2)])
         (and (empty? (transition-effs txn))
              (verify (equiv sn1 (transition-dest txn)))))]
      [else
       (define combos (make-combinations edges1 edges2))
       (verify (one-of combos))])))

;; (Listof TransitionEffect) (Listof TransitionEffect) -> Bool
;; determine if actual includes (supertypes of) the effects of spec in the same
;; order
(define (effects-subsequence? spec actual)
    (match spec
      ['()
       #t]
      [(cons eff1 more-spec)
       (match actual
         ['()
          #f]
         [(cons eff2 more-actual)
          (if (eff<:? eff1 eff2)
              (effects-subsequence? more-spec more-actual)
              (effects-subsequence? spec more-actual))])]))

;; (Hashof D (Setof Transition))
;; (Hashof D (Setof Transition))
;; StateName
;; (Goal -> Bool) -> Bool
;; Determine if:
;;   for each event E, going from sn1,
;;   such that for each event D going from sn2, ¬ D <: E,
;;   for the set of states X connected to sn1 by E,
;;   each state in X is equivalent to sn2,
;;   as determined by the verify procedure
(define (same-on-extra-events? transitions1 transitions2 sn2 verify)
  (define evts1 (hash-keys transitions1))
  (define evts2 (hash-keys transitions2))
  (define extra-evts
    (for/set ([evt1 (in-list evts1)]
              #:unless (for/or ([evt2 (in-list evts2)])
                         (D<:? evt2 evt1)))
      evt1))
  (for*/and ([evt (in-set extra-evts)]
             [txn (in-set (hash-ref transitions1 evt))])
    (verify (equiv (transition-dest txn) sn2))))

(module+ test
  (test-case
      "simplest simul"
    (define r (Role 'x (list)))
    (check-true (simulates? r r)))
  (test-case
      "identity simulation"
    (check-true (simulates? manager manager))
    (check-true (simulates? client client))
    (check-true (simulates? seller seller)))
  (test-case
      "simulation isn't vacuous"
    (check-false (simulates? manager client))
    (check-false (simulates? client manager))
    (check-false (simulates? manager seller))
    (check-false (simulates? seller manager))
    (check-false (simulates? client seller))
    (check-false (simulates? seller client)))
  (test-case
      "leader-spec identity simulation"
    (check-true (simulates? leader-spec leader-spec)))
  (test-case
      "things aren't quite right with leader-actual"
    (check-false (simulates? leader-actual leader-spec))
    (check-true (simulates? leader-fixed? leader-spec)))
  (test-case
      "things aren't quite right with leader-revised"
    (check-false (simulates? leader-revised leader-spec)))
  (test-case
      "things aren't quite right with member role"
    (check-false (simulates? member-actual member-spec))
    (define member-actual/revised
      (Role
       'member41
       (list
        (Shares (club-member String))
        (Reacts
         (Asserted (Observe (book-interest String ⋆ ⋆)))
         (Role
          'during-inner42
          (list
           (Shares (book-interest String String Bool))
           (Reacts
            (Retracted (Observe (book-interest String ⋆ ⋆)))
            ;; removed (Stop 'during-inner42 '()) here
            '())))))))
    (check-true (simulates? member-actual/revised member-spec)))
  (test-case
      "things aren't quite right with seller role"
    (check-false (simulates? seller-actual seller))
    (define seller-spec/revised
      (Role 'seller
            ;; change body to a During
            (list
             (During (Observe (book-quote String ⋆))
                     (list (Shares (book-quote String Int)))))))
    (check-true (simulates? seller-actual seller-spec/revised))))

;; ---------------------------------------------------------------------------
;; SubGraphs

;; Role Role -> (Listof RoleGraph)
;; Find all subgraphs of the implementation role that simulate the spec role
(define (simulating-subgraphs impl spec)
  ;; assume spec doesn't have any internal events
  (define spec-rg (compile spec))
  (define impl-rg (compile/internal-events (compile impl)))
  (define evts (relevant-events spec-rg))
  (for/list ([srg (subgraphs impl-rg evts)]
             #:when (simulates?/rg srg spec-rg))
    srg))

;; Role Role -> (Maybe RoleGraph)
;; try to find any subgraph of the implementation simulating the spec
;; TODO: would be nice to find the largest
(define (find-simulating-subgraph impl spec)
  (define spec-rg (compile spec))
  (define impl-rg (compile/internal-events (compile impl)))
  (find-simulating-subgraph/rg impl-rg spec-rg))

;; RoleGraph RoleGraph -> (Maybe RoleGraph)
(define (find-simulating-subgraph/rg impl-rg spec-rg)
  (define evts (relevant-events spec-rg))
  (for/first ([srg (subgraphs impl-rg evts)]
              #:when (simulates?/rg srg spec-rg))
    srg))

;; Role Role -> Bool
(define (find-simulating-subgraph/report-error impl spec)
  (define spec-rg (compile spec))
  (define impl-rg (compile/internal-events (compile impl)))
  (define ans (find-simulating-subgraph/rg impl-rg spec-rg))
  (cond
    [ans
     #t]
    [else
     (define-values (ft sg) (find-largest-simulating-subgraph-counterexample impl-rg spec-rg))
     (print-failing-trace ft impl-rg spec-rg)
     #f]))

;; RoleGraph RoleGraph -> (Values FailingTrace RoleGraph)
;; assuming impl does not have any simulating subgraphs of spec
;; largest *trace*, not largest subgraph
(define (find-largest-simulating-subgraph-counterexample impl-rg spec-rg)
  (define evts (relevant-events spec-rg))
  (define-values (trace len rg)
    (for/fold ([best-trace #f]
               [best-length 0]
               [best-subgraph #f])
              ([srg (subgraphs impl-rg evts)])
      (define ft (find-simulation-counterexample srg spec-rg))
      (define len (failing-trace-length ft))
      ;; thing >= will prefer larger graphs
      (if (>= len best-length)
          (values ft len srg)
          (values best-trace best-length best-subgraph))))
  (values trace rg))

;; FailingTrace -> Int
(define (failing-trace-length ft)
  (length (failing-trace-steps ft)))

(module+ test
  (test-case
      "task manager has task performer subgraphs"
    (define tpr (parse-T task-performer-spec))
    (define tmr (parse-T task-manager-ty))
    (define ans (simulating-subgraphs tmr tpr))
    (check-equal? (length ans) 340)
    (define tprg (compile tpr))
    (check-true (simulates?/rg (first ans) tprg))
    (check-true (simulates?/rg (second ans) tprg))))

;; RoleGraph (Setof τ) -> (Sequenceof RoleGraph)
;; generate non-empty subgraphs, where at least the given assertions are enabled
(define (subgraphs rg as)
  (match-define (role-graph _ state#) rg)
  ;; (Setof (U τ DataflowEvt))
  (define all-events
    (for*/set ([st (in-hash-values state#)]
               [txn# (in-value (state-transitions st))]
               [D (in-hash-keys txn#)])
    (match D
      ;; TODO - might not make as much sense w/ internal events
      [(or (Asserted τ)
           (Retracted τ)
           (Message τ)
           (Know τ)
           (Forget τ)
           (Realize τ))
       τ]
      [_ D])))
  (in-generator
   (define cache (mutable-set))
   (for* ([states* (in-combinations (hash-keys state#))]
          [events* (in-combinations (set->list all-events))]
          [event-set (in-value (list->set events*))]
          #:when (assertion-superset? (set-remove event-set DataflowEvt) as))
     (define states (list->set states*))
     (define (event-enabled? D)
       ;; TODO - include internal events
       (for/or ([e (in-set event-set)])
         (or (equal? DataflowEvt e)
             (D<:? D (Asserted e))
             (D<:? D (Retracted e)))))
     (define st#
       (for/hash ([st (in-list states*)])
         (match-define (state _ orig-txn# assertions) (hash-ref state# st))
         (define (enabled-txns D)
           (define orig-txns (hash-ref orig-txn# D))
           (for/set ([txn (in-set orig-txns)]
                     #:when (set-member? states (transition-dest txn)))
             txn))
         (define txn#
           (for*/hash ([D (in-hash-keys orig-txn#)]
                       #:when (event-enabled? D)
                       [new-txns (in-value (enabled-txns D))]
                       #:unless (set-empty? new-txns))
             (values D new-txns)))
         (values st (state st txn# assertions))))
     (for ([st0 (in-list states*)])
       (define rg (role-graph st0 st#))
       (unless (set-member? cache rg)
         (define reachable (reachable-states rg))
         (define all-inc?
           (for/and ([st (in-set states)])
             (set-member? reachable st)))
         (when all-inc?
           (yield rg))
         (set-add! cache rg))))))

;; RoleGraph -> (Setof StateName)
;; Determine the set of states reachable from the starting state
(define (reachable-states rg)
  (match-define (role-graph st0 state#) rg)
  (let search ([work (list st0)]
               [seen (set)])
    (match work
      ['() seen]
      [(cons current more)
       (match-define (state name txn# _) (hash-ref state# current))
       (cond
         [(set-member? seen name)
          (search more seen)]
         [else
          (define connections
            (for*/list ([txn* (in-hash-values txn#)]
                        [txn (in-set txn*)])
              (transition-dest txn)))
          (search (append more connections)
                  (set-add seen name))])])))

(module+ test
  (test-case
      "reachable states"
    (define rg
      (role-graph (set 'X 'Y 'Z)
                  (hash (set 'X 'Y 'Z) (state (set 'X 'Y 'Z)
                                              (hash (Asserted Int) (set (transition '() (set 'X 'Y 'Z)))
                                                    (Retracted Int) (set (transition '() (set 'X 'Y))))
                                              (set))
                        (set 'X) (state (set 'X) '#hash() (set))
                        (set 'X 'Y) (state (set 'X 'Y)
                                           (hash (Asserted Int) (set (transition '() (set 'X 'Y 'Z))))
                                           (set)))))
      (define reachable (reachable-states rg))
      (check-true (set-member? reachable (set 'X 'Y 'Z)))
      (check-true (set-member? reachable (set 'X 'Y)))
      (check-false (set-member? reachable (set 'X))))
  (test-case
      "struct seems to make a difference?"
    (define rg
      (role-graph
       (set 'during-inner2 'during-inner1 'tm)
       (hash
        (set 'during-inner2 'during-inner1 'tm)
        (state
         (set 'during-inner2 'during-inner1 'tm)
         (hash
          (Asserted (Struct 'TaskAssignment (list)))
          (set (transition '() (set 'during-inner2 'during-inner1 'tm)))
          (Retracted (Struct 'TaskAssignment (list)))
          (set (transition '() (set 'during-inner1 'tm))))
         (set))
        (set 'tm)
        (state (set 'tm) '#hash() (set))
        (set 'during-inner1 'tm)
        (state
         (set 'during-inner1 'tm)
         (hash
          (Asserted (Struct 'TaskAssignment (list)))
          (set (transition '() (set 'during-inner2 'during-inner1 'tm))))
         (set)))))
    (define reachable (reachable-states rg))
    (check-true (set-member? reachable (set 'during-inner2 'during-inner1 'tm)))
    (check-true (set-member? reachable (set 'during-inner1 'tm)))
    (check-false (set-member? reachable (set 'tm)))))

;; RoleGraph -> (Setof (U τ DataflowEvt))
;; extract the assertions that cause transitions, and dataflow events if they
;; occur
(define (relevant-events rg)
  (match-define (role-graph _ state#) rg)
  (for*/set ([st (in-hash-values state#)]
             [txn# (in-value (state-transitions st))]
             [D (in-hash-keys txn#)]
             #:when (external-evt? D))
    (match D
      [(or (Asserted τ) (Retracted τ))
       τ]
      [_ D])))

;; ---------------------------------------------------------------------------
;; Visualization

;; TODO - for now, assume there are no names that need escaping

;; RoleGraph -> DotString
;; name is an optional string
;; translate the states to DOT that can be passed to graphviz
(define (render rg
                #:name [name #f])
  (match-define (role-graph st0 st#) rg)
  (define graph-name (or name "Roles"))
  (define entry-node (format "~a [style=bold];" (state-name->dot-name st0)))
  (define edges
    (for/list ([(sn st) (in-hash st#)])
      (define dot-name (state-name->dot-name sn))
      (define txns (state-transitions st))
      (define dot-edges
        (for*/list ([(D targets) (in-hash txns)]
                    [target (in-set targets)])
          (render-edge dot-name D target)))
      (string-join dot-edges "\n")))
  (string-join (cons entry-node edges)
               "\n"
               #:before-first (format "digraph ~a {\n" graph-name)
               #:after-last "\n}"))

;; RoleGraph PathString -> DotString
;; Like render but write the output to a file
(define (render-to-file rg dest
                        #:name [name #f])
  (with-output-to-file dest
    (lambda () (write-string (render rg #:name name)))
    #:exists 'replace))

;; StateName -> String
(define (state-name->dot-name sn)
  (define nms
    (for/list ([nm (in-set sn)])
      (~a nm)))
  (string-join nms ","
               #:before-first "\"{"
               #:after-last "}\""))

;; String D Transition -> DotString
;; describe an edge between the states with the corresponding label
(define (render-edge from evt txn)
  (match-define (transition effs to) txn)
  (define target-dot (state-name->dot-name to))
  (define evt-label (D->label evt))
  (define edge-label
    ;; TODO - better presentation of effects
    (if (empty? effs)
        evt-label
        (string-append evt-label "[...]")))
  (format "~a -> ~a [label=\"~a\"];" from target-dot edge-label))

;; D -> DotString
;; give a description of an event suitable for rendering
(define (D->label evt)
  (match evt
    [(Asserted τ)
     (string-append "+" (τ->string τ))]
    [(Retracted τ)
     (string-append "-" (τ->string τ))]
    [(Message τ)
     (string-append "!" (τ->string τ))]
    [(Know τ)
     (string-append "~+" (τ->string τ))]
    [(Forget τ)
     (string-append "~-" (τ->string τ))]
    [(Realize τ)
     (string-append "~!" (τ->string τ))]
    [(StartOf fn)
     (format "(Started ~a)" fn)]
    [(StopOf fn)
     (format "(Stopped ~a)" fn)]
    [(== StartEvt)
     "Start"]
    [(== StopEvt)
     "Stop"]
    [(== DataflowEvt)
     "Dataflow"]))

;; τ -> String
(define (τ->string τ)
  ;; (Listof String) -> String
  (define (paren-join xs)
    (string-join xs
                 #:before-first "("
                 #:after-last ")"))
  (match τ
    [(Base name)
     (symbol->string name)]
    [(== ⋆) "⋆"]
    [(Observe τ2)
     (string-append "?" (τ->string τ2))]
    [(List τ2)
     (τ->string (Struct 'List (list τ2)))]
    [(Set τ2)
     (τ->string (Struct 'Set (list τ2)))]
    [(Hash τk τv)
     (τ->string (Struct 'Hash (list τk τv)))]
    [(Struct nm τs)
     (define slots (map τ->string τs))
     (paren-join (cons (~a nm) slots))]
    [(U τs)
     (define slots (map τ->string τs))
     (paren-join (cons "U" slots))]
    [(internal-label _ ty)
     (τ->string ty)]))

;; ---------------------------------------------------------------------------
;; Converting types from the turnstile implementation

;; QuotedType -> T
(define (parse-T ty)
  (match ty
    [(list 'Role (list name) eps ...)
     (define parsed-eps (map parse-EP eps))
     (Role name parsed-eps)]
    [(list 'Spawn t)
     (Spawn (parse-T t))]
    [(list 'Sends t)
     (Sends (parse-τ t))]
    [(list 'Realizes t)
     (Realizes (parse-τ t))]
    [(list 'Stop name body ...)
     (define bdy (cond [(empty? body) body]
                       [(= (length body) 1) (first body)]
                       [else (cons 'Effs body)]))
     (Stop name (parse-Body bdy))]
    ))

;; Sexp -> EP
(define (parse-EP ep)
  (match ep
    [(list 'Shares ty)
     (define parsed-ty (parse-τ ty))
     (Shares parsed-ty)]
    [(list 'Know ty)
     (define parsed-ty (parse-τ ty))
     (Know parsed-ty)]
    [(list 'Reacts D b ...)
     (define bdy (if (= (length b) 1)
                     (first b)
                     (cons 'Effs b)))
     (Reacts (parse-D D) (parse-Body bdy))]))

(define (parse-Body b)
  (match b
    [(list 'Branch bs ...)
     (Branch (map parse-Body bs))]
    [(list 'Effs bs ...)
     (list (map parse-Body bs))]
    [(list)
     (list)]
    [_
     (parse-T b)]))

(define (parse-D d)
  (match d
    [(list 'Asserted t)
     (Asserted (parse-τ t))]
    [(list 'Retracted t)
     (Retracted (parse-τ t))]
    [(list 'Message t)
     (Message (parse-τ t))]
    [(list 'Know t)
     (Know (parse-τ t))]
    [(list 'Forget t)
     (Forget (parse-τ t))]
    [(list 'Realize t)
     (Realize (parse-τ t))]
    ['OnStart
     StartEvt]
    ['OnStop
     StopEvt]
    ['OnDataflow
     DataflowEvt]))

;; Sexp -> τ
(define (parse-τ ty)
  (match ty
    [(list 'Observe t)
     (Observe (parse-τ t))]
    [(list 'List t)
     (List (parse-τ t))]
    [(list 'Set t)
     (Set (parse-τ t))]
    [(list 'Hash t-k t-v)
     (Hash (parse-τ t-k) (parse-τ t-v))]
    ['★/t
     ⋆]
    [(list (or 'U* 'U) t ...)
     (U (map parse-τ t))]
    [(list 'Bind t)
     ;; TODO : questionable
     ⋆
     #;(parse-τ t)]
    ['Discard
     ⋆]
    [(list struct-name tys ...)
     (Struct struct-name (map parse-τ tys))]
    [(? symbol?)
     (Base ty)])
  )

(module+ test
  (check-equal? (parse-T '(Stop during-inner))
                (Stop 'during-inner (list)))
  (test-case
      "real seller type"
    (check-true (Role? (parse-T real-seller-ty))))
  (test-case
      "Stop with a single continuation effect"
    (check-true (Stop? (parse-T '(Stop poll-members
                                       (Branch (Effs (Stop get-quotes)) (Effs)))))))
  (test-case
      "parsed types are (not) the same as my manual conversions"
    ;; because I parse (Bind τ) as ⋆, whereas my manual conversions use τ thus
    ;; the "real" types are more specialized and implement the manual
    ;; conversions, but not vice versa
    (check-true (simulates? (parse-T real-seller-ty) seller-actual))
    (check-false (simulates? seller-actual (parse-T real-seller-ty)))

    (check-true (simulates? (parse-T real-member-ty) member-actual))
    (check-false (simulates? member-actual (parse-T real-member-ty)))

    (check-true (simulates? (parse-T real-leader-ty) leader-actual))
    (check-false (simulates? leader-actual (parse-T real-leader-ty)))
    (check-true (simulates? (parse-T real-leader-ty) leader-revised))
    (check-false (simulates? leader-revised (parse-T real-leader-ty))))

  (test-case
      "parse a Stop with two actions"
    (define r '(Stop
                assign-manager
                (Role
                 (waiting-for-answer)
                 (Reacts OnStop)
                 )
                (Role
                 (_)
                 (Know
                  (SlotAssignment (ReqID (Tuple Int Symbol) Symbol) Symbol)))))
    (check-true (Stop? (parse-T r)))))

;; --------------------------------------------------------------------------
;; Examples, Book Club

(define manager
  (Role 'account-manager
        (list (Shares (Struct 'account (list Int)))
              (Reacts (Asserted (Struct 'deposit '())) '()))))
(define client
  (Role 'client
        (list (Reacts (Asserted (Struct 'account (list Int))) '()))))

;; τ τ -> τ
;; short hand for creating a book quote struct type
(define (book-quote ty1 ty2)
  (Struct 'BookQuoteT (list ty1 ty2)))

;; τ τ τ -> τ
;; short hand for creating a book quote interest type
(define (book-interest ty1 ty2 ty3)
  (Struct 'BookInterestT (list ty1 ty2 ty3)))

;; τ -> τ
;; short hand for creating a book of the month type
(define (book-of-the-month ty)
  (Struct 'BookOfTheMonthT (list ty)))

;; τ -> τ
;; short hand for creating a club member type
(define (club-member ty)
  (Struct 'ClubMemberT (list ty)))

(define seller
  (Role 'seller
        (list
         (Reacts (Asserted (Observe (book-quote String ⋆)))
                 (Role 'fulfill
                       (list (Shares (book-quote String Int))))))))

(define seller-actual
  (Role
   'seller27
   (list
    (Reacts
     (Asserted (Observe (book-quote String ⋆)))
     (Role
      'during-inner29
      (list
       (Shares (book-quote String (U (list Int Int))))
       (Reacts
        (Retracted (Observe (book-quote String ⋆)))
        (Stop 'during-inner29 '()))))))))

(define leader-spec
  (Role 'leader
        (list
         (Reacts
          (Asserted (book-quote String Int))
          (Role 'poll
                (list
                 (Reacts
                  (Asserted (book-interest String String Bool))
                  (Branch
                   (list
                    (Stop 'leader
                          (Role 'announce
                                (list
                                 (Shares (book-of-the-month String)))))
                    (Stop 'poll (list)))))))))))

(define leader-actual
  (Role
   'get-quotes
   (list
    (Reacts
     (Asserted (book-quote String Int))
     (Branch
      (list
       ;; problem 1: spec doesn't say actor can give up when running out of books
       (Stop 'get-quotes '())
       (Role
        'poll-members
        (list
         (Reacts
          (Asserted (book-interest String String ⋆))
          (Branch (list
                   ;; problem 2: combining poll-members and get-quotes here (should be another branch)
                   (Stop 'poll-members
                         (Stop 'get-quotes '()))
                   (Stop 'get-quotes
                         (Role 'announce
                               (list
                                (Shares (book-of-the-month String))))))))
         (Reacts (Retracted (book-interest String String Bool)) (list))
         (Reacts (Asserted (book-interest String String Bool)) (list))
         (Reacts (Retracted (book-interest String String Bool)) (list))
         (Reacts (Asserted (book-interest String String Bool)) (list)))))))
    (Reacts (Retracted (club-member String)) (list))
    (Reacts (Asserted (club-member String)) (list)))))

(define leader-fixed?
  (Role 'get-quotes
        (list
         (Reacts (Asserted (book-quote String Int))
                 (Branch (list
                          (Role 'poll-members
                                (list
                                 (Reacts (Asserted (book-interest String String ⋆))
                                         (Branch (list
                                                  (Stop 'poll-members
                                                        '())
                                                  (Stop 'get-quotes
                                                        (Role 'announce
                                                              (list
                                                               (Shares (book-of-the-month String))))))))
                                 (Reacts (Retracted (book-interest String String Bool)) (list))
                                 (Reacts (Asserted (book-interest String String Bool)) (list))
                                 (Reacts (Retracted (book-interest String String Bool)) (list))
                                 (Reacts (Asserted (book-interest String String Bool)) (list)))))))
         (Reacts (Retracted (club-member String)) (list))
         (Reacts (Asserted (club-member String)) (list)))))

(define leader-revised
  (Role
   'get-quotes
   (list
    (Reacts
     (Asserted (book-quote String Int))
     (Branch
      (list
       (Branch (list (Stop 'get-quotes (list)) (list)))
       (Role
        'poll-members
        (list
         (Reacts
          (Asserted (book-interest String String ⋆))
          (list
           (Branch
            (list
             (Stop 'poll-members
                   (Branch (list
                            (Stop 'get-quotes (list))
                            (list))))
             (list)))
           (Branch
            (list
             (Stop
              'get-quotes
              (Role 'announce (list (Shares (book-of-the-month String)))))
             (list)))))
         (Reacts (Retracted (book-interest String String Bool)) (list))
         (Reacts (Asserted (book-interest String String Bool)) (list))
         (Reacts (Retracted (book-interest String String Bool)) (list))
         (Reacts (Asserted (book-interest String String Bool)) (list)))))))
    (Reacts (Retracted (club-member String)) (list))
    (Reacts (Asserted (club-member String)) (list)))))

(define member-spec
  (Role
   'member
   (list
    (Shares (club-member String))
    (Reacts (Asserted (Observe (book-interest String ⋆ ⋆)))
            (Role 'respond
                  (list
                   (Shares (book-interest String String Bool))))))))

(define member-actual
  (Role
   'member41
   (list
   (Shares (club-member String))
   (Reacts
    (Asserted (Observe (book-interest String ⋆ ⋆)))
    (Role
     'during-inner42
     (list
     (Shares (book-interest String String Bool))
     (Reacts
      (Retracted (Observe (book-interest String ⋆ ⋆)))
      ;; this bit is a noticeable deviation from the spec
      (Stop 'during-inner42 '()))))))))

(define real-seller-ty
  '(Role
    (seller)
    (Reacts
     (Asserted (Observe (BookQuoteT (Bind String) Discard)))
     (Role
      (during-inner)
      (Shares (BookQuoteT String Int))
      (Reacts
       (Retracted (Observe (BookQuoteT String Discard)))
       (Stop during-inner))))))

(define real-member-ty
  '(Role
    (member)
    (Shares (ClubMemberT String))
    (Reacts
     (Asserted (Observe (BookInterestT (Bind String) Discard Discard)))
     (Role
      (during-inner)
      (Shares (BookInterestT String String Bool))
      (Reacts
       (Retracted (Observe (BookInterestT String Discard Discard)))
       (Stop during-inner))))))

(define real-leader-ty
  '(Role
    (get-quotes)
    (Reacts
     (Asserted (BookQuoteT String (Bind Int)))
     (Branch
      (Effs (Branch (Effs (Stop get-quotes)) (Effs)))
      (Effs
       (Role
        (poll-members)
        (Reacts
         (Asserted (BookInterestT String (Bind String) Discard))
         (Branch
          (Effs (Stop poll-members (Branch (Effs (Stop get-quotes)) (Effs))))
          (Effs))
         (Branch
          (Effs
           (Stop get-quotes (Role (announce) (Shares (BookOfTheMonthT String)))))
          (Effs)))
        (Reacts (Retracted (BookInterestT String (Bind String) Bool)))
        (Reacts (Asserted (BookInterestT String (Bind String) Bool)))
        (Reacts (Retracted (BookInterestT String (Bind String) Bool)))
        (Reacts (Asserted (BookInterestT String (Bind String) Bool)))))))
    (Reacts (Retracted (ClubMemberT (Bind String))))
    (Reacts (Asserted (ClubMemberT (Bind String))))))

;; ---------------------------------------------------------------------------
;; Flink Examples

(define task-assigner-spec
  '(Role
    (assign)
    (Shares
     (Observe
      (TaskPerformance
       Symbol
       (Task
        (Tuple Int Symbol)
        (U (MapWork String) (ReduceWork (Hash String Int) (Hash String Int))))
       ★/t)))
    (Reacts
     (Asserted
      (TaskPerformance
       Symbol
       (Task
        (Tuple Int Symbol)
        (U (MapWork String) (ReduceWork (Hash String Int) (Hash String Int))))
       ★/t))
     (Branch (Stop assign) (Effs)))))

(module+ test
  (test-case "parse and compile task-assigner-spec"
    (check-true (Role? (parse-T task-assigner-spec)))
    (check-true (role-graph? (compile (parse-T task-assigner-spec))))))

(define task-performer-spec
  '(Role
    (listen)
    (Reacts
     (Asserted
      (Observe
       (TaskPerformance
        Symbol
        (Task
         (Tuple Int Symbol)
         (U
          (MapWork String)
          (ReduceWork (Hash String Int) (Hash String Int))))
        ★/t)))
     (Role
      (during-inner)
      (Reacts
       (Retracted
        (Observe
         (TaskPerformance
          Symbol
          (Task
           (Tuple Int Symbol)
           (U
            (MapWork String)
            (ReduceWork (Hash String Int) (Hash String Int))))
          ★/t)))
       (Stop during-inner))
      (Shares
       (TaskPerformance
        Symbol
        (Task
         (Tuple Int Symbol)
         (U
          (MapWork String)
          (ReduceWork (Hash String Int) (Hash String Int))))
        (U (Finished (Hash String Int)) Symbol)))))))

(module+ test
    (test-case "parse and compile task-performer-spec"
      (check-true (Role? (parse-T task-performer-spec)))
      (check-true (role-graph? (compile (parse-T task-performer-spec))))))


(define job-manager-actual
  '(Role
    (jm)
    (Shares (JobManagerAlive))
    (Reacts
     (Asserted
      (Observe
       (JobCompletion
        (Bind Symbol)
        (Bind
         (List
          (Task
           (Tuple Int Symbol)
           (U (MapWork String) (ReduceWork Int Int)))))
        Discard)))
     (Role
      (during-inner)
      (Reacts
       OnStart
       (Realizes
        (TaskIsReady
         Symbol
         (Task
          (Tuple Int Symbol)
          (U
           (MapWork String)
           (ReduceWork (Hash String Int) (Hash String Int))))))
       (Role
        (delegate-tasks)
        (Reacts
         (Realize (TaskIsReady Symbol (Bind (U))))
         (Role
          (perform)
          (Reacts
           OnStart
           (Role
            (select)
            (Reacts
             (Know
              (SlotAssignment (ReqID (Tuple Int Symbol) Symbol) (Bind Symbol)))
             (Role
              (assign)
              (Reacts
               (Asserted
                (TaskPerformance
                 Symbol
                 (Task
                  (Tuple Int Symbol)
                  (U
                   (MapWork String)
                   (ReduceWork (Hash String Int) (Hash String Int))))
                 (Bind (U (Finished (Hash String Int)) Symbol))))
               (Branch
                (Effs)
                (Effs)
                (Effs (Stop assign))
                (Effs
                 (Stop
                  perform
                  (Branch
                   (Effs (Realizes (TasksFinished Symbol (Hash String Int))))
                   (Effs
                    (Branch
                     (Effs
                      (Realizes
                       (TaskIsReady
                        Symbol
                        (Task
                         (Tuple Int Symbol)
                         (U
                          (MapWork String)
                          (ReduceWork (Hash String Int) (Hash String Int)))))))
                     (Effs))))))))
              (Reacts (Retracted (TaskManager Symbol Discard)) (Stop assign))))))
          (Reacts OnStop)
          (Reacts OnStart)))
        (Reacts
         (Realize (TasksFinished Symbol (Bind (Hash String Int))))
         (Stop
          delegate-tasks
          (Role
           (done)
           (Shares
            (JobCompletion
             Symbol
             (List
              (Task
               (Tuple Int Symbol)
               (U (MapWork String) (ReduceWork Int Int))))
             (Hash String Int))))))))
      (Reacts
       (Retracted
        (Observe
         (JobCompletion
          Symbol
          (List
           (Task
            (Tuple Int Symbol)
            (U (MapWork String) (ReduceWork Int Int))))
          Discard)))
       (Stop during-inner))))
    (Reacts
     OnStart
     (Role
      (slot-manager)
      (Know (Slots Int))
      (Reacts
       (Know
        (Observe
         (SlotAssignment
          (ReqID (Bind (Tuple Int Symbol)) (Bind Symbol))
          Discard)))
       (Role
        (during-inner2)
        (Reacts OnStop)
        (Reacts
         OnStart
         (Role
          (assign-manager)
          (Reacts
           (Know (Slots (Bind Int)))
           (Branch
            (Effs)
            (Effs
             (Branch
              (Effs
               (Stop
                assign-manager
                (Role
                 (waiting-for-answer)
                 (Reacts OnStop)
                 (Reacts
                  (Asserted
                   (Observe
                    (TaskPerformance
                     Symbol
                     (Task
                      (Tuple Int Symbol)
                      (Bind
                       (U
                        (MapWork String)
                        (ReduceWork (Hash String Int) (Hash String Int)))))
                     Discard)))
                  (Role
                   (_)
                   (Reacts
                    (Asserted
                     (TaskPerformance
                      Symbol
                      (Task
                       (Tuple Int Symbol)
                       (U
                        (MapWork String)
                        (ReduceWork (Hash String Int) (Hash String Int))))
                      Discard))
                    (Stop waiting-for-answer)))))
                (Role
                 (_)
                 (Know
                  (SlotAssignment (ReqID (Tuple Int Symbol) Symbol) Symbol)))))
              (Effs)))))))
        (Reacts
         (Forget
          (Observe
           (SlotAssignment (ReqID (Tuple Int Symbol) Symbol) Discard)))
         (Stop during-inner2))))
      (Reacts (Retracted (TaskManager (Bind Symbol) (Bind Int))))
      (Reacts (Asserted (TaskManager (Bind Symbol) (Bind Int))))))))

(module+ test
  (test-case
      "job manager reads and compiles"
    (define jmr (run/timeout (thunk (parse-T job-manager-actual))))
    (check-true (Role? jmr))
    (define jm (run/timeout (thunk (compile jmr)) 5000))
    (check-true (role-graph? jm))
    (define jmi (run/timeout (thunk (compile/internal-events jm)) 5000))
    (check-true (role-graph? jmi))
    ;; TODO : times out, probably due to infinite loop
    #;(check-true (run/timeout (thunk (simulates?/rg jmi jmi)) 100000))))

(define task-runner-ty
  '(Role
    (runner)
    (Shares (TaskRunner Symbol))
    (Reacts
     (Asserted
      (Observe
       (TaskPerformance
        Symbol
        (Bind
         (Task
          (Tuple Int Symbol)
          (U
           (MapWork String)
           (ReduceWork (Hash String Int) (Hash String Int)))))
        Discard)))
     (Role
      (during-inner)
      (Shares
       (TaskPerformance
        Symbol
        (Task
         (Tuple Int Symbol)
         (U
          (MapWork String)
          (ReduceWork (Hash String Int) (Hash String Int))))
        (U (Finished (Hash String Int)) Symbol)))
      (Reacts
       (Retracted
        (Observe
         (TaskPerformance
          Symbol
          (Task
           (Tuple Int Symbol)
           (U
            (MapWork String)
            (ReduceWork (Hash String Int) (Hash String Int))))
          Discard)))
       (Stop during-inner))))
    (Reacts (Retracted (TaskManager Symbol Discard)) (Stop runner))))

(module+ test
  (test-case "parse and compile task-runner-ty"
    (check-true (Role? (parse-T task-runner-ty)))
    (check-true (role-graph? (compile (parse-T task-runner-ty)))))
  (test-case "task-runner subgraph(s) simulate task-performer"
    (define tr (parse-T task-runner-ty))
    (define tpr (parse-T task-performer-spec))
    (define ans (run/timeout (thunk (simulating-subgraphs tr tpr))))
    (check-true (list? ans))
    (check-false (empty? ans))))

(define task-manager-ty
  `(Role
    (tm)
    (Reacts
     (Asserted (JobManagerAlive))
     (Role
      (during-inner2)
      (Shares (TaskManager Symbol Int))
      (Reacts
       (Asserted
        (Observe
         (TaskPerformance
          Symbol
          (Bind
           (Task
            (Tuple Int Symbol)
            (U
             (MapWork String)
             (ReduceWork (Hash String Int) (Hash String Int)))))
          Discard)))
       (Role
        (during-inner3)
        (Shares
         (TaskPerformance
          Symbol
          (Task
           (Tuple Int Symbol)
           (U
            (MapWork String)
            (ReduceWork (Hash String Int) (Hash String Int))))
          (U (Finished (Hash String Int)) Symbol)))
        (Reacts
         (Asserted
          (TaskPerformance
           Symbol
           (Task
            (Tuple Int Symbol)
            (U
             (MapWork String)
             (ReduceWork (Hash String Int) (Hash String Int))))
           (Bind (U (Finished (Hash String Int)) Symbol)))))
        (Reacts OnStop)
        (Reacts
         (Retracted
          (Observe
           (TaskPerformance
            Symbol
            (Task
             (Tuple Int Symbol)
             (U
              (MapWork String)
              (ReduceWork (Hash String Int) (Hash String Int))))
            Discard)))
         (Stop during-inner3))))
      (Reacts OnDataflow)
      (Reacts
       OnStart
       (Role
        (monitor-task-runner)
        (Reacts
         (Retracted (TaskRunner Symbol))
         (Spawn ,task-runner-ty))
        (Reacts (Asserted (TaskRunner Symbol)))
        (Reacts
         OnStart
         (Spawn ,task-runner-ty))))
      (Reacts (Retracted (JobManagerAlive)) (Stop during-inner2))))))

(module+ test
  (test-case "parse and compile task-manager-ty"
    (check-true (Role? (parse-T task-manager-ty)))
    (check-true (role-graph? (compile (parse-T task-manager-ty)))))
  (test-case
      "work needs to be done"
    ;; even though the task manager plays both the TaskPerformer and TaskAssigner roles,
    ;; it does so situationally, so shouldn't directly simulate either
    (define tm (parse-T task-manager-ty))
    (check-false (simulates? tm (parse-T task-assigner-spec)))
    (check-false (simulates? tm (parse-T task-performer-spec)))))


#;(module+ test
  (test-case
      "job manager subgraph(s) implement task assigner"
    (define jmr (run/timeout (thunk (parse-T job-manager-actual))))
    (define tar (parse-T task-assigner-spec))
    (define ans (run/timeout (thunk (simulating-subgraphs jmr tar)) 4000))
    (check-true (list? ans))
    (check-false (empty? ans))))

(module+ done-facet-dying-too-soon
  ;; has a bug with done facet dying too soon
  (define job-manager-v2
    '(Role
      (jm)
      (Shares (JobManagerAlive))
      (Reacts
       (Asserted
        (Job
         (Bind Symbol)
         (Bind (List (Task Int (U (MapWork String) (ReduceWork Int Int)))))))
       (Role
        (during-inner)
        (Reacts
         OnStart
         (Role
          (delegate-tasks)
          (Reacts
           OnDataflow
           (Role
            (perform)
            (Reacts
             OnStart
             (Role
              (select)
              (Reacts (Forget (SelectedTM (Bind Symbol))))
              (Reacts
               OnDataflow
               (Branch
                (Effs
                 (Branch
                  (Effs
                   (Role
                    (assign)
                    (Shares
                     (TaskAssignment
                      Symbol
                      Symbol
                      (Task
                       Int
                       (U
                        (MapWork String)
                        (ReduceWork (Hash String Int) (Hash String Int))))))
                    (Know (SelectedTM Symbol))
                    (Reacts
                     (Asserted
                      (TaskState
                       Symbol
                       Symbol
                       Int
                       (Bind (U (Finished (Hash String Int)) Symbol))))
                     (Branch
                      (Effs)
                      (Effs)
                      (Effs (Stop assign))
                      (Effs
                       (Stop
                        perform
                        (Branch
                         (Effs
                          (Role
                           (done)
                           (Shares (JobFinished Symbol (Hash String Int))))
                          (Realizes (TasksFinished Symbol)))
                         (Effs))))))
                    (Reacts
                     OnStart
                     (Role
                      (take-slot)
                      (Reacts
                       (Asserted (TaskState Symbol Symbol Int Discard))
                       (Stop take-slot))))
                    (Reacts
                     (Retracted (TaskManager Symbol Discard))
                     (Stop assign))))
                  (Effs)))
                (Effs)))))
            (Reacts OnStop)
            (Reacts OnStart)))
          (Reacts (Realize (TasksFinished Symbol)) (Stop delegate-tasks))))
        (Reacts
         (Retracted
          (Job
           Symbol
           (List (Task Int (U (MapWork String) (ReduceWork Int Int))))))
         (Stop during-inner))))
      (Reacts (Retracted (TaskManager (Bind Symbol) (Bind Int))))
      (Reacts (Asserted (TaskManager (Bind Symbol) (Bind Int))))))

  ;; fixed above bug
  (define job-manager-v3
    '(Role
      (jm)
      (Shares (JobManagerAlive))
      (Reacts
       (Asserted
        (Job
         (Bind Symbol)
         (Bind (List (Task Int (U (MapWork String) (ReduceWork Int Int)))))))
       (Role
        (during-inner)
        (Reacts
         OnStart
         (Role
          (delegate-tasks)
          (Reacts
           OnDataflow
           (Role
            (perform)
            (Reacts
             OnStart
             (Role
              (select)
              (Reacts (Forget (SelectedTM (Bind Symbol))))
              (Reacts
               OnDataflow
               (Branch
                (Effs
                 (Branch
                  (Effs
                   (Role
                    (assign)
                    (Shares
                     (TaskAssignment
                      Symbol
                      Symbol
                      (Task
                       Int
                       (U
                        (MapWork String)
                        (ReduceWork (Hash String Int) (Hash String Int))))))
                    (Know (SelectedTM Symbol))
                    (Reacts
                     (Asserted
                      (TaskState
                       Symbol
                       Symbol
                       Int
                       (Bind (U (Finished (Hash String Int)) Symbol))))
                     (Branch
                      (Effs)
                      (Effs)
                      (Effs (Stop assign))
                      (Effs
                       (Stop
                        perform
                        (Branch
                         (Effs
                          (Realizes (TasksFinished Symbol (Hash String Int))))
                         (Effs))))))
                    (Reacts
                     OnStart
                     (Role
                      (take-slot)
                      (Reacts
                       (Asserted (TaskState Symbol Symbol Int Discard))
                       (Stop take-slot))))
                    (Reacts
                     (Retracted (TaskManager Symbol Discard))
                     (Stop assign))))
                  (Effs)))
                (Effs)))))
            (Reacts OnStop)
            (Reacts OnStart)))
          (Reacts
           (Realize (TasksFinished Symbol (Bind (Hash String Int))))
           (Stop
            delegate-tasks
            (Role (done) (Shares (JobFinished Symbol (Hash String Int))))))))
        (Reacts
         (Retracted
          (Job
           Symbol
           (List (Task Int (U (MapWork String) (ReduceWork Int Int))))))
         (Stop during-inner))))
      (Reacts (Retracted (TaskManager (Bind Symbol) (Bind Int))))
      (Reacts (Asserted (TaskManager (Bind Symbol) (Bind Int)))))))

;; ---------------------------------------------------------------------------
;; Message Examples/Tests

(define msgy-r1
  '(Role (m)
     (Reacts (Asserted Int)
             (Sends String)
             (Role (m2)
               (Shares (x))))))

(define msgy-r2
  '(Role (m)
         (Reacts (Asserted Int)
                 (Role (m2)
                       (Shares (x))))))

(define msgy-spec
  '(Role (n)
         (Reacts (Asserted Int)
                 (Sends String)
                 (Role (n2)
                       (Shares (x))))))

(module+ test
  (test-case
      "basic functionality of roles with messages"
    (define mr1 (parse-T msgy-r1))
    (check-true (Role? mr1))
    (define mr2 (parse-T msgy-r2))
    (check-true (Role? mr2))
    (define mrs (parse-T msgy-spec))
    (check-true (Role? mrs))
    (define rg1 (compile mr1))
    (check-true (role-graph? rg1))
    (define rg2 (compile mr2))
    (check-true (role-graph? rg2))
    (define rgs (compile mrs))
    (check-true (role-graph? rgs))
    (check-true (simulates? mr1 mr1))
    (check-true (simulates? mr2 mr2))
    (check-true (simulates? mrs mrs))
    (check-true (simulates? mr1 mrs))
    (check-false (simulates? mr2 mrs))))

(module+ demo-leader-subgraph
  (define leader
    '(Role ; = react
      (get-quotes)
      (Reacts ; = on
       (Asserted (BookQuoteT String (Bind Int)))
       (Branch
        (Effs (Branch (Effs (Stop get-quotes)) (Effs)))
        (Effs
         (Role
          (poll-members)
          (Reacts
           (Asserted (BookInterestT String (Bind String) Discard))
           (Branch
            (Effs (Stop poll-members (Branch (Effs (Stop get-quotes)) (Effs))))
            (Effs))
           (Branch
            (Effs
             (Stop get-quotes (Role (announce) (Shares (BookOfTheMonthT String)))))
            (Effs)))
          (Reacts (Retracted (BookInterestT String (Bind String) Bool)))
          (Reacts (Asserted (BookInterestT String (Bind String) Bool)))
          (Reacts (Retracted (BookInterestT String (Bind String) Bool)))
          (Reacts (Asserted (BookInterestT String (Bind String) Bool)))))))
      (Reacts (Retracted (ClubMemberT (Bind String))))
      (Reacts (Asserted (ClubMemberT (Bind String))))))

  (define leader-impl (parse-T leader))
  (define simulating (simulating-subgraphs leader-impl leader-spec))
  (displayln (length simulating))
  (define largest (argmax role-graph-size simulating))
  (render-to-file largest "largest-simulating.dot")
  )

(module+ demo-removing-internal-events
  (define ty
    '(Role (x)
           (Reacts OnStart
                   (Role (y)
                         (Shares (Hi))
                         (Reacts (Asserted (Bye))
                                 (Stop y))))))
  (define r (parse-T ty))
  (define rg (compile r))
  (define rgi (compile/internal-events rg))
  (render-to-file rg "before.dot")
  (render-to-file rgi "after.dot")
)

(module+ test
  (test-case
    "regression: ok for implementation not to have edges if the current state matches"
    (define a (role-graph
               (set 'seller341 'during-inner343)
               (hash
                (set 'seller341 'during-inner343)
                (state
                 (set 'seller341 'during-inner343)
                 '#hash()
                 (set
                  '#s(Observe #s(Observe #s(Struct BookQuoteT (#s(Base String) #s(Mk⋆)))))
                  '#s(Struct BookQuoteT (#s(Base String) #s(Base Int))))))))
    (define b (role-graph
               (set 'seller)
               (hash
                (set 'seller)
                (state
                 (set 'seller)
                 (hash
                  '#s(Asserted #s(Observe #s(Struct BookQuoteT (#s(Base String) #s(Mk⋆)))))
                  (set (transition '() (set '_ 'seller))))
                 (set
                  '#s(Observe #s(Observe #s(Struct BookQuoteT (#s(Base String) #s(Mk⋆)))))))
                (set '_ 'seller)
                (state
                 (set '_ 'seller)
                 '#hash()
                 (set
                  '#s(Observe #s(Observe #s(Struct BookQuoteT (#s(Base String) #s(Mk⋆)))))
                  '#s(Struct BookQuoteT (#s(Base String) #s(Base Int))))))))
    (check-true (run/timeout (thunk (simulates?/rg a b))))))
