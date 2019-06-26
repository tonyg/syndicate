#lang racket

(require (only-in racket/hash hash-union))
(require racket/generator)

(module+ test
  (require rackunit))

;; -------------------------------------------------------------------------
;; Role Type Data Definitions

;; a FacetName is a symbol

;; a T is one of
;;   - (Role FacetName (Listof EP)), also abbreviated as just Role
;;   - (Spawn τ)
;;   - (Sends τ)
;;   - (Realizes τ)
;;   - (Stop FacetName Body)
(struct Role (nm eps) #:transparent)
(struct Spawn (ty) #:transparent)
(struct Sends (ty) #:transparent)
(struct Realizes (ty) #:transparent)
(struct Stop (nm body) #:transparent)

;; a EP is one of
;;   - (Reacts D Body), describing an event handler
;;   - (Shares τ), describing an assertion
;;   - (Know τ), describing an internal assertion
(struct Reacts (evt body) #:transparent)
(struct Shares (ty) #:transparent)
(struct Know (ty) #:transparent)

;; a Body describes actions carried out in response to some event, and
;; is one of
;;   - T
;;   - (Listof Body)
;;   - (Branch (Listof Body))
(struct Branch (arms) #:transparent)

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
(struct Asserted (ty) #:transparent)
(struct Retracted (ty) #:transparent)
(struct Message (ty) #:transparent)
(struct Forget (ty) #:transparent)
(struct Realize (ty) #:transparent)
(define StartEvt 'Start)
(define StopEvt 'Stop)
(define DataflowEvt 'Dataflow)

;; a D+ is a D with StartEvt and StopEvt replaced with variants that name the
;; specified facet,
;;   - (StartOf FacetName)
;;   - (StopOf FacetName)
(struct StartOf (fn) #:transparent)
(struct StopOf (fn) #:transparent)

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
(struct U (tys) #:transparent)
(struct Struct (nm tys) #:transparent)
(struct Observe (ty) #:transparent)
(struct List (ty) #:transparent)
(struct Set (ty) #:transparent)
(struct Hash (ty-k ty-v) #:transparent)
(struct Mk⋆ () #:transparent)
;; TODO this might be a problem when used as a match pattern
(define ⋆ (Mk⋆))
(struct Base (name) #:transparent)
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
;; Testing Utilities

(module+ test
  (require racket/engine)

  ;; (-> A) Real -> (U A Engine)
  ;; run the given thunk in an engine for 'fuel' milliseconds
  ;; if the engine completes, returns the result, otherwise the engine itself
  (define (run/timeout tnk [fuel 1000])
    (define e (engine (lambda (p) (tnk))))
    (define r (engine-run fuel e))
    (if r (engine-result e) e)))


;; -----------------------------------------------------------------------------
;; Compiling Roles to state machines

;; a State is a (state StateName (Hashof D+ (Setof Transition)))
;; a StateName is a (Setof FacetName)
;; let's assume that all FacetNames are unique
;; a Transition is a (transition (Listof TransitionEffect) StateName)
(struct transition (effs dest) #:transparent)
;; a TransitionEffect is one of
;;   - (send τ)
;;   - (realize τ)
(struct send (ty) #:transparent)
(struct realize (ty) #:transparent)
;; ok, this is also ignoring Spawn actions for now, would show up in the transitions hash
(struct state (name transitions) #:transparent)

;; a FacetTree is a
;;   (facet-tree (Hashof (U #f FacetName) (Listof FacetName))
;;               (Hashof FacetName (U #f FacetName)))
;; describing the potential immediate children of each facet
;; and each facet's parent. The parent of the root facet is #f.
(struct facet-tree (down up) #:transparent)

;; a RoleGraph is a
;;   (role-graph StateName (Hashof StateName State))
;; describing the initial state and the behavior in each state.
(struct role-graph (st0 states) #:transparent)

;; Role -> RoleGraph
;; in each state, the transitions will include the reactions of the parent
;; facet(s)
(define (compile role)
  (define roles# (describe-roles role))
  (define ft (make-facet-tree role))
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
       (define new-work
         (for*/list ([txns (in-hash-values transitions)]
                     [txn (in-set txns)]
                     [st (in-value (transition-dest txn))]
                     #:unless (equal? st current)
                     #:unless (hash-has-key? states st))
           st))
       (loop (append more new-work)
             (hash-set states current (state current transitions)))]
      ['()
       (role-graph (set (Role-nm role)) states)])))

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

;; a DetectedCylce is a (List (Listof StateName) D D D), as in
;;   (list path init evt D)
;; where
;;   - path represents the sequences of states containing a cycle,
;;   - init is the external event that initiated this activity
;;   - evt is the last-taken internal event
;;   - D is the edge in the graph that matched evt

;; RoleGraph Role -> (U RoleGraph DetectedCycle)
;; "Optimize" the given role graph with respect to internal events.
;; The resulting graph will have transitions of only external events.
(define (compile/internal-events rg role)
  (match-define (role-graph st0 orig-st#) rg)
  ;; doing funny business with state (set) here
  (define orig-st#+ (hash-set orig-st# (set) (state (set) (hash))))
  (define assertion# (all-states-assertions/internal (in-hash-keys orig-st#+) role))

  ;; a WorkItem is a
  ;;   (work-item StateName (Listof StateName) D+ (Listof D+) (Listof TransitionEffect))
  ;; such as (work-item from path/r to by with effs), where
  ;;   - from is the origin state for this chain of events
  ;;   - path/r is the list of states in the path to this point, *after* from, in reverse
  ;;     (meaning that all of these transitions are due to *internal* events)
  ;;   - to is the current state that has been reached
  ;;   - by is the external event that kicked off this sequence
  ;;   - with is a list of pending events to be processed
  ;;   - effs are effects emitted on this path
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
                      #:unless (and (set-empty? sn)
                                    (not (equal? sn new-st0))))
             (values sn (state sn txns))))
         (role-graph new-st0 states)]
        [(cons (work-item from path/r to by with effs) more-work)
         (define prev (if (empty? path/r) from (first path/r)))
         (define txn# (state-transitions (hash-ref orig-st#+ to)))
         (define visited+ (set-add visited to))
         (define new-events (route-internal (hash-ref assertion# prev)
                                            (hash-ref assertion# to)))
         ;; TODO - this is saying something about how the implementation schedules handlers;
         ;; I think it should be something like exploring (append with (permutations new-events))
         (define started (for/list ([fn (in-set (set-subtract to prev))]) (StartOf fn)))
         (define stopped (for/list ([fn (in-set (set-subtract prev to))]) (StopOf fn)))
         (define new-events* (cons DataflowEvt (append started stopped (set->list new-events))))
         (define pending (append with new-events*))
         (define pending/first-relevant
           (dropf pending
                  (lambda (evt)
                    (not
                     (for/or ([D (in-hash-keys txn#)])
                       ;; TODO - think I want non-empty intersection instead of subtyping
                       (D<:? evt D))))))
         (match pending/first-relevant
           ['()
            (define new-paths-work
              (for*/list (#:unless (set-member? visited to)
                          [(D txns) (in-hash txn#)]
                          #:when (external-evt? D)
                          #:unless (equal? D DataflowEvt)
                          [t (in-set txns)])
                (match-define (transition es dst) t)
                (work-item to '() dst D (effs->internal-events es) es)))
            (define new-st# (update-path st# from to by effs))
            (walk (append more-work new-paths-work) visited+ new-st#)]
           [(cons evt more-pending)
            (define path/r+ (cons to path/r))
            (define more-labor
              (for*/list ([(D ts) (in-hash txn#)]
                          #:when (D<:? evt D)
                          [t (in-set ts)])
                (match-define (transition more-effs dest) t)
                (when (and (member dest path/r+)
                           ;; TODO - cycles involving Start/Stop are tricky. Punt for now
                           (not (start/stop-evt? evt)))
                  (fail (list (cons from (reverse (cons dest path/r+)))
                              by
                              evt
                              D)))
                (define internal-effs (effs->internal-events more-effs))
                (work-item from
                           path/r+
                           dest
                           by
                           (append more-pending internal-effs)
                           (append effs more-effs))))
            (walk (append more-work more-labor) visited+ st#)])]))
    (local-require racket/trace)
    #;(trace walk)
    (walk (list (work-item (set) '() st0 StartEvt '() '()))
          (set)
          (hash))))

(module+ test
  (test-case
      "most minimal functionality for removing internal events"
    ;; manager role has basically nothing to it
    (define m (compile manager))
    (define i (compile/internal-events m manager))
    (check-true (role-graph? i))
    (check-true (simulates?/rg i manager m manager))
    (check-true (simulates?/rg m manager i manager))
    ;; this isn't necessarily *needed*, but nice to know
    (check-equal? i m))
  (test-case
      "removing internal events on more involved role"
    ;; though it doesn't use any internal events
    (define tmr (parse-T task-manager-ty))
    (define tm (compile tmr))
    (define tmi (compile/internal-events tm tmr))
    (check-true (role-graph? tmi))
    (check-true (simulates?/rg tmi tmr tm tmr))
    (check-true (simulates?/rg tm tmr tmi tmr))
    (check-equal? tmi tm))
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
    (define i (run/timeout (thunk (compile/internal-events rg r))))
    (check-true (list? i))
    (check-equal? (length i) 4)
    (match-define (list path kick-off evt edge) i)
    ;; the first 'x -> 'x cycle is ignored because it's a Start event
    (check-equal? path (list (set) (set 'x) (set 'x) (set 'x)))
    (check-equal? kick-off StartEvt)
    (check-equal? evt (Realize Int))
    (check-equal? edge (Realize Int)))
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
      (define rgi (run/timeout (thunk (compile/internal-events rg r))))
      (check-true (role-graph? rgi))
      (match-define (role-graph st0 st#) rgi)
      (check-equal? st0 (set 'x 'y))
      (check-true (hash-has-key? st# (set 'x 'y)))
      (define xy-txns (state-transitions (hash-ref st# (set 'x 'y))))
      (check-equal? xy-txns (hash (Asserted (Struct 'Bye '()))
                                  (set (transition '() (set 'x)))))
      (check-true (hash-has-key? st# (set 'x)))
      (define x-txns (state-transitions (hash-ref st# (set 'x))))
      (check-equal? x-txns (hash)))))

;; (Setof τ) (Setof τ) -> (Setof D)
;; Subtyping-based assertion routing (*not* intersection - TODO)
(define (route-internal prev current)
  ;; note that messages are handled separately, don't need to worry about them
  ;; here
  (define old-interests (interests prev))
  (define old-matches (matching-interests old-interests prev))
  (define new-interests (interests current))
  (define new-matches (matching-interests new-interests current))
  (define appeared (label-assertions (assertion-delta new-matches old-matches) Know))
  (define disappeared (label-assertions (assertion-delta old-matches new-matches) Forget))
  (define appearing-interests (assertion-delta new-interests old-interests))
  (define newly-relevant (label-assertions (matching-interests appearing-interests current) Know))
  (set-union appeared disappeared newly-relevant))

;; (Setof τ) -> (Setof τ)
;; the type of interests in a set
(define (interests as)
  (for/set ([a (in-set as)]
            #:when (Observe? a))
    (Observe-ty a)))

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

;; (Listof (TransitionEffect)) -> (Listof D)
(define (effs->internal-events effs)
  (for/list ([e (in-list effs)]
             #:when (realize? e))
    (match-define (realize m) e)
    (Realize m)))

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

(module+ test
  (test-case
      "compile seller"
    (define rg (compile seller))
    (check-true (role-graph? rg))
    (match-define (role-graph sn0 seller#) rg)
    (check-equal? sn0 (set 'seller))
    (check-true (hash-has-key? seller# (set 'seller)))
    (check-true (hash-has-key? seller# (set 'seller 'fulfill)))
    (check-equal? (hash-keys seller#)
                  (list (set 'seller 'fulfill)
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
    (match-define (state _ gq-transitions) gq-st)
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
    (match-define (state _ gqpm-transitions) gqpm-st)
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
         ;; TODO - handle Spawn
         [(or (Sends _)
              (Realizes _))
          (loop rest downs ups)]
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
          (define new-parent (hash-ref ups target))
          (define more-work
            (for/list ([k (in-list (Body->actions body))])
              (cons new-parent k)))
          (loop (append rest more-work)
                downs
                ups)])])))

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

;; a RoleEffect is one of
;;   - (start RoleName)
;;   - (stop RoleName)
;;   - (send τ)
;;   - (realize τ)
;; TODO - leaving out Spawn here
(struct start (nm) #:transparent)
(struct stop (nm) #:transparent)

;; a TransitionDesc is a (Hashof D+ (Setof (Listof RoleEffect)), describing the
;; possible ways an event (+/- of an assertion) can alter the facet tree.
;; It always includes the keys (StartOf FacetName) and (StopOf FacetName).
(define (txn-desc0 fn) (hash (StartOf fn) (set) (StopOf fn) (set)))

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
          (set (transition (list eff) st))]
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
          (for/fold ([txns (set (transition '() st-))])
                    ([f-name (in-list children)])
            (define stop-effs (hash-ref (hash-ref txn# f-name) (StopOf f-name)))
            (define stop-effs+ (if (set-empty? stop-effs)
                                   (set '())
                                   stop-effs))
            (for*/set ([txn (in-set txns)]
                       [st (in-value (transition-dest txn))]
                       [effs* (in-set stop-effs+)]
                       [next-txn (in-set (loop st (append effs* rest)))])
              (transition (append (transition-effs txn)
                                  (transition-effs next-txn))
                          (transition-dest next-txn))))])])))

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
    [(Spawn _)
     (error 'enumerate-roles "Spawn not yet implemented")]))

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
    [(Spawn _)
     (error)]))

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

;; Role -> (Setof τ)
;; Compute the set of internal assertions the role contributes (on its own, not
;; considering parent assertions)
(define (role-assertions/internal r)
  (for*/set ([ep (in-list (Role-eps r))]
             [τ? (in-value (EP-assertion/internal ep))]
             #:when τ?)
    τ?))

;; EP -> (U #f τ)
;; the type of assertion and endpoint contributes, otherwise #f for
;; dataflow/start/stop
(define (EP-assertion EP)
  (match EP
    [(Shares τ)
     τ]
    [(Reacts D _)
     (match D
       [(or (Asserted τ)
            (Retracted τ)
            (Message τ))
        ;; TODO - this doesn't put ⋆ in where an underlying pattern uses a capture
        (Observe τ)]
       [_
        #f])]
    [_ #f]))

;; EP -> (U #f τ)
;; the type of internal assertion and endpoint contributes, otherwise #f for
;; dataflow/start/stop
(define (EP-assertion/internal EP)
  (match EP
    [(Know τ)
     τ]
    [(Reacts D _)
     (match D
       [(or (Know τ)
            (Forget τ)
            (Realize τ))
        ;; TODO - this doesn't put ⋆ in where an underlying pattern uses a capture
        (Observe τ)]
       [_
        #f])]
    [_ #f]))

(module+ test
  ;; make sure the or pattern above works the way I think it does
  (check-equal? (EP-assertion (Reacts (Asserted Int) #f))
                (Observe Int))
  (check-equal? (EP-assertion (Reacts (Retracted String) #f))
                (Observe String)))

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
  (simulates?/rg rg1 role1 rg2 role2))

;; RoleGraph Role RoleGraph Role -> Bool
;; determine if the first role acts suitably like the second role.
;; at all times, it is asserting a superset of the second's assertions
;; rg1 ~ actual
;; rg2 ~ spec
;; like simulates?, but take in and use the compiled role graph; the role1 and
;; role2 arguments are just for determining the assertions in each state
;; useful when checking subgraphs
(define (simulates?/rg rg1 role1 rg2 role2)
  (match-define (role-graph st0-1 st#1) rg1)
  (match-define (role-graph st0-2 st#2) rg2)
  (define assertion#1 (all-states-assertions (in-hash-keys st#1) role1))
  (define assertion#2 (all-states-assertions (in-hash-keys st#2) role2))
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
         (define assertions1 (hash-ref assertion#1 sn1))
         (define assertions2 (hash-ref assertion#2 sn2))
         (unless (assertion-superset? assertions1 assertions2)
           (return #f))
         (define transitions1 (state-transitions (hash-ref st#1 sn1)))
         (define transitions2 (state-transitions (hash-ref st#2 sn2)))
         (define (verify/with-current-assumed g)
           (verify g (set-add assumptions goal)))
         (unless (same-on-specified-events? transitions1
                                            transitions2
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

;; (Sequenceof StateName) Role -> (Hashof StateName (Setof τ))
;; map each state name to its active assertions
(define (all-states-assertions state-seq role)
  (define all-roles (enumerate-roles role))
  (define assertion# (all-roles-assertions all-roles))
  (for/hash ([sn state-seq])
    (values sn
            (for/fold ([assertions (set)])
                      ([facet-name (in-set sn)])
              (set-union assertions (hash-ref assertion# facet-name (set)))))))

;; (Sequenceof StateName) Role -> (Hashof StateName (Setof τ))
;; map each state name to its active internal assertions
(define (all-states-assertions/internal state-seq role)
  (define all-roles (enumerate-roles role))
  (define assertion# (all-roles-assertions/internal all-roles))
  (for/hash ([sn state-seq])
    (values sn
            (for/fold ([assertions (set)])
                      ([facet-name (in-set sn)])
              (set-union assertions (hash-ref assertion# facet-name (set)))))))

;; (List Role) -> (Hashof RoleName (Setof τ))
;; map each role's name to the assertions it contributes
(define (all-roles-assertions roles)
  (for/hash ([role (in-list roles)])
    (values (Role-nm role)
            (role-assertions role))))

;; (List Role) -> (Hashof RoleName (Setof τ))
;; map each role's name to the internal assertions it contributes
(define (all-roles-assertions/internal roles)
  (for/hash ([role (in-list roles)])
    (values (Role-nm role)
            (role-assertions/internal role))))

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
(define (same-on-specified-events? transitions1 transitions2 verify)
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
       #f]
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
  (define spec-rg (compile spec))
  (define impl-rg (compile impl))
  (define evts (relevant-events spec-rg))
  (for/list ([srg (subgraphs impl-rg evts)]
             #:when (simulates?/rg srg impl spec-rg spec))
    srg))

(module+ test
  (test-case
      "task manager has task performer subgraphs"
    (define tpr (parse-T task-performer-spec))
    (define tmr (parse-T task-manager-ty))
    (define ans (simulating-subgraphs tmr tpr))
    (check-equal? (length ans) 2)
    (define tprg (compile tpr))
    (check-true (simulates?/rg (first ans) tmr tprg tpr))
    (check-true (simulates?/rg (second ans) tmr tprg tpr))))

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
         (define orig-txn# (state-transitions (hash-ref state# st)))
         (define txn#
           (for/hash ([D (in-hash-keys orig-txn#)]
                      #:when (event-enabled? D))
             (define orig-txns (hash-ref orig-txn# D))
             (define new-txns
               (for/set ([txn (in-set orig-txns)]
                         #:when (set-member? states (transition-dest txn)))
                 txn))
             ;; TODO - what if new-txns is empty?
             (values D new-txns)))
         (values st (state st txn#))))
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
       (match-define (state name txn#) (hash-ref state# current))
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
                  (hash (set 'X 'Y 'Z) (state (set 'X 'Y 'Z) (hash (Asserted Int) (set (transition '() (set 'X 'Y 'Z)))
                                                                   (Retracted Int) (set (transition '() (set 'X 'Y)))))
                        (set 'X) (state (set 'X) '#hash())
                        (set 'X 'Y) (state (set 'X 'Y) (hash (Asserted Int) (set (transition '() (set 'X 'Y 'Z))))))))
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
          (set (transition '() (set 'during-inner1 'tm)))))
        (set 'tm)
        (state (set 'tm) '#hash())
        (set 'during-inner1 'tm)
        (state
         (set 'during-inner1 'tm)
         (hash
          (Asserted (Struct 'TaskAssignment (list)))
          (set (transition '() (set 'during-inner2 'during-inner1 'tm))))))))
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
     (paren-join (cons "U" slots))]))

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
     (define bdy (if (= (length body) 1)
                     (first body)
                     body))
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
    (check-false (simulates? leader-revised (parse-T real-leader-ty)))))

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

(define job-manager-actual
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
    OnDataflow
    (Role
     (perform)
     (Reacts
      OnStart
      (Role
       (select)
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
                    (Shares (JobFinished Symbol (Hash String Int)))))
                  (Effs))))))
             (Reacts
              OnStart
              (Role
               (take-slot)
               (Reacts
                (Asserted (TaskState Symbol Symbol Int Discard))
                (Stop take-slot))))
             (Reacts (Retracted (TaskManager Symbol Discard)) (Stop assign))))
           (Effs)))
         (Effs)))))
     (Reacts OnStop)
     (Reacts OnStart)))
   (Reacts
    (Retracted
     (Job
      Symbol
      (List (Task Int (U (MapWork String) (ReduceWork Int Int))))))
    (Stop during-inner))))
 (Reacts (Retracted (TaskManager (Bind Symbol) (Bind Int))))
 (Reacts (Asserted (TaskManager (Bind Symbol) (Bind Int))))))

(module+ test
  (test-case
      "job manager reads and compiles"
    (define jmr (parse-T job-manager-actual))
    (check-true (Role? jmr))
    (define jm (compile jmr))
    (check-true (role-graph? jm))
    (check-true (simulates? jmr jmr))))

(define task-performer-spec
  '(Role
    (listen)
    (Reacts
     (Asserted
      (TaskAssignment
       Symbol
       Symbol
       (Task
        Int
        (U
         (MapWork String)
         (ReduceWork (Hash String Int) (Hash String Int))))))
     (Role
      (during-inner)
      (Reacts
       (Retracted
        (TaskAssignment
         Symbol
         Symbol
         (Task
          Int
          (U
           (MapWork String)
           (ReduceWork (Hash String Int) (Hash String Int))))))
       (Stop during-inner))
      (Shares
       (TaskState
        Symbol
        Symbol
        Int
        (U (Finished (Hash String Int)) Symbol)))))))

(module+ test
  (test-case "parse and compile task-performer-spec"
    (check-true (Role? (parse-T task-performer-spec)))
    (check-true (role-graph? (compile (parse-T task-performer-spec))))))

(define task-runner-ty
  '(Role
    (runner)
    (Shares (TaskRunner Symbol (U (Executing Int) Symbol)))
    (Reacts
     (Asserted
      (TaskAssignment
       Symbol
       (Bind Symbol)
       (Task
        (Bind Int)
        (Bind
         (U
          (MapWork String)
          (ReduceWork (Hash String Int) (Hash String Int)))))))
     (Role
      (during-inner)
      (Shares
       (TaskState Symbol Symbol Int (U (Finished (Hash String Int)) Symbol)))
      (Reacts
       (Retracted
        (TaskAssignment
         Symbol
         Symbol
         (Task
          Int
          (U
           (MapWork String)
           (ReduceWork (Hash String Int) (Hash String Int))))))
       (Stop during-inner))))
    (Reacts OnDataflow)))

(module+ test
  (test-case "parse and compile task-runner-ty"
    (check-true (Role? (parse-T task-runner-ty)))
    (check-true (role-graph? (compile (parse-T task-runner-ty))))
    (check-true (simulates? (parse-T task-runner-ty)
                            (parse-T task-performer-spec)))))

(define task-assigner-spec
  '(Role
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
    (Reacts
     (Asserted (TaskState Symbol Symbol Int ★/t))
     ())))

(module+ test
  (test-case "parse and compile task-assigner-spec"
    (check-true (Role? (parse-T task-assigner-spec)))
    (check-true (role-graph? (compile (parse-T task-assigner-spec))))))

(define task-manager-ty
  '(Role
 (tm)
 (Reacts
  (Asserted (JobManagerAlive))
  (Role
   (during-inner1)
   (Shares (TaskManager Symbol Int))
   (Reacts
    (Asserted
     (TaskAssignment
      Symbol
      (Bind Symbol)
      (Task
       (Bind Int)
       (Bind
        (U
         (MapWork String)
         (ReduceWork (Hash String Int) (Hash String Int)))))))
    (Role
     (during-inner2)
     (Shares
      (TaskAssignment
       Symbol
       Symbol
       (Task
        Int
        (U
         (MapWork String)
         (ReduceWork (Hash String Int) (Hash String Int))))))
     (Shares
      (TaskState Symbol Symbol Int (U (Finished (Hash String Int)) Symbol)))
     (Reacts
      (Asserted
       (TaskState
        Symbol
        Symbol
        Int
        (Bind (U (Finished (Hash String Int)) Symbol)))))
     (Reacts OnStop)
     (Reacts
      (Retracted
       (TaskAssignment
        Symbol
        Symbol
        (Task
         Int
         (U
          (MapWork String)
          (ReduceWork (Hash String Int) (Hash String Int))))))
      (Stop during-inner2))))
   (Reacts (Retracted (TaskRunner (Bind Symbol) (U (Executing Int) Symbol))))
   (Reacts (Asserted (TaskRunner (Bind Symbol) (U (Executing Int) Symbol))))
   (Reacts (Retracted (TaskRunner (Bind Symbol) Discard)))
   (Reacts (Asserted (TaskRunner (Bind Symbol) Discard)))
   (Reacts (Retracted (JobManagerAlive)) (Stop during-inner1))))))

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

(module+ test
  (test-case
      "job manager with internal events basic functionality"
    (define jmr (parse-T job-manager-v2))
    (check-true (Role? jmr))
    (define jmrg (compile jmr))
    (check-true (role-graph? jmrg))
    (check-true (simulates? jmr jmr)))
  (test-case
      "job manager subgraph(s) implement task assigner"
    (define jmr (parse-T job-manager-v2))
    (define tar (parse-T task-assigner-spec))
    ;; TODO - would be good to have a timeout
    (define ans (simulating-subgraphs jmr tar))
    (check-true (list? ans))
    (check-false (empty? ans))))

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
    (Reacts (Asserted (TaskManager (Bind Symbol) (Bind Int))))))

(module+ test
  (test-case
      "job manager v3 basic functionality"
    (define jmr (parse-T job-manager-v3))
    (check-true (Role? jmr))
    (define jmrg (run/timeout (thunk (compile jmr))))
    (check-true (role-graph? jmrg))
    (check-true (run/timeout (thunk (simulates? jmr jmr))))
    (define jmrgi (run/timeout (thunk (compile/internal-events jmrg jmr))))
    (check-true (role-graph? jmrgi))
    (check-true (run/timeout (thunk (simulates?/rg jmrgi jmr jmrgi jmr))))))

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
