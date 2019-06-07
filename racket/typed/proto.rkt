#lang racket

(require (only-in racket/hash hash-union))

(module+ test
  (require rackunit))

;; -------------------------------------------------------------------------
;; Role Type Data Definitions

;; a FacetName is a symbol

;; a T is one of
;;   - (Role FacetName (Listof EP)), also abbreviated as just Role
;;   - (Spawn τ)
;;   - (Stop FacetName Body)
(struct Role (nm eps) #:transparent)
(struct Spawn (ty) #:transparent)
(struct Stop (nm body) #:transparent)

;; a EP is one of
;;   - (Reacts D Body), describing an event handler
;;   - (Shares τ), describing an assertion
(struct Reacts (evt body) #:transparent)
(struct Shares (ty) #:transparent)

;; a Body describes actions carried out in response to some event, and
;; is one of
;;   - T
;;   - (Listof Body)
;;   - (Branch (Listof Body))
(struct Branch (arms) #:transparent)

;; a D is one of
;;   - (Know τ), reaction to assertion
;;   - (¬Know τ), reaction to retraction
;;   - StartEvt, reaction to facet startup
;;   - StopEvt, reaction to facet shutdown
;;   - DataflowEvt, reaction to field updates
(struct Know (ty) #:transparent)
(struct ¬Know (ty) #:transparent)
(define StartEvt 'Start)
(define StopEvt 'Stop)
(define DataflowEvt 'Dataflow)

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
  (Reacts (Know assertion)
          (Role facet-name
                (cons (Reacts (¬Know assertion)
                              (Stop facet-name '()))
                      eps))))

;; --------------------------------------------------------------------------
;; Examples

(define manager
  (Role 'account-manager
        (list (Shares (Struct 'account (list Int)))
              (Reacts (Know (Struct 'deposit '())) '()))))
(define client
  (Role 'client
        (list (Reacts (Know (Struct 'account (list Int))) '()))))

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
         (Reacts (Know (Observe (book-quote String ⋆)))
                 (Role 'fulfill
                       (list (Shares (book-quote String Int))))))))

(define seller-actual
  (Role
   'seller27
   (list
    (Reacts
     (Know (Observe (book-quote String ⋆)))
     (Role
      'during-inner29
      (list
       (Shares (book-quote String (U (list Int Int))))
       (Reacts
        (¬Know (Observe (book-quote String ⋆)))
        (Stop 'during-inner29 '()))))))))

(define leader-spec
  (Role 'leader
        (list
         (Reacts
          (Know (book-quote String Int))
          (Role 'poll
                (list
                 (Reacts
                  (Know (book-interest String String Bool))
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
     (Know (book-quote String Int))
     (Branch
      (list
       ;; problem 1: spec doesn't say actor can give up when running out of books
       (Stop 'get-quotes '())
       (Role
        'poll-members
        (list
         (Reacts
          (Know (book-interest String String ⋆))
          (Branch (list
                   ;; problem 2: combining poll-members and get-quotes here (should be another branch)
                   (Stop 'poll-members
                         (Stop 'get-quotes '()))
                   (Stop 'get-quotes
                         (Role 'announce
                               (list
                                (Shares (book-of-the-month String))))))))
         (Reacts (¬Know (book-interest String String Bool)) (list))
         (Reacts (Know (book-interest String String Bool)) (list))
         (Reacts (¬Know (book-interest String String Bool)) (list))
         (Reacts (Know (book-interest String String Bool)) (list)))))))
    (Reacts (¬Know (club-member String)) (list))
    (Reacts (Know (club-member String)) (list)))))

(define leader-fixed?
  (Role 'get-quotes
        (list
         (Reacts (Know (book-quote String Int))
                 (Branch (list
                          (Role 'poll-members
                                (list
                                 (Reacts (Know (book-interest String String ⋆))
                                         (Branch (list
                                                  (Stop 'poll-members
                                                        '())
                                                  (Stop 'get-quotes
                                                        (Role 'announce
                                                              (list
                                                               (Shares (book-of-the-month String))))))))
                                 (Reacts (¬Know (book-interest String String Bool)) (list))
                                 (Reacts (Know (book-interest String String Bool)) (list))
                                 (Reacts (¬Know (book-interest String String Bool)) (list))
                                 (Reacts (Know (book-interest String String Bool)) (list)))))))
         (Reacts (¬Know (club-member String)) (list))
         (Reacts (Know (club-member String)) (list)))))

(define leader-revised
  (Role
   'get-quotes
   (list
    (Reacts
     (Know (book-quote String Int))
     (Branch
      (list
       (Branch (list (Stop 'get-quotes (list)) (list)))
       (Role
        'poll-members
        (list
         (Reacts
          (Know (book-interest String String ⋆))
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
         (Reacts (¬Know (book-interest String String Bool)) (list))
         (Reacts (Know (book-interest String String Bool)) (list))
         (Reacts (¬Know (book-interest String String Bool)) (list))
         (Reacts (Know (book-interest String String Bool)) (list)))))))
    (Reacts (¬Know (club-member String)) (list))
    (Reacts (Know (club-member String)) (list)))))

(define member-spec
  (Role
   'member
   (list
    (Shares (club-member String))
    (Reacts (Know (Observe (book-interest String ⋆ ⋆)))
            (Role 'respond
                  (list
                   (Shares (book-interest String String Bool))))))))

(define member-actual
  (Role
   'member41
   (list
   (Shares (club-member String))
   (Reacts
    (Know (Observe (book-interest String ⋆ ⋆)))
    (Role
     'during-inner42
     (list
     (Shares (book-interest String String Bool))
     (Reacts
      (¬Know (Observe (book-interest String ⋆ ⋆)))
      ;; this bit is a noticeable deviation from the spec
      (Stop 'during-inner42 '()))))))))

;; -----------------------------------------------------------------------------
;; Compiling Roles to state machines

;; a State is a (state StateName (Hashof D (Setof StateName)))
;;   where each D in the hash satisfies external-evt?
;; a StateName is a (Setof FacetName)
;; let's assume that all FacetNames are unique
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
       (define all-txns
         (for/list ([nm (in-set current)])
           (hash-ref roles# nm)))
       (define agg-txn
         (for/fold ([agg (hash)])
                   ([txns (in-list all-txns)])
           (hash-union agg txns
                       #:combine combine-effect-sets)))
       (define transitions
         (for/hash ([(D effs) (in-hash agg-txn)]
                    #:when (external-evt? D))
           ;; TODO - may want to remove self loops here
           (define destinations
             (for*/set ([eff* (in-set effs)]
                        [dst (in-set (apply-effects eff* current ft roles#))])
               dst))
           (values D destinations)))
       (define new-work
         (for*/list ([st-set (in-hash-values transitions)]
                     [st (in-set st-set)]
                     #:unless (equal? st current)
                     #:unless (hash-has-key? states st))
           st))
       (loop (append more new-work)
             (hash-set states current (state current transitions)))]
      ['()
       (role-graph (set (Role-nm role)) states)])))

;; D -> Bool
;; test if D corresponds to an external event (assertion, message)
(define (external-evt? D)
  (match D
    [(or (Know _)
         (¬Know _))
     #t]
    [(== DataflowEvt)
     #t]
    [_
     #f]))

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
    (check-true (hash-has-key? transitions (Know quote-request)))
    (check-equal? (hash-ref transitions (Know quote-request))
                  (set (set 'seller 'fulfill))))
  (test-case
      "compile role that quits"
    (define r
      (Role 'x
            (list (Reacts (Know Int)
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
    (check-true (hash-has-key? transitions (Know Int)))
    (check-equal? (hash-ref transitions (Know Int))
                  (set (set))))

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
    (define bq (Know (book-quote String Int)))
    (check-true (hash? gq-transitions))
    (check-true (hash-has-key? gq-transitions bq))
    (define dests (hash-ref gq-transitions bq))
    (check-true (set? dests))
    (check-true (set-member? dests (set 'get-quotes 'poll-members)))
    (check-true (hash-has-key? state# (set 'get-quotes 'poll-members)))
    (define gqpm-st (hash-ref state# (set 'get-quotes 'poll-members)))
    (check-true (state? gqpm-st))
    (match-define (state _ gqpm-transitions) gqpm-st)
    (define bi (Know (book-interest String String ⋆)))
    (check-true (hash? gqpm-transitions))
    (check-true (hash-has-key? gqpm-transitions bi))
    (define dests2 (hash-ref gqpm-transitions bi))
    (check-true (set? dests2))
    (check-true (set-member? dests2 (set 'get-quotes))))

  (test-case
      "simplified poll should quit"
    (define poll/simpl
      (Role
       'poll-members
       (list
        (Reacts
         (Know (book-interest String String ⋆))
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
    (define bi (Know (book-interest String String ⋆)))
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
;; TODO - leaving out Spawn here
(struct start (nm) #:transparent)
(struct stop (nm) #:transparent)

;; a TransitionDesc is a (Hashof D (Setof (Listof RoleEffect)), describing the
;; possible ways an event (+/- of an assertion) can alter the facet tree.
;; It always includes the keys StartEvt and StopEvt.
(define txn-desc0 (hash StartEvt (set) StopEvt (set)))

;; (Listof RoleEffect) StateName
;; FacetTree
;; (Hashof FacetName TransitionDesc)
;; -> (Setof StateName)
;; determine the state resulting from some effects, given the currently active
;; facets and a description of possible facet locations and behavior.
(define (apply-effects effs st ft txn#)
  (let loop ([st st]
             [effs effs])
    (match effs
      ['()
       (set st)]
      [(cons eff rest)
       (match eff
         [(start nm)
          (define st+ (set-add st nm))
          (define start-effs (hash-ref (hash-ref txn# nm) StartEvt))
          (cond
            [(set-empty? start-effs)
             (loop st+ rest)]
            [else
             (for*/set ([eff* (in-set start-effs)]
                        [result (in-set (loop st+ (append rest eff*)))])
               result)])]
         [(stop nm)
          ;; better include nm
          (define children (find-children ft nm st))
          (define st-
            (for/fold ([st st])
                      ([c (in-list children)])
              (set-remove st c)))
          (for/fold ([sts (set st-)])
                    ([f-name (in-list children)])
            (define stop-effs (hash-ref (hash-ref txn# f-name) StopEvt))
            (cond
              [(set-empty? stop-effs)
               (for*/set ([st (in-set sts)]
                          [result (in-set (loop st rest))])
                 result)]
              [else
               (for*/set ([st (in-set sts)]
                          [effs* (in-set stop-effs)]
                          [result (in-set (loop st (append rest effs*)))])
                 result)]))])])))

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
    [(Spawn _)
     (error)]))

;; Role -> TransitionDesc
;; determine how the event handlers in a role alter the facet tree
(define (describe-role role)
  (match role
    [(Role nm eps)
     (for/fold ([txns txn-desc0])
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
          (define (update-effect-set existing)
            (combine-effect-sets effects existing))
          (hash-update txns evt update-effect-set (set))]))]))

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
                  txn-desc0))
  (test-case
      "describe nested role"
    (define desc (describe-roles seller))
    (check-true (hash-has-key? desc 'seller))
    (check-true (hash-has-key? desc 'fulfill))
    (check-equal? (hash-ref desc 'fulfill)
                  txn-desc0)
    (define seller-txns (hash-ref desc 'seller))
    (define quote-request
      (Observe (book-quote String ⋆)))
    (check-true (hash-has-key? seller-txns (Know quote-request)))
    (check-equal? (hash-ref seller-txns (Know quote-request))
                  (set (list (start 'fulfill)))))
  (test-case
      "describe-roles bug"
    (define role (Role 'poll
                       (list
                        (Reacts (Know Int)
                                (Branch
                                 (list (Stop 'leader (Role 'announce (list (Shares Int))))
                                       (Stop 'poll (list))))))))
    (define desc (describe-roles role))
    (check-true (hash? desc))
    (check-true (hash-has-key? desc 'poll))
    (define txns (hash-ref desc 'poll))
    (check-true (hash-has-key? txns (Know Int)))
    (check-equal? (hash-ref txns (Know Int))
                  (set (list (stop 'leader) (start 'announce))
                       (list (stop 'poll)))))
  (test-case
      "leader-spec announce"
    (define desc (describe-roles leader-spec))
    (check-true (hash-has-key? desc 'announce))
    (check-equal? (hash-ref desc 'announce)
                  txn-desc0))
  (test-case
      "leader-spec transitions from {leader,poll} to {leader}"
    (define desc (describe-roles leader-spec))
    (check-true (hash-has-key? desc 'poll))
    (define poll-txns (hash-ref desc 'poll))
    (define evt (Know (book-interest String String Bool)))
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
    [(list (Know τ1) (Know τ2))
     (<:? τ1 τ2)]
    [(list (¬Know τ1) (¬Know τ2))
     (<:? τ1 τ2)]
    [(list (== StartEvt) (== StartEvt))
     #t]
    [(list (== StopEvt) (== StopEvt))
     #t]
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
    [(Reacts D _)
     (match D
       [(or (Know τ)
            (¬Know τ))
        ;; TODO - this doesn't put ⋆ in where an underlying pattern uses a capture
        (Observe τ)]
       [_
        #f])]))

(module+ test
  ;; make sure the or pattern above works the way I think it does
  (check-equal? (EP-assertion (Reacts (Know Int) #f))
                (Observe Int))
  (check-equal? (EP-assertion (Reacts (¬Know String) #f))
                (Observe String)))

;; an Equation is (equiv StateName StateName)
;;
;; a Goal is one of
;;   - Equation
;;   - (one-of (Setof StateMatch))
;;
;; a StateMatch is a (Setof Equation)
(struct equiv (a b) #:transparent)
(struct one-of (opts) #:transparent)

;; (Setof StateName) (Setof StateName) -> (Setof (Setof Equation))
;; Create potential state matchings
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
  (match-define (role-graph st0-1 st#1) (compile role1))
  (match-define (role-graph st0-2 st#2) (compile role2))
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
           (for/and ([goal (in-set matching)])
             (define hypotheses (set-remove matching goal))
             (verify goal (set-union hypotheses assumptions))))])))
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

;; (Hashof D (Setof StateName))
;; (Hashof D (Setof StateName))
;; (Goal -> Bool) -> Bool
;; Determine if:
;;   for each event D going from sn2,
;;   for each event E, D <: E, going from sn1,
;;   (with the exception of the Dataflow HACK below)
;;   for the set of states X connected to sn2 by D,
;;   for the set of states Y connected to sn1 by E,
;;   it is possible to pair the states of X and Y such that they are in simulation,
;;   as determined by the verify procedure
(define (same-on-specified-events? transitions1 transitions2 verify)
  (for/and ([(D2 dests2) (in-hash transitions2)])
    (define dests1
      (for/fold ([agg (set)])
                ([(D1 dests) (in-hash transitions1)]
                 #:when (D<:? D2 D1)
                 ;; only consider dataflow events vs. non-dataflow when
                 ;; there is not a dataflow edge in the spec (HACK)
                 #:unless (and (equal? D1 DataflowEvt)
                               (not (equal? D2 DataflowEvt))
                               (hash-has-key? transitions2 D1)))
        (set-union agg dests)))
    (cond
      [(set-empty? dests1)
       #f]
      [else
       (define combos (make-combinations dests1 dests2))
       (verify (one-of combos))])))


;; (Hashof D (Setof StateName))
;; (Hashof D (Setof StateName))
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
             [dest (in-set (hash-ref transitions1 evt))])
    (verify (equiv dest sn2))))

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
         (Know (Observe (book-interest String ⋆ ⋆)))
         (Role
          'during-inner42
          (list
           (Shares (book-interest String String Bool))
           (Reacts
            (¬Know (Observe (book-interest String ⋆ ⋆)))
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
;; Visualization

(module+ vis
  ;; TODO - for now, assume there are no names that need escaping

  ;; RoleGraph -> DotString
  ;; name is an optional string
  ;; translate the states to DOT that can be passed to graphviz
  (define (render rg
                  #:name [name #f])
    (match-define (role-graph st0 st#) rg)
    (define graph-name (or name "Roles"))
    (define entry-node (format "~a;" (state-name->dot-name st0)))
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

  ;; String D StateName -> DotString
  ;; describe an edge between the states with the corresponding label
  (define (render-edge from evt to)
    (define target-dot (state-name->dot-name to))
    (define edge-label (D->label evt))
    (format "~a -> ~a [label=\"~a\"];" from target-dot edge-label))

  ;; D -> DotString
  ;; give a description of an event suitable for rendering
  (define (D->label evt)
    (match evt
      [(Know τ)
       (string-append "+" (τ->string τ))]
      [(¬Know τ)
       (string-append "-" (τ->string τ))]
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
  )

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
    [(list 'Know t)
     (Know (parse-τ t))]
    [(list '¬Know t)
     (¬Know (parse-τ t))]
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

(define real-seller-ty
  '(Role
    (seller)
    (Reacts
     (Know (Observe (BookQuoteT (Bind String) Discard)))
     (Role
      (during-inner)
      (Shares (BookQuoteT String Int))
      (Reacts
       (¬Know (Observe (BookQuoteT String Discard)))
       (Stop during-inner))))))

(define real-member-ty
  '(Role
    (member)
    (Shares (ClubMemberT String))
    (Reacts
     (Know (Observe (BookInterestT (Bind String) Discard Discard)))
     (Role
      (during-inner)
      (Shares (BookInterestT String String Bool))
      (Reacts
       (¬Know (Observe (BookInterestT String Discard Discard)))
       (Stop during-inner))))))

(define real-leader-ty
  '(Role
    (get-quotes)
    (Reacts
     (Know (BookQuoteT String (Bind Int)))
     (Branch
      (Effs (Branch (Effs (Stop get-quotes)) (Effs)))
      (Effs
       (Role
        (poll-members)
        (Reacts
         (Know (BookInterestT String (Bind String) Discard))
         (Branch
          (Effs (Stop poll-members (Branch (Effs (Stop get-quotes)) (Effs))))
          (Effs))
         (Branch
          (Effs
           (Stop get-quotes (Role (announce) (Shares (BookOfTheMonthT String)))))
          (Effs)))
        (Reacts (¬Know (BookInterestT String (Bind String) Bool)))
        (Reacts (Know (BookInterestT String (Bind String) Bool)))
        (Reacts (¬Know (BookInterestT String (Bind String) Bool)))
        (Reacts (Know (BookInterestT String (Bind String) Bool)))))))
    (Reacts (¬Know (ClubMemberT (Bind String))))
    (Reacts (Know (ClubMemberT (Bind String))))))

;; ---------------------------------------------------------------------------
;; Flink Examples

(define job-manager-actual
  '(Role
 (jm)
 (Shares (JobManagerAlive))
 (Reacts
  (Know
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
              (Know
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
                (Know (TaskState Symbol Symbol Int Discard))
                (Stop take-slot))))
             (Reacts (¬Know (TaskManager Symbol Discard)) (Stop assign))))
           (Effs)))
         (Effs)))))
     (Reacts OnStop)
     (Reacts OnStart)))
   (Reacts
    (¬Know
     (Job
      Symbol
      (List (Task Int (U (MapWork String) (ReduceWork Int Int))))))
    (Stop during-inner))))
 (Reacts (¬Know (TaskManager (Bind Symbol) (Bind Int))))
 (Reacts (Know (TaskManager (Bind Symbol) (Bind Int))))))

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
     (Know
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
       (¬Know
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
     (Know
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
       (¬Know
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
     (Know (TaskState Symbol Symbol Int ★/t))
     (Branch (Stop assign) (Effs)))))

(module+ test
  (test-case "parse and compile task-assigner-spec"
    (check-true (Role? (parse-T task-assigner-spec)))
    (check-true (role-graph? (compile (parse-T task-assigner-spec))))))

(define task-manager-ty
  '(Role
 (tm)
 (Reacts
  (Know (JobManagerAlive))
  (Role
   (during-inner1)
   (Shares (TaskManager Symbol Int))
   (Reacts
    (Know
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
      (Know
       (TaskState
        Symbol
        Symbol
        Int
        (Bind (U (Finished (Hash String Int)) Symbol)))))
     (Reacts OnStop)
     (Reacts
      (¬Know
       (TaskAssignment
        Symbol
        Symbol
        (Task
         Int
         (U
          (MapWork String)
          (ReduceWork (Hash String Int) (Hash String Int))))))
      (Stop during-inner2))))
   (Reacts (¬Know (TaskRunner (Bind Symbol) (U (Executing Int) Symbol))))
   (Reacts (Know (TaskRunner (Bind Symbol) (U (Executing Int) Symbol))))
   (Reacts (¬Know (TaskRunner (Bind Symbol) Discard)))
   (Reacts (Know (TaskRunner (Bind Symbol) Discard)))
   (Reacts (¬Know (JobManagerAlive)) (Stop during-inner1))))))

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
