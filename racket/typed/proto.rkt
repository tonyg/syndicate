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
(struct Know (ty) #:transparent)
(struct ¬Know (ty) #:transparent)

;; a τ is one of
;;   - (U (Listof τ))
;;   - (Struct StructName (Listof τ ...))
;;   - (Observe τ)
;;   - ⋆
;;   - Int
;;   - String
;;   - Bool
(struct U (tys) #:transparent)
(struct Struct (nm tys) #:transparent)
(struct Observe (ty) #:transparent)
(struct Mk⋆ () #:transparent)
;; TODO this might be a problem when used as a match pattern
(define ⋆ (Mk⋆))
(struct MkInt () #:transparent)
(define Int (MkInt))
(struct MkString () #:transparent)
(define String (MkString))
(struct MkBool () #:transparent)
(define Bool (MkBool))

;; a StructName is a Symbol

;; --------------------------------------------------------------------------
;; Examples

(define manager
  (Role 'account-manager
        (list (Shares (Struct 'account (list Int)))
              (Reacts (Know (Struct 'deposit '())) '()))))
(define client
  (Role 'client
        (list (Reacts (Know (Struct 'account (list Int))) '()))))

(define seller
  (Role 'seller
        (list
         (Reacts (Know (Observe (Struct 'BookQuoteT (list String ⋆))))
                 (Role 'fulfill
                       (list (Shares (Struct 'BookQuoteT (list String Int)))))))))

;; τ τ -> τ
;; short hand for creating a book quote struct type
(define (book-quote ty1 ty2)
  (Struct 'book-quote (list ty1 ty2)))

;; τ τ τ -> τ
;; short hand for creating a book quote interest type
(define (book-interest ty1 ty2 ty3)
  (Struct 'book-interest (list ty1 ty2)))

(define leader-spec
  (Role 'leader
        (list
         (Reacts (Know (book-quote String Int))
                 (Role 'poll
                       (list
                        (Reacts (Know (book-interest String String Bool))
                                (Branch
                                 (list
                                  (Stop 'leader
                                         (Role 'announce
                                               (list
                                                (Shares (Struct 'book-of-the-month String)))))
                                  (Stop 'poll (list)))))))))))

#;(define-type-alias leader-actual
  (Role (get-quotes31)
        (Reacts (Know (BookQuoteT String (Bind Int)))
                (Stop get-quotes)
                (Role (poll-members36)
                      (Reacts OnDataflow
                              (Stop poll-members
                                    (Stop get-quotes))
                              (Stop get-quotes
                                    (Role (announce39)
                                          (Shares (BookOfTheMonthT String)))))
                      (Reacts (¬Know (BookInterestT String (Bind String) Bool)))
                      (Reacts (Know (BookInterestT String (Bind String) Bool)))
                      (Reacts (¬Know (BookInterestT String (Bind String) Bool)))
                      (Reacts (Know (BookInterestT String (Bind String) Bool)))))
        (Reacts (¬Know (ClubMemberT (Bind String))))
        (Reacts (Know (ClubMemberT (Bind String))))))

;; -----------------------------------------------------------------------------
;; Compiling Roles to state machines

;; a State is a (state StateName (Hashof D (Setof StateName)))
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
         (for/hash ([(D effs) (in-hash agg-txn)])
           ;; TODO - may want to remove self loops here
           (define destinations
             (for/set ([eff* (in-set effs)])
               (apply-effects eff* current ft)))
           (values D destinations)))
       (define new-work
         (for*/list ([st-set (in-hash-values transitions)]
                     [st (in-set st-set)]
                    #:unless (hash-has-key? states st))
           st))
       (loop (append more new-work)
             (hash-set states current (state current transitions)))]
      ['()
       (role-graph (set (Role-nm role)) states)])))

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
      (Observe (Struct 'BookQuoteT (list String ⋆))))
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
                  (set (set)))))

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


(module+ test
  (test-case
      "ancestors in leader-spec facet tree"
    (define ft (make-facet-tree leader-spec))
    (check-true (ancestor? 'leader 'leader ft))
    (check-true (ancestor? 'poll 'leader ft))
    (check-false (ancestor? 'leader 'poll ft))
    (check-false (ancestor? 'announce 'leader ft))))

;; a RoleEffect is one of
;;   - (start RoleName)
;;   - (stop RoleName)
;; TODO - leaving out Spawn here
(struct start (nm) #:transparent)
(struct stop (nm) #:transparent)

;; a TransitionDesc is a (Hashof D (Setof (Listof RoleEffect)), describing the
;; possible ways an event (+/- of an assertion) can alter the facet tree

;; (Listof RoleEffect) StateName FacetTree -> StateName determine the state
;; resulting from some effects, given the currently active facets and a
;; description of possible facet locations.
(define (apply-effects effs st ft)
  (for/fold ([st st])
            ([eff (in-list effs)])
    (match eff
      [(start nm)
       (set-add st nm)]
      [(stop nm)
       (for/set ([f-name (in-set st)]
                 #:unless (ancestor? f-name nm ft))
         f-name)])))

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
     (for/fold ([txns (hash)])
               ([ep (in-list eps)]
                #:when (Reacts? ep))
       (match-define (Reacts evt body) ep)
       (define effects (Body->effects body))
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
                  (hash)))
  (test-case
      "describe nested role"
    (define desc (describe-roles seller))
    (check-true (hash-has-key? desc 'seller))
    (check-true (hash-has-key? desc 'fulfill))
    (check-equal? (hash-ref desc 'fulfill)
                  (hash))
    (define seller-txns (hash-ref desc 'seller))
    (define quote-request
      (Observe (Struct 'BookQuoteT (list String ⋆))))
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
                  (hash)))
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
       (set-union agg (Body->effects b)))]
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
       [(list (== Int) (== Int))
        #t]
       [(list (== String) (== String))
        #t]
       [(list (== Bool) (== Bool))
        #t]
       [(list (U τs) _)
        (for/and ([τ (in-list τs)])
          (<:? τ τ2))]
       [(list _ (U τs))
        (for/or ([τ (in-list τs)])
          (<:? τ1 τ))]
       [(list (Observe τ11) (Observe τ22))
        (<:? τ11 τ22)]
       [(list (Struct nm1 τs1) (Struct nm2 τs2))
        (and (equal? nm1 nm2)
             (= (length τs1) (length τs2))
             (for/and ([τ11 (in-list τs1)]
                       [τ22 (in-list τs2)])
               (<:? τ11 τ22)))]
       [_
        #f])]))

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
    (define edges
      (for/list ([(sn st) (in-hash st#)])
        (define dot-name (state-name->dot-name sn))
        (define txns (state-transitions st))
        (define dot-edges
          (for*/list ([(D targets) (in-hash txns)]
                      [target (in-set targets)])
            (render-edge dot-name D target)))
        (string-join dot-edges "\n")))
    (string-join edges
                 "\n"
                 #:before-first (format "digraph ~a {\n" graph-name)
                 #:after-last "\n}"))

  ;; RoleGraph PathString -> DotString
  ;; Like render but write the output to a file
  (define (render-to-file rg dest
                          #:name [name #f])
    (with-output-to-file dest
      (lambda () (write-string (render rg)))
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
       (string-append "-" (τ->string τ))]))

  ;;   - (U (Listof τ))
  ;;   - (Struct StructName (Listof τ ...))
  ;;   - (Observe τ)
  ;;   - ⋆
  ;;   - Int
  ;;   - String
  ;; τ -> String
  (define (τ->string τ)
    (match τ
      [(== Int) "Int"]
      [(== String) "String"]
      [(== Bool) "Bool"]
      [(== ⋆) "⋆"]
      [(Observe τ2)
       (string-append "?" (τ->string τ2))]
      [(Struct nm τs)
       (define slots (string-join (map τ->string τs) " "))
       (string-append "("
                      (~a nm)
                      (if (empty? slots) "" " ")
                      slots
                      ")")]
      [(U τs)
       (define slots (string-join (map τ->string τs) " "))
       (string-append "(U"
                      slots
                      ")")]))
  )
