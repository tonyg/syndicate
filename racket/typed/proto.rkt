#lang racket

(require (only-in racket/hash hash-union))

(module+ test
  (require rackunit))

;; a T is one of
;;   - (Role FacetName (Listof EP)), also abbreviated as just Role
;;   - (Spawn τ)
;;   - (Stop FacetName (Listof T))
(struct Role (nm eps) #:transparent)
(struct Spawn (ty) #:transparent)
(struct Stop (nm tys) #:transparent)

;; a FacetName is a symbol

;; a EP is one of
;;   - (Reacts D (Listof T)), describing an event handler
;;   - (Shares τ), describing an assertion
(struct Reacts (evt body) #:transparent)
(struct Shares (ty) #:transparent)

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
                 (list
                  (Role 'fulfill
                        (list (Shares (Struct 'BookQuoteT (list String Int))))))))))

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
                 (list
                  (Role 'poll
                        (list
                         ;; how to express two possible reactions to the same assertion?
                         (Reacts (Know (book-interest String String Bool))
                                 (list
                                  (Stop 'leader
                                        (list
                                         (Role 'announce
                                               (list
                                                (Shares (Struct 'book-of-the-month String))))))))
                         (Reacts (Know (book-interest String String Bool))
                                 (list
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

;; a State is a (state StateName (Hashof D StateName))
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

;; Role -> (Hashof StateName State)
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
                       #:combine append)))
       (define transitions
         (for/hash ([(D effs) (in-hash agg-txn)])
           (values D (apply-effects effs current ft))))
       (define new-work
         (for/list ([st (in-hash-values transitions)]
                    #:unless (hash-has-key? states st))
           st))
       (loop (append more new-work)
             (hash-set states current (state current transitions)))]
      ['()
       states])))

(module+ test
  (test-case
      "compile seller"
    (define seller# (compile seller))
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
                  (set 'seller 'fulfill))))

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
              (match-define (Reacts _ acts) ep)
              (map ((curry cons) nm) acts)))
          (loop (apply append rest more-work*)
                downs3
                ups2)]
         [(Stop target ks)
          (define new-parent (hash-ref ups target))
          (define more-work
            (for/list ([k (in-list ks)])
              (cons new-parent k)))
          (loop (append rest more-work)
                downs
                ups)])])))

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

;; a TransitionDesc is a (Hashof D (Listof RoleEffect)), describing when
;; transitions occur (+/- of an assertion) and how they alter the facet tree.

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
  (let loop ([roles (list role)]
             [desc (hash)])
    (match roles
      [(cons role roles)
       (match-define (Role nm eps) role)
       (define txn (describe-role role))
       (define next-desc (hash-set desc nm txn))
       (define acts
         (for*/list ([ep (in-list eps)]
                     #:when (Reacts? ep)
                     [body (in-value (Reacts-body ep))]
                     [act (in-list body)])
           act))
       ;; need to find references to Roles inside arbitrarily nested Stops
       (let search ([acts acts]
                    [more-roles roles])
         (match acts
           ['()
            (loop more-roles next-desc)]
           [(cons act acts)
            (match act
              [(Role _ _)
               (search acts (cons act more-roles))]
              [(Stop _ more-acts)
               (search (append acts more-acts) more-roles)]
              [_
               (search acts more-roles)])]))]
      ['()
       desc])))

;; Role -> TransitionDesc
;; determine how the event handlers in a role alter the facet tree
(define (describe-role role)
  (match role
    [(Role nm eps)
     (for/fold ([txns (hash)])
               ([ep (in-list eps)]
                #:when (Reacts? ep))
       (match-define (Reacts evt acts) ep)
       (define effects
         (let loop ([acts acts]
                    [effs (list)])
           (match acts
             ['()
              effs]
             [(cons act more)
              (match act
                [(Role nm2 _)
                 (loop more (cons (start nm2) effs))]
                [(Stop nm2 Ts)
                 (loop (append more Ts)
                       (cons (stop nm2) effs))]
                [_
                 (loop more effs)])])))
       (cond
         [(empty? effects)
          txns]
         [else
          (hash-update txns evt ((curry append) effects) '())]))]))

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
                  (list (start 'fulfill))))
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
    (check-true (hash-has-key? poll-txns (Know (book-interest String String Bool))))
    ;; I'm not sure how this should be represented, but this case is definitely wrong:
    (check-false (hash-ref poll-txns (Know (book-interest String String Bool)))
                 (list (stop 'poll) (start 'announce) (stop 'leader)))
    ))

;; ---------------------------------------------------------------------------
;; Visualization

(module+ vis
  ;; TODO - for now, assume there are no names that need escaping

  ;; (Hashof StateName State) -> DotString
  ;; name is an optional string
  ;; translate the states to DOT that can be passed to graphviz
  (define (render st#
                  #:name [name #f])
    (define graph-name (or name "Roles"))
    (define edges
      (for/list ([(sn st) (in-hash st#)])
        (define dot-name (state-name->dot-name sn))
        (define txns (state-transitions st))
        (define dot-edges
          (for/list ([(D target) (in-hash txns)])
            (render-edge dot-name D target)))
        (string-join dot-edges "\n")))
    (string-join edges
                 "\n"
                 #:before-first (format "digraph ~a {\n" graph-name)
                 #:after-last "\n}"))

  ;; (Hashof StateName State) PathString -> DotString
  ;; Like render but write the output to a file
  (define (render-to-file st# dest
                          #:name [name #f])
    (with-output-to-file dest
      (lambda () (write-string (render st#)))
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
