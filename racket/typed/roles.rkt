#lang turnstile

(provide (rename-out [syndicate:#%module-begin #%module-begin])
         (rename-out [typed-app #%app])
         #%top-interaction
         require only-in
         ;; Types
         Int Bool String Tuple Bind Discard → List
         Role Reacts Reaction Shares Know ¬Know Message
         FacetName Field ★/t
         Observe Inbound Outbound Actor U
         ;; Statements
         #;let spawn #;dataspace start-facet set! #;begin #;stop #;unsafe-do
         ;; endpoints
         assert on
         ;; expressions
         tuple #;λ ref observe inbound outbound
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         + - * / and or not > < >= <= = equal? displayln
         ;; making types
         define-type-alias
         define-constructor
         ;; DEBUG and utilities
         print-type print-role
         (rename-out [printf- printf])
         ;; Extensions
         ;; match if cond
         )

(require (prefix-in syndicate: syndicate/actor-lang))

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx))
(require (for-syntax turnstile/examples/util/filter-maximal))
(require macrotypes/postfix-in)
(require (rename-in racket/math [exact-truncate exact-truncate-]))
(require (postfix-in - racket/list))
(require (postfix-in - racket/set))

(module+ test
  (require rackunit)
  (require turnstile/rackunit-typechecking))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checking Conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; : describes the immediate result of evaluation
;; a key aggregates `assert` endpoints
;; r key aggregates each `on` endpoint as a `Reaction`
;; e key aggregates effects, such as starting a facet


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-binding-type Role #:arity >= 2 #:bvs = 1)
(define-type-constructor Shares #:arity = 1)
(define-type-constructor Reacts #:arity >= 0)
(define-type-constructor Reaction #:arity >= 2)
(define-type-constructor Know #:arity = 1)
(define-type-constructor ¬Know #:arity = 1)
(define-type-constructor Message #:arity = 1)
(define-type-constructor Field #:arity = 1)
(define-type-constructor Bind #:arity = 1)

(define-type-constructor → #:arity > 0)
(define-type-constructor Tuple #:arity >= 0)
(define-type-constructor Observe #:arity = 1)
(define-type-constructor Inbound #:arity = 1)
(define-type-constructor Outbound #:arity = 1)
(define-type-constructor Actor #:arity = 1)
(define-type-constructor AssertionSet #:arity = 1)
(define-type-constructor Patch #:arity = 2)
(define-type-constructor List #:arity = 1)
(define-type-constructor Set #:arity = 1)

(define-base-types Int Bool String Discard ★/t FacetName)

(define-for-syntax (type-eval t)
  ((current-type-eval) t))

(define-type-constructor U* #:arity >= 0)

(define-for-syntax (prune+sort tys)
  (stx-sort 
   (filter-maximal 
    (stx->list tys)
    typecheck?)))
  
(define-syntax (U stx)
  (syntax-parse stx
    [(_ . tys)
     ;; canonicalize by expanding to U*, with only (sorted and pruned) leaf tys
     #:with ((~or (~U* ty1- ...) ty2-) ...) (stx-map (current-type-eval) #'tys)
     #:with tys- (prune+sort #'(ty1- ... ... ty2- ...))
     (if (= 1 (stx-length #'tys-))
         (stx-car #'tys-)
         (syntax/loc stx (U* . tys-)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Defined Types, aka Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; τ.norm in 1st case causes "not valid type" error when referring to ⊥ in another file.
;; however, this version expands the type at every reference, incurring a potentially large
;; overhead---2x in the case of book-club.rkt
;; (copied from ext-stlc example)
(define-syntax define-type-alias
  (syntax-parser
    [(_ alias:id τ)
     #'(define-syntax- alias
         (make-variable-like-transformer #'τ))]
    [(_ (f:id x:id ...) ty)
     #'(define-syntax- (f stx)
         (syntax-parse stx
           [(_ x ...)
            #:with τ:any-type #'ty
            #'τ.norm]))]))

(begin-for-syntax
  (define-splicing-syntax-class type-constructor-decl
    (pattern (~seq #:type-constructor TypeCons:id))
    (pattern (~seq) #:attr TypeCons #f))

  (struct user-ctor (typed-ctor untyped-ctor)
    #:property prop:procedure
    (lambda (v stx)
      (define transformer (user-ctor-typed-ctor v))
      (syntax-parse stx
        [(_ e ...)
         #`(#,transformer e ...)]))))

(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ (Cons:id slot:id ...)
        ty-cons:type-constructor-decl
        (~seq #:with
              Alias AliasBody) ...)
     #:with TypeCons (or (attribute ty-cons.TypeCons) (format-id stx "~a/t" (syntax-e #'Cons)))
     #:with MakeTypeCons (format-id #'TypeCons "make-~a" #'TypeCons)
     #:with GetTypeParams (format-id #'TypeCons "get-~a-type-params" #'TypeCons)
     #:with TypeConsExpander (format-id #'TypeCons "~~~a" #'TypeCons)
     #:with TypeConsExtraInfo (format-id #'TypeCons "~a-extra-info" #'TypeCons)
     #:with (StructName Cons- type-tag) (generate-temporaries #'(Cons Cons Cons))
     (define arity (stx-length #'(slot ...)))
     #`(begin-
         (struct- StructName (slot ...) #:reflection-name 'Cons #:transparent)
         (define-syntax (TypeConsExtraInfo stx)
           (syntax-parse stx
             [(_ X (... ...)) #'('type-tag 'MakeTypeCons 'GetTypeParams)]))
         (define-type-constructor TypeCons
           #:arity = #,arity
           #:extra-info 'TypeConsExtraInfo)
         (define-syntax (MakeTypeCons stx)
           (syntax-parse stx
             [(_ t (... ...))
              #:fail-unless (= #,arity (stx-length #'(t (... ...)))) "arity mismatch"
              #'(TypeCons t (... ...))]))
         (define-syntax (GetTypeParams stx)
           (syntax-parse stx
             [(_ (TypeConsExpander t (... ...)))
              #'(t (... ...))]))
         (define-syntax Cons
           (user-ctor #'Cons- #'StructName))
         (define-typed-syntax (Cons- e (... ...)) ≫
           #:fail-unless (= #,arity (stx-length #'(e (... ...)))) "arity mismatch"
           [⊢ e ≫ e- (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())] (... ...)
           ----------------------
           [⊢ (#%app- StructName e- (... ...)) (⇒ : (TypeCons τ (... ...)))
              (⇒ a ()) (⇒ r ()) (⇒ e ())])
         (define-type-alias Alias AliasBody) ...)]))

(begin-for-syntax
  (define-syntax ~constructor-extra-info
    (pattern-expander
     (syntax-parser
       [(_ tag mk get)
        #'(_ (_ tag) (_ mk) (_ get))])))

  (define-syntax ~constructor-type
    (pattern-expander
     (syntax-parser
       [(_ tag . rst)
        #'(~and it
                (~fail #:unless (user-defined-type? #'it))
                (~parse tag (get-type-tag #'it))
                (~Any _ . rst))])))

  (define-syntax ~constructor-exp
    (pattern-expander
     (syntax-parser
       [(_ cons . rst)
        #'(~and (cons . rst)
                (~fail #:unless (ctor-id? #'cons)))])))

  (define (inspect t)
    (syntax-parse t
      [(~constructor-type tag t ...)
       (list (syntax-e #'tag) (stx-map type->str #'(t ...)))]))

  (define (tags-equal? t1 t2)
    (equal? (syntax-e t1) (syntax-e t2)))
    
  (define (user-defined-type? t)
    (get-extra-info (type-eval t)))

  (define (get-type-tag t)
    (syntax-parse (get-extra-info t)
      [(~constructor-extra-info tag _ _)
       (syntax-e #'tag)]))

  (define (get-type-args t)
    (syntax-parse (get-extra-info t)
      [(~constructor-extra-info _ _ get)
       (define f (syntax-local-value #'get))
       (syntax->list (f #`(get #,t)))]))

  (define (make-cons-type t args)
    (syntax-parse (get-extra-info t)
      [(~constructor-extra-info _ mk _)
       (define f (syntax-local-value #'mk))
        (type-eval (f #`(mk #,@args)))]))

  (define (ctor-id? stx)
    (and (identifier? stx)
         (user-ctor? (syntax-local-value stx (const #f)))))

  (define (untyped-ctor stx)
    (user-ctor-untyped-ctor (syntax-local-value stx (const #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conveniences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax

  ;; constructors with arity one
  (define-syntax-class kons1
    (pattern (~or (~datum observe)
                  (~datum inbound)
                  (~datum outbound))))

  (define (kons1->constructor stx)
    (syntax-parse stx
      #:datum-literals (observe inbound outbound)
      [observe #'syndicate:observe]
      [inbound #'syndicate:inbound]
      [outbound #'syndicate:outbound]))

  (define-syntax-class basic-val
    (pattern (~or boolean
                  integer
                  string)))

  (define-syntax-class prim-op
    (pattern (~or (~literal +)
                  (~literal -)
                  (~literal displayln)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities Over Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (bot? t)
  (<: t (type-eval #'(U*))))

(define-for-syntax (flat-type? τ)
  (syntax-parse τ
    [(~→ τ ...) #f]
    [(~Actor τ) #f]
    [_ #t]))

(define-for-syntax (strip-? t)
  (type-eval
   (syntax-parse t
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     [(~Observe τ) #'τ]
     [_ #'(U*)])))

(define-for-syntax (strip-inbound t)
  (type-eval
   (syntax-parse t
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     [(~Inbound τ) #'τ]
     [_ #'(U*)])))

(define-for-syntax (strip-outbound t)
  (type-eval
   (syntax-parse t
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     [(~Outbound τ) #'τ]
     [_ #'(U*)])))

(define-for-syntax (relay-interests t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     [(~Observe (~Inbound τ)) #'(Observe τ)]
     [_ #'(U*)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Type Type -> Bool
(define-for-syntax (<: t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [((~U* τ1 ...) _)
     (stx-andmap (lambda (t) (<: t t2)) #'(τ1 ...))]
    [(_ (~U* τ2:type ...))
     (stx-ormap (lambda (t) (<: t1 t)) #'(τ2 ...))]
    ;; TODO
    #;[((~Actor τ1:type) (~Actor τ2:type))
     ;; should these be .norm? Is the invariant that inputs are always fully
     ;; evalutated/expanded?
     (and (<: #'τ1 #'τ2)
          (<: (∩ (strip-? #'τ1) #'τ2) #'τ1))]
    [((~AssertionSet τ1) (~AssertionSet τ2))
     (<: #'τ1 #'τ2)]
    [((~Set τ1) (~Set τ2))
     (<: #'τ1 #'τ2)]
    [((~Patch τ11 τ12) (~Patch τ21 τ22))
     (and (<: #'τ11 #'τ21)
          (<: #'τ12 #'τ22))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
     (stx-andmap <: #'(τ1 ...) #'(τ2 ...))]
    [(_ ~★/t)
     (flat-type? t1)]
    [((~Observe τ1:type) (~Observe τ2:type))
     (<: #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (<: #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (<: #'τ1 #'τ2)]
    [((~constructor-type t1 τ1:type ...) (~constructor-type t2 τ2:type ...))
     #:when (tags-equal? #'t1 #'t2)
     (and (stx-length=? #'(τ1 ...) #'(τ2 ...))
          (stx-andmap <: #'(τ1 ...) #'(τ2 ...)))]
    [((~→ τ-in1 ... τ-out1) (~→ τ-in2 ... τ-out2))
     #:when (stx-length=? #'(τ-in1 ...) #'(τ-in2 ...))
     (and (stx-andmap <: #'(τ-in2 ...) #'(τ-in1 ...))
          (<: #'τ-out1 #'τ-out2))]
    [(~Discard _)
     #t]
    ;; TODO: clauses for Roles, and so on
    ;; should probably put this first.
    [_ (type=? t1 t2)]))

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; MODIFYING GLOBAL TYPECHECKING STATE!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(begin-for-syntax
  (current-typecheck-relation <:))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core forms

(define-typed-syntax (start-facet name:id ((~datum fields) [x:id τ-f:type e:expr] ...) ep ...+) ≫
  #:fail-unless (stx-andmap flat-type? #'(τ-f ...)) "keep your uppity data outta my fields"
  ;; TODO - probably don't want these expressions to have any effects
  [⊢ e ≫ e- (⇐ : τ-f)] ...
  [[name ≫ name- : FacetName] [x ≫ x- : (Field τ-f.norm)] ...
   ⊢ [ep ≫ ep- (⇒ r (τ-r ...))
                (⇒ a (τ-a ...))
                (⇒ e (τ-e ...))] ...]
  #:with as (type-eval #'(U τ-a ... ...))
  #:with τ #'(Role (name-)
                   (Shares as)
                   (Reacts τ-r ... ...)
                   τ-e ... ...)
  --------------------------------------------------------------
  [⊢ (syndicate:react (let- ([name- (syndicate:current-facet-id)])
                            #,(make-fields #'(x- ...) #'(e- ...))
                            ep- ...))
     (⇒ : ★/t)
     (⇒ r ())
     (⇒ a ())
     (⇒ e (τ))])

(define-for-syntax (make-fields names inits)
  (syntax-parse #`(#,names #,inits)
    [((x:id ...) (e ...))
     #'(syndicate:field [x e] ...)]))

(define-typed-syntax (assert e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  -------------------------------------
  [⊢ (syndicate:assert e-) (⇒ : ★/t)
                           (⇒ a (τ))
                           (⇒ r ())
                           (⇒ e ())])

(begin-for-syntax
  (define-syntax-class asserted-or-retracted
    #:datum-literals (asserted retracted)
    (pattern (~or (~and asserted
                        (~bind [syndicate-kw #'syndicate:asserted]
                               [react-con #'Know]))
                  (~and retracted
                        (~bind [syndicate-kw #'syndicate:retracted]
                               [react-con #'¬Know]))))))

(define-typed-syntax on
  ;; TODO - on start/stop
  #;[(on (~literal start) s) ≫
   [⊢ s ≫ s- (⇒ :i τi) (⇒ :o τ-o) (⇒ :a τ-a)]
   -----------------------------------
   [⊢ (syndicate:on-start s-) (⇒ : (U)) (⇒ :i τi) (⇒ :o τ-o) (⇒ :a τ-a)]]
  #;[(on (~literal stop) s) ≫
   [⊢ s ≫ s- (⇒ :i τi) (⇒ :o τ-o) (⇒ :a τ-a)]
   -----------------------------------
   [⊢ (syndicate:on-stop s-) (⇒ : (U)) (⇒ :i τi) (⇒ :o τ-o) (⇒ :a τ-a)]]
  [(on (a/r:asserted-or-retracted p) s) ≫
   [⊢ p ≫ _ (⇒ : τp)]
   #:with p- (compile-syndicate-pattern #'p)
   #:with ([x:id τ:type] ...) (pat-bindings #'p)
   [[x ≫ x- : τ a (U*) r (U*) e (U*)] ... ⊢ s ≫ s-  (⇒ a τ-as) (⇒ r τ-rs) (⇒ e (τ-e ...))]
   #:do [(displayln (stx-map type->str #'τ-as))]
   #:do [(displayln (stx-andmap bot? #'τ-as))]
   ;; #:do [(displayln (stx-andmap bot? #'τ-rs))]
   #:fail-unless (and (stx-andmap bot? #'τ-as) (stx-andmap bot? #'τ-rs)) "illegal context"
   #:with (rhs ...) (if (stx-null? #'(τ-e ...)) #'((U*)) #'(τ-e ...))
   #:with τ-r #'(Reaction (a/r.react-con τp) rhs ...)
   -----------------------------------
   [⊢ (syndicate:on (a/r.syndicate-kw p-)
                    (let- ([x- x] ...) s-))
      (⇒ : ★/t)
      (⇒ r (τ-r))
      (⇒ e ())
      (⇒ a ())]])

;; pat -> ([Id Type] ...)
(define-for-syntax (pat-bindings stx)
  (syntax-parse stx
    #:datum-literals (bind tuple)
    [(bind x:id τ:type)
     #'([x τ])]
    [(tuple p ...)
     #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
     #'([x τ] ... ...)]
    [(k:kons1 p)
     (pat-bindings #'p)]
    [(~constructor-exp cons p ...)
     #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
     #'([x τ] ... ...)]
    [_
     #'()]))

(define-for-syntax (compile-pattern pat bind-id-transformer exp-transformer)
  (let loop ([pat pat])
    (syntax-parse pat
      #:datum-literals (tuple discard bind)
      [(tuple p ...)
       #`(list 'tuple #,@(stx-map loop #'(p ...)))]
      [(k:kons1 p)
       #`(#,(kons1->constructor #'k) #,(loop #'p))]
      [(bind x:id τ:type)
       (bind-id-transformer #'x)]
      [discard
       #'_]
      [(~constructor-exp ctor p ...)
       (define/with-syntax uctor (untyped-ctor #'ctor))
       #`(uctor #,@(stx-map loop #'(p ...)))]
      [_
       (exp-transformer pat)])))

(define-for-syntax (compile-syndicate-pattern pat)
  (compile-pattern pat
                   (lambda (id) #`($ #,id))
                   identity))

(define-typed-syntax (spawn τ-c:type s) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ a ()) (⇒ r ()) (⇒ e (τ-e ...))]
  ;; TODO: s shouldn't refer to facets or fields!
  ;; TODO - check the role against the type of the dataspace
  #|
  #:fail-unless (<: #'τ-o.norm #'τ-c.norm)
                (format "Output ~a not valid in dataspace ~a" (type->str #'τ-o.norm) (type->str #'τ-c.norm))
  #:fail-unless (<: (type-eval #'(Actor τ-a.norm))
                    (type-eval #'(Actor τ-c.norm))) "Spawned actors not valid in dataspace"
  #:fail-unless (project-safe? (∩ (strip-? #'τ-o.norm) #'τ-c.norm)
                               #'τ-i.norm) "Not prepared to handle all inputs"
  |#
  --------------------------------------------------------------------------------------------
  ;; TODO - need a key for spawning actors, I guess
  [⊢ (syndicate:spawn (syndicate:on-start s-)) (⇒ : ★/t) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (set! x:id e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  [⊢ x ≫ x- (⇒ : (~Field τ-x:type))]
  #:fail-unless (<: #'τ #'τ-x) "Ill-typed field write"
  ----------------------------------------------------
  [⊢ (x- e-) (⇒ : ★/t) (⇒ a ()) (⇒ r ()) (⇒ e ())])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (ref x:id) ≫
  [⊢ x ≫ x- ⇒ (~Field τ)]
  ------------------------
  [⊢ (x-) (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (typed-app e_fn e_arg ...) ≫
  ;; TODO : other keys
  [⊢ e_fn ≫ e_fn- (⇒ : (~→ τ_in ... τ_out))]
  #:fail-unless (stx-length=? #'[τ_in ...] #'[e_arg ...])
                (num-args-fail-msg #'e_fn #'[τ_in ...] #'[e_arg ...])
  [⊢ e_arg ≫ e_arg- (⇐ : τ_in)] ...
  ------------------------------------------------------------------------
  [⊢ (#%app- e_fn- e_arg- ...) (⇒ : τ_out)
                               (⇒ a ())
                               (⇒ r ())
                               (⇒ e ())])

(define-typed-syntax (tuple e:expr ...) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())] ...
  -----------------------
  [⊢ (list- 'tuple e- ...) (⇒ : (Tuple τ ...))
                           (⇒ a ())
                           (⇒ r ())
                           (⇒ e ())])

(define-typed-syntax (select n:nat e:expr) ≫
  [⊢ e ≫ e- (⇒ : (~Tuple τ ...)) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  #:do [(define i (syntax->datum #'n))]
  #:fail-unless (< i (stx-length #'(τ ...))) "index out of range"
  #:with τr (list-ref (stx->list #'(τ ...)) i)
  --------------------------------------------------------------
  [⊢ (tuple-select n e-) (⇒ : τr) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define- (tuple-select n t)
  (list-ref- t (add1 n)))

;; it would be nice to abstract over these three
(define-typed-syntax (observe e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:observe e-) (⇒ : (Observe τ)) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (inbound e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:inbound e-) (⇒ : (Inbound τ)) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (outbound e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:outbound e-) (⇒ : (Outbound τ)) (⇒ a ()) (⇒ r ()) (⇒ e ())])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (bind x:id τ:type) ≫
  ----------------------------------------
  [⊢ (error- 'bind "escaped") (⇒ : (Bind τ)) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax discard
  [_ ≫
   --------------------
   ;; TODO: change void to _
   [⊢ (error- 'discard "escaped") (⇒ : Discard) (⇒ a ()) (⇒ r ()) (⇒ e ())]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core-ish forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hmmm
(define-primop + (→ Int Int Int))
(define-primop - (→ Int Int Int))
(define-primop * (→ Int Int Int))
#;(define-primop and (→ Bool Bool Bool))
(define-primop or (→ Bool Bool Bool))
(define-primop not (→ Bool Bool))
(define-primop < (→ Int Int Bool))
(define-primop > (→ Int Int Bool))
(define-primop <= (→ Int Int Bool))
(define-primop >= (→ Int Int Bool))
(define-primop = (→ Int Int Bool))

(define-typed-syntax (/ e1 e2) ≫
  [⊢ e1 ≫ e1- (⇐ : Int) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  [⊢ e2 ≫ e2- (⇐ : Int) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  ------------------------
  [⊢ (exact-truncate- (/- e1- e2-)) (⇒ : Int) (⇒ a ()) (⇒ r ()) (⇒ e ())])

;; for some reason defining `and` as a prim op doesn't work
(define-typed-syntax (and e ...) ≫
  [⊢ e ≫ e- (⇐ : Bool) (⇒ a ()) (⇒ r ()) (⇒ e ())] ...
  ------------------------
  [⊢ (and- e- ...) (⇒ : Bool) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (equal? e1:expr e2:expr) ≫
  [⊢ e1 ≫ e1- (⇒ : τ1:type) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  #:fail-unless (flat-type? #'τ1.norm)
  (format "equality only available on flat data; got ~a" (type->str #'τ1))
  [⊢ e2 ≫ e2- (⇐ : τ1) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  ---------------------------------------------------------------------------
  [⊢ (equal?- e1- e2-) (⇒ : Bool) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (empty? e) ≫
  [⊢ e ≫ e- (⇒ : (~List _)) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  -----------------------
  [⊢ (empty?- e-) (⇒ : Bool) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (first e) ≫
  [⊢ e ≫ e- (⇒ : (~List τ)) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  -----------------------
  [⊢ (first- e-) (⇒ : τ) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (rest e) ≫
  [⊢ e ≫ e- (⇒ : (~List τ)) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  -----------------------
  [⊢ (rest- e-) (⇒ : (List τ)) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define-typed-syntax (member? e l) ≫
  [⊢ e ≫ e- (⇒ : τe:type) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  [⊢ l ≫ l- (⇒ : (~List τl:type)) (⇒ a ()) (⇒ r ()) (⇒ e ())]
  #:fail-unless (<: #'τe.norm #'τl.norm) "incompatible list"
  ----------------------------------------
  [⊢ (member?- e- l-) (⇒ : Bool) (⇒ a ()) (⇒ r ()) (⇒ e ())])

(define- (member?- v l)
  (and- (member- v l) #t))

(define-typed-syntax (displayln e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a as) (⇒ r rs) (⇒ e es)]
  ---------------
  [⊢ (displayln- e-) (⇒ : ★/t) (⇒ a as) (⇒ r rs) (⇒ e es)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
  ----------------
  [⊢ (#%datum- . n) (⇒ : Int) (⇒ a ()) (⇒ r ()) (⇒ e ())]]
  [(_ . b:boolean) ≫
  ----------------
  [⊢ (#%datum- . b) (⇒ : Bool) (⇒ a ()) (⇒ r ()) (⇒ e ())]]
  [(_ . s:string) ≫
  ----------------
  [⊢ (#%datum- . s) (⇒ : String) (⇒ a ()) (⇒ r ()) (⇒ e ())]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (print-type e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a as) (⇒ r rs) (⇒ e es)]
  #:do [(displayln (type->str #'τ))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ a as) (⇒ r rs) (⇒ e es)])

(define-typed-syntax (print-role e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ a as) (⇒ r rs) (⇒ e es)]
  #:do [(for ([r (in-syntax #'es)])
          (displayln (type->str r)))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ a as) (⇒ r rs) (⇒ e es)])