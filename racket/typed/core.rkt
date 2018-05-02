#lang turnstile

(provide (rename-out [syndicate:#%module-begin #%module-begin])
         (rename-out [typed-app #%app])
         (rename-out [syndicate:begin-for-declarations declare-types])
         #%top-interaction
         require only-in
         ;; Types
         Int Bool String Tuple Bind Discard → ★
         Observe Inbound Outbound Actor U
         ;; Statements
         actor dataspace unsafe-do
         tuple λ observe inbound outbound
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
         print-type
         (rename-out [printf- printf])
         ;; Extensions
         )

(require (rename-in racket/match [match-lambda match-lambda-]))
(require (rename-in racket/math [exact-truncate exact-truncate-]))
(require (prefix-in syndicate: syndicate/core-lang))

(module+ test
  (require rackunit)
  (require turnstile/rackunit-typechecking))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(define-base-types Int Bool String Discard ★ FacetName)
(define-type-constructor Bind #:arity = 1)
(define-type-constructor Tuple #:arity >= 0)
(define-type-constructor U #:arity >= 0)
(define-type-constructor → #:arity > 0)
(define-type-constructor Observe #:arity = 1)
(define-type-constructor Inbound #:arity = 1)
(define-type-constructor Outbound #:arity = 1)
(define-type-constructor Actor #:arity = 1)

(define-for-syntax (type-eval t)
  ((current-type-eval) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Defined Types, aka Constructors

;; τ.norm in 1st case causes "not valid type" error when file is compiled
;; (copied from ext-stlc example)
(define-syntax define-type-alias
  (syntax-parser
    [(_ alias:id τ:any-type)
     #'(define-syntax- alias
         (make-variable-like-transformer #'τ.norm))]
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
           [⊢ e ≫ e- (⇒ : τ)] (... ...)
           ----------------------
           [⊢ (#%app- StructName e- (... ...)) (⇒ : (TypeCons τ (... ...)))])
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
;; Syntax

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
;; Subtyping

;; Type Type -> Bool
(define-for-syntax (<: t1 t2)
  #;(printf "Checking ~a <: ~a\n" (type->str t1) (type->str t2))
  ;; should add a check for type=?
  (syntax-parse #`(#,t1 #,t2)
    #;[(τ1 τ2) #:do [(displayln (type->str #'τ1))
                     (displayln (type->str #'τ2))]
               #:when #f
               (error "")]
    [((~U τ1 ...) _)
     (stx-andmap (lambda (t) (<: t t2)) #'(τ1 ...))]
    [(_ (~U τ2:type ...))
     (stx-ormap (lambda (t) (<: t1 t)) #'(τ2 ...))]
    [((~Actor τ1:type) (~Actor τ2:type))
     ;; should these be .norm? Is the invariant that inputs are always fully
     ;; evalutated/expanded?
     (and (<: #'τ1 #'τ2)
          (<: (∩ (strip-? #'τ1) #'τ2) #'τ1))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
     (stx-andmap <: #'(τ1 ...) #'(τ2 ...))]
    [(_ ~★)
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
    [((~Bind τ1) (~Bind τ2))
     (<: #'τ1 #'τ2)]
    ;; should probably put this first.
    [_ (type=? t1 t2)]))

;; Flat-Type Flat-Type -> Type
(define-for-syntax (∩ t1 t2)
  (unless (and (flat-type? t1) (flat-type? t2))
    (error '∩ "expected two flat-types"))
  (syntax-parse #`(#,t1 #,t2)
    [(_ ~★)
     t1]
    [(~★ _)
     t2]
    [(_ _)
     #:when (type=? t1 t2)
     t1]
    [((~U τ1:type ...) _)
     (type-eval #`(U #,@(stx-map (lambda (t) (∩ t t2)) #'(τ1 ...))))]
    [(_ (~U τ2:type ...))
     (type-eval #`(U #,@(stx-map (lambda (t) (∩ t1 t)) #'(τ2 ...))))]
    ;; all of these fail-when/unless clauses are meant to cause this through to
    ;; the last case and result in ⊥.
    ;; Also, using <: is OK, even though <: refers to ∩, because <:'s use of ∩ is only
    ;; in the Actor case.
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:fail-unless (stx-length=? #'(τ1 ...) #'(τ2 ...)) #f
     #:with (τ ...) (stx-map ∩ #'(τ1 ...) #'(τ2 ...))
     ;; I don't think stx-ormap is part of the documented api of turnstile *shrug*
     #:fail-when (stx-ormap (lambda (t) (<: t (type-eval #'(U)))) #'(τ ...)) #f
     (type-eval #'(Tuple τ ...))]
    [((~constructor-type tag1 τ1:type ...) (~constructor-type tag2 τ2:type ...))
     #:when (tags-equal? #'tag1 #'tag2)
     #:with (τ ...) (stx-map ∩ #'(τ1 ...) #'(τ2 ...))
     #:fail-when (stx-ormap (lambda (t) (<: t (type-eval #'(U)))) #'(τ ...)) #f
     (make-cons-type t1 #'(τ ...))]
    ;; these three are just the same :(
    [((~Observe τ1:type) (~Observe τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ (type-eval #'(U))) #f
     (type-eval #'(Observe τ))]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ (type-eval #'(U))) #f
     (type-eval #'(Inbound τ))]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     #:with τ (∩ #'τ1 #'τ2)
     #:fail-when (<: #'τ (type-eval #'(U))) #f
     (type-eval #'(Outbound τ))]
    [_ (type-eval #'(U))]))

;; Type Type -> Bool
;; first type is the contents of the set
;; second type is the type of a pattern
(define-for-syntax (project-safe? t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [(_ (~Bind τ2:type))
     (and (finite? t1) (<: t1 #'τ2))]
    [(_ ~Discard)
     #t]
    [((~U τ1:type ...) _)
     (stx-andmap (lambda (t) (project-safe? t t2)) #'(τ1 ...))]
    [(_ (~U τ2:type ...))
     (stx-andmap (lambda (t) (project-safe? t1 t)) #'(τ2 ...))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:when (overlap? t1 t2)
     (stx-andmap project-safe? #'(τ1 ...) #'(τ2 ...))]
    [((~constructor-type t1 τ1:type ...) (~constructor-type t2 τ2:type ...))
     #:when (tags-equal? #'t1 #'t2)
     (stx-andmap project-safe? #'(τ1 ...) #'(τ2 ...))]
    [((~Observe τ1:type) (~Observe τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (project-safe? #'τ1 #'τ2)]
    [_ #t]))

;; AssertionType PatternType -> Bool
;; Is it possible for things of these two types to match each other?
;; Flattish-Type = Flat-Types + ★, Bind, Discard (assertion and pattern types)
(define-for-syntax (overlap? t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [(~★ _) #t]
    [(_ (~Bind _)) #t]
    [(_ ~Discard) #t]
    [((~U τ1:type ...) _)
     (stx-ormap (lambda (t) (overlap? t t2)) #'(τ1 ...))]
    [(_ (~U τ2:type ...))
     (stx-ormap (lambda (t) (overlap? t1 t)) #'(τ2 ...))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     (and (stx-length=? #'(τ1 ...) #'(τ2 ...))
          (stx-andmap overlap? #'(τ1 ...) #'(τ2 ...)))]
    [((~constructor-type t1 τ1:type ...) (~constructor-type t2 τ2:type ...))
     (and (tags-equal? #'t1 #'t2)
          (stx-andmap overlap? #'(τ1 ...) #'(τ2 ...)))]
    [((~Observe τ1:type) (~Observe τ2:type))
     (overlap? #'τ1 #'τ2)]
    [((~Inbound τ1:type) (~Inbound τ2:type))
     (overlap? #'τ1 #'τ2)]
    [((~Outbound τ1:type) (~Outbound τ2:type))
     (overlap? #'τ1 #'τ2)]
    [_ (<: t1 t2)]))
    

;; Flattish-Type -> Bool
(define-for-syntax (finite? t)
  (syntax-parse t
    [~★ #f]
    [(~U τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    [(~Tuple τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    [(~constructor-type _ τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    [(~Observe τ:type)
     (finite? #'τ)]
    [(~Inbound τ:type)
     (finite? #'τ)]
    [(~Outbound τ:type)
     (finite? #'τ)]
    [_ #t]))

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; MODIFYING GLOBAL TYPECHECKING STATE!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(begin-for-syntax
  (current-typecheck-relation <:))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statements

(define-typed-syntax (dataspace τ-c:type s ...) ≫
  ;; #:do [(printf "τ-c: ~a\n" (type->str #'τ-c.norm))]
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ : τ-s:type)] ...
  ;; #:do [(printf "dataspace types: ~a\n" (stx-map type->str #'(τ-s.norm ...)))
  ;;      (printf "dataspace type: ~a\n" (type->str ((current-type-eval) #'(Actor τ-c.norm))))]
  #:fail-unless (stx-andmap (lambda (t) (<: t
                                            (type-eval #'(Actor τ-c.norm))))
                            #'(τ-s.norm ...))
                "Not all actors conform to communication type"
  #:with τ-ds-i (strip-inbound #'τ-c.norm)
  #:with τ-ds-o (strip-outbound #'τ-c.norm)
  #:with τ-relay (relay-interests #'τ-c.norm)
  -----------------------------------------------------------------------------------
  [⊢ (syndicate:dataspace s- ...) (⇒ : (U τ-ds-i τ-ds-o τ-relay))])

(define-for-syntax (strip-? t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★ #'★]
     [(~Observe τ) #'τ]
     [_ #'(U)])))

(define-for-syntax (strip-inbound t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★ #'★]
     [(~Inbound τ) #'τ]
     [_ #'(U)])))

(define-for-syntax (strip-outbound t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★ #'★]
     [(~Outbound τ) #'τ]
     [_ #'(U)])))

(define-for-syntax (relay-interests t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★ #'★]
     [(~Observe (~Inbound τ)) #'(Observe τ)]
     [_ #'(U)])))

(define-typed-syntax (actor τ-c:type beh st0 as0) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ beh ≫ beh- ⇒ τ-b:type]
  [⊢ st0 ≫ st0- ⇒ τ-s:type]
  [⊢ as0 ≫ as0- ⇒ τ-a:type]
  --------------------------------------------------------------------------------------------
  [⊢ (syndicate:actor beh- st0- as0-) ⇒ (Actor τ-c)])

(define-for-syntax (procedure-type? τ)
  (syntax-parse τ
    [(~→ τ ...+) #t]
    [_ #f]))

(define-for-syntax (flat-type? τ)
  (syntax-parse τ
    [(~→ τ ...) #f]
    [_ #t]))

(define-typed-syntax (unsafe-do rkt:expr ...) ≫
  ------------------------
  [⊢ (let- () rkt ...) (⇒ : (U)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions

(define-typed-syntax (tuple e:expr ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  -----------------------
  [⊢ (list 'tuple e- ...) (⇒ : (Tuple τ ...))])

(define-typed-syntax (typed-app e_fn e_arg ...) ≫
  [⊢ e_fn ≫ e_fn- (⇒ : (~→ τ_in:type ... τ_out:type))]
  #:fail-unless (stx-length=? #'[τ_in ...] #'[e_arg ...])
                (num-args-fail-msg #'e_fn #'[τ_in ...] #'[e_arg ...])
  [⊢ e_arg ≫ e_arg- ⇐ τ_in] ...
  ------------------------------------------------------------------------
  [⊢ (#%app- e_fn- e_arg- ...) (⇒ : τ_out)])

;; it would be nice to abstract over these three
(define-typed-syntax (observe e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:observe e-) (⇒ : (Observe τ))])

(define-typed-syntax (inbound e:expr) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:inbound e-) (⇒ : (Inbound τ))])

(define-typed-syntax (outbound e:expr) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:outbound e-) (⇒ : (Outbound τ))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns

(define-typed-syntax (bind x:id τ:type) ≫
  ----------------------------------------
  ;; TODO: at some point put $ back in
  [⊢ (void-) (⇒ : (Bind τ))])

(define-typed-syntax discard
  [_ ≫
   --------------------
   ;; TODO: change void to _
   [⊢ (void-) (⇒ : Discard)]])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives

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
  [⊢ e1 ≫ e1- (⇐ : Int)]
  [⊢ e2 ≫ e2- (⇐ : Int)]
  ------------------------
  [⊢ (exact-truncate- (/- e1- e2-)) (⇒ : Int)])

;; for some reason defining `and` as a prim op doesn't work
(define-typed-syntax (and e ...) ≫
  [⊢ e ≫ e- (⇐ : Bool)] ...
  ------------------------
  [⊢ (and- e- ...) (⇒ : Bool)])

(define-typed-syntax (equal? e1:expr e2:expr) ≫
  [⊢ e1 ≫ e1- (⇒ : τ1:type)]
  #:fail-unless (flat-type? #'τ1.norm)
  (format "equality only available on flat data; got ~a" (type->str #'τ1))
  [⊢ e2 ≫ e2- (⇐ : τ1)]
  ---------------------------------------------------------------------------
  [⊢ (equal?- e1- e2-) (⇒ : Bool)])

(define-typed-syntax (displayln e:expr) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------
  [⊢ (displayln- e-) (⇒ : (U))])
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Values

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
  ----------------
  [⊢ (#%datum- . n) (⇒ : Int)]]
  [(_ . b:boolean) ≫
  ----------------
  [⊢ (#%datum- . b) (⇒ : Bool)]]
  [(_ . s:string) ≫
  ----------------
  [⊢ (#%datum- . s) (⇒ : String)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define-typed-syntax (print-type e) ≫
  [⊢ e ≫ e- ⇒ τ]
  #:do [(displayln (type->str #'τ))]
  ----------------------------------
  [⊢ e- ⇒ τ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extensions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests