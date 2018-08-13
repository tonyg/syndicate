#lang turnstile

(provide (rename-out [syndicate:#%module-begin #%module-begin])
         (rename-out [typed-app #%app])
         #%top-interaction
         require only-in
         ;; Types
         Int Bool String Tuple Bind Discard → List
         Role Reacts Shares Know ¬Know Message Stop
         FacetName Field ★/t
         Observe Inbound Outbound Actor U
         Computation Value Endpoints Roles Spawns
         ;; Statements
         let let* if spawn dataspace start-facet set! begin stop #;unsafe-do
         ;; Derived Forms
         during
         ;; endpoints
         assert on field
         ;; expressions
         tuple lambda ref observe inbound outbound
         ;; making types
         define-type-alias
         define-constructor
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         + - * / and or not > < >= <= = equal? displayln printf define
         ;; lists
         list first rest member? empty? for for/fold
         ;; sets
         Set set set-member? set-add set-count set-union set-subtract set-intersect
         list->set set->list
         ;; DEBUG and utilities
         print-type print-role
         ;; Extensions
         match cond
         )

(require (prefix-in syndicate: syndicate/actor-lang))

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx))
(require (for-syntax turnstile/examples/util/filter-maximal))
(require macrotypes/postfix-in)
(require (rename-in racket/math [exact-truncate exact-truncate-]))
(require (postfix-in - racket/list))
(require (postfix-in - racket/set))
(require (postfix-in - racket/match))

(module+ test
  (require rackunit)
  (require turnstile/rackunit-typechecking))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checking Conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; : describes the immediate result of evaluation
;; ep key aggregates endpoint affects:
;;   `Shares`, `Reacts`, and `MakesField`
;; Note thar MakesField is only an effect, not a type
;; f key aggregates facet effects (starting a facet) as `Role`s
;; s key aggregates spawned actors as `Actor`s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-binding-type Role #:arity >= 0 #:bvs = 1)
(define-type-constructor Shares #:arity = 1)
(define-type-constructor Reacts #:arity >= 1)
(define-type-constructor Know #:arity = 1)
(define-type-constructor ¬Know #:arity = 1)
(define-type-constructor Stop #:arity >= 1)
(define-type-constructor Message #:arity = 1)
(define-type-constructor Field #:arity = 1)
(define-type-constructor Bind #:arity = 1)
(define-base-types OnStart OnStop MakesField)
(define-for-syntax field-prop-name 'fields)


(define-type-constructor Tuple #:arity >= 0)
(define-type-constructor Observe #:arity = 1)
(define-type-constructor Inbound #:arity = 1)
(define-type-constructor Outbound #:arity = 1)
(define-type-constructor Actor #:arity = 1)
(define-type-constructor AssertionSet #:arity = 1)
(define-type-constructor Patch #:arity = 2)
(define-type-constructor List #:arity = 1)
(define-type-constructor Set #:arity = 1)

(define-type-constructor → #:arity > 0)
;; for describing the RHS
;; a value and a description of the effects
(define-type-constructor Computation #:arity = 4)
(define-type-constructor Value #:arity = 1)
(define-type-constructor Endpoints #:arity >= 0)
(define-type-constructor Roles #:arity >= 0)
(define-type-constructor Spawns #:arity >= 0)


(define-base-types Int Bool String Discard ★/t FacetName)

(define-for-syntax (type-eval t)
  ((current-type-eval) t))

(define-type-constructor U* #:arity >= 0)

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

(define-type-alias ⊥ (U*))

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

;; for looking at the "effects"
(begin-for-syntax
  (define-syntax ~effs
    (pattern-expander
     (syntax-parser
       [(_ eff:id ...)
        #:with tmp (generate-temporary 'effss)
        #'(~and tmp
                (~parse (eff ...) (stx-or #'tmp #'())))])))

  (define (stx-truth? a)
    (and a (not (and (syntax? a) (false? (syntax-e a))))))
  (define (stx-or a b)
    (cond [(stx-truth? a) a]
          [else b])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Defined Types, aka Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
           #:fail-unless (all-pure? #'(e- (... ...))) "expressions must be pure"
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

;; (SyntaxOf RoleType ...) -> (Syntaxof InputType OutputType SpawnType)
(define-for-syntax (analyze-roles rs)
  (define-values (lis los lss)
    (for/fold ([is '()]
               [os '()]
               [ss '()])
              ([r (in-syntax rs)])
      (define-values (i o s) (analyze-role-input/output r))
      (values (cons i is) (cons o os) (cons s ss))))
  #`(#,(type-eval #`(U #,@lis))
     #,(type-eval #`(U #,@los))
     #,(type-eval #`(U #,@lss))))

;; Wanted test case, but can't use it bc it uses things defined for-syntax
#;(module+ test
 (let ([r (type-eval #'(Role (x) (Shares Int)))])
   (syntax-parse (analyze-role-input/output r)
     [(τ-i τ-o)
      (check-true (type=? #'τ-o (type-eval #'Int)))])))

;; RoleType -> (Values InputType OutputType SpawnType)
(define-for-syntax (analyze-role-input/output t)
  (syntax-parse t
    [(~Stop name:id τ-r ...)
     #:with (τi τo τa) (analyze-roles #'(τ-r ...))
     (values #'τi #'τo #'τa)]
    [(~Actor τc)
     (values (mk-U*- '()) (mk-U*- '()) t)]
    [(~Role (name:id)
       (~or (~Shares τ-s)
            (~Reacts τ-if τ-then ...)) ...
       (~and (~Role _ ...) sub-role) ...)
     (define-values (is os ss)
       (for/fold ([ins '()]
                  [outs '()]
                  [spawns '()])
                 ([t (in-syntax #'(τ-then ... ... sub-role ...))])
         (define-values (i o s) (analyze-role-input/output t))
         (values (cons i ins) (cons o outs) (cons s spawns))))
     (define pat-types (stx-map event-desc-type #'(τ-if ...)))
     (values (type-eval #`(U #,@is #,@pat-types))
             (type-eval #`(U τ-s ... #,@os #,@(stx-map pattern-sub-type pat-types)))
             (type-eval #`(U #,@ss)))]))

;; EventDescriptorType -> Type
(define-for-syntax (event-desc-type desc)
  (syntax-parse desc
    [(~Know τ) #'τ]
    [(~¬Know τ) #'τ]
    [(~Message τ) desc]
    [_ (type-eval #'(U*))]))

;; PatternType -> Type
(define-for-syntax (pattern-sub-type pt)
  (define t (replace-bind-and-discard-with-★ pt))
  (type-eval #`(Observe #,t)))

(define-for-syntax (replace-bind-and-discard-with-★ t)
  (syntax-parse t
    [(~Bind _)
     (type-eval #'★/t)]
    [~Discard
     (type-eval #'★/t)]
    [(~U* τ ...)
     (type-eval #`(U #,@(stx-map replace-bind-and-discard-with-★ #'(τ ...))))]
    [(~Tuple τ ...)
     (type-eval #`(Tuple #,@(stx-map replace-bind-and-discard-with-★ #'(τ ...))))]
    [(~Observe τ)
     (type-eval #`(Observe #,(replace-bind-and-discard-with-★ #'τ)))]
    [(~Inbound τ)
     (type-eval #`(Inbound #,(replace-bind-and-discard-with-★ #'τ)))]
    [(~Outbound τ)
     (type-eval #`(Outbound #,(replace-bind-and-discard-with-★ #'τ)))]
    [(~constructor-type _ τ ...)
     (make-cons-type t (stx-map replace-bind-and-discard-with-★ #'(τ ...)))]
    [_ t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping and Judgments on Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Type Type -> Bool
(define-for-syntax (<: t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [((~U* τ1 ...) _)
     (stx-andmap (lambda (t) (<: t t2)) #'(τ1 ...))]
    [(_ (~U* τ2:type ...))
     (stx-ormap (lambda (t) (<: t1 t)) #'(τ2 ...))]
    [((~Actor τ1) (~Actor τ2))
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

;; Flat-Type Flat-Type -> Type
(define-for-syntax (∩ t1 t2)
  (unless (and (flat-type? t1) (flat-type? t2))
    (error '∩ "expected two flat-types"))
  (syntax-parse #`(#,t1 #,t2)
    [(_ ~★/t)
     t1]
    [(~★/t _)
     t2]
    [(_ _)
     #:when (type=? t1 t2)
     t1]
    [((~U* τ1:type ...) _)
     (type-eval #`(U #,@(stx-map (lambda (t) (∩ t t2)) #'(τ1 ...))))]
    [(_ (~U* τ2:type ...))
     (type-eval #`(U #,@(stx-map (lambda (t) (∩ t1 t)) #'(τ2 ...))))]
    [((~AssertionSet τ1) (~AssertionSet τ2))
     #:with τ12 (∩ #'τ1 #'τ2)
     (type-eval #'(AssertionSet τ12))]
    [((~Set τ1) (~Set τ2))
     #:with τ12 (∩ #'τ1 #'τ2)
     (type-eval #'(Set τ12))]
    [((~Patch τ11 τ12) (~Patch τ21 τ22))
     #:with τ1 (∩ #'τ11 #'τ12)
     #:with τ2 (∩ #'τ21 #'τ22)
     (type-eval #'(Patch τ1 τ2))]
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
    [(_ ~★/t)
     #t]
    [((~U* τ1:type ...) _)
     (stx-andmap (lambda (t) (project-safe? t t2)) #'(τ1 ...))]
    [(_ (~U* τ2:type ...))
     (stx-andmap (lambda (t) (project-safe? t1 t)) #'(τ2 ...))]
    [((~Tuple τ1:type ...) (~Tuple τ2:type ...))
     #:when (overlap? t1 t2)
     (stx-andmap project-safe? #'(τ1 ...) #'(τ2 ...))]
    [((~constructor-type _ τ1:type ...) (~constructor-type _ τ2:type ...))
     #:when (overlap? t1 t2)
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
;; Flattish-Type = Flat-Types + ★/t, Bind, Discard (assertion and pattern types)
(define-for-syntax (overlap? t1 t2)
  (syntax-parse #`(#,t1 #,t2)
    [(~★/t _) #t]
    [(_ (~Bind _)) #t]
    [(_ ~Discard) #t]
    [(_ ~★/t) #t]
    [((~U* τ1:type ...) _)
     (stx-ormap (lambda (t) (overlap? t t2)) #'(τ1 ...))]
    [(_ (~U* τ2:type ...))
     (stx-ormap (lambda (t) (overlap? t1 t)) #'(τ2 ...))]
    [((~List _) (~List _))
     ;; share the empty list
     #t]
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
    [~★/t #f]
    [(~U* τ:type ...)
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
    [(~Set τ:type)
     (finite? #'τ)]
    [_ #t]))

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; MODIFYING GLOBAL TYPECHECKING STATE!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(begin-for-syntax
  (current-typecheck-relation <:))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Checking

;; DesugaredSyntax EffectName -> (Syntaxof Effect ...)
(define-for-syntax (get-effect e- eff)
  (or (syntax-property e- eff) #'()))

;; DesugaredSyntax EffectName -> Bool
(define-for-syntax (effect-free? e- eff)
  (define prop (syntax-property e- eff))
  (or (false? prop) (stx-null? prop)))

;; DesugaredSyntax -> Bool
(define-for-syntax (pure? e-)
  (for/and ([key (in-list '(ep f s))])
    (effect-free? e- key)))

;; (SyntaxOf DesugaredSyntax ...) -> Bool
(define-for-syntax (all-pure? es)
  (stx-andmap pure? es))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (int-def-ctx-bind-type-rename x x- t ctx)
  (syntax-local-bind-syntaxes (list x-) #f ctx)
  (syntax-local-bind-syntaxes (list x)
                              #`(make-rename-transformer
                                 (add-orig (assign-type #'#,x- #'#,t #:wrap? #f) #'#,x))
                              ctx))

;; -> (Values e-... (Listof Type) (Listof EndpointEffects) (Listof FacetEffects) (Listof SpawnEffects))
;; recognizes local binding forms
;; (field/intermediate [x e] ...
;; (define/intermediate x x- τ e)
(define-for-syntax (walk/bind e...
                              [def-ctx (syntax-local-make-definition-context)]
                              [unique (gensym 'walk/bind)])
  (define-values (rev-e-... rev-τ... ep-effects facet-effects spawn-effects)
    (for/fold ([rev-e-... '()]
               [rev-τ... '()]
               [ep-effects '()]
               [facet-effects '()]
               [spawn-effects '()])
              ([e (in-syntax e...)])
      (define e- (local-expand e (list unique) (list #'erased) def-ctx))
      (define τ (syntax-property e- ':))
      (define-values (ep-effs f-effs s-effs)
        (values (syntax->list (get-effect e- 'ep))
                (syntax->list (get-effect e- 'f))
                (syntax->list (get-effect e- 's))))
      (syntax-parse e-
        #:literals (erased field/intermediate define/intermediate)
        [(erased (field/intermediate (x:id x-:id τ e-) ...))
         (for ([orig-name (in-syntax #'(x ... ))]
               [new-name (in-syntax #'(x- ...))]
               [field-ty (in-syntax #'(τ ...))])
           (int-def-ctx-bind-type-rename orig-name new-name field-ty def-ctx))]
        [(erased (define/intermediate x:id x-:id τ e-))
         (int-def-ctx-bind-type-rename #'x #'x- #'τ def-ctx)]
        [_ (void)])
      (values (cons e- rev-e-...)
              (cons τ rev-τ...)
              (append ep-effs ep-effects)
              (append f-effs facet-effects)
              (append s-effs spawn-effects))))
  (values (reverse rev-e-...)
          (reverse rev-τ...)
          ep-effects
          facet-effects
          spawn-effects))

(define-typed-syntax (start-facet name:id ep ...+) ≫
  #:with name- (syntax-local-identifier-as-binding (syntax-local-introduce (generate-temporary #'name)))
  #:with name+ (syntax-local-identifier-as-binding #'name)
  #:with facet-name-ty (type-eval #'FacetName)
  #:do [(define ctx (syntax-local-make-definition-context))
        (define unique (gensym 'start-facet))
        (define name-- (internal-definition-context-introduce ctx #'name- 'add))
        (int-def-ctx-bind-type-rename #'name+ #'name- #'facet-name-ty ctx)
        (define-values (ep-... τ... ep-effects facet-effects spawn-effects)
          (walk/bind #'(ep ...) ctx unique))
        (unless (and (stx-null? facet-effects) (stx-null? spawn-effects))
          (type-error #:src #'(ep ...) #:msg "only endpoint effects allowed"))]
  #:with ((~or (~and τ-a (~Shares _))
               (~and τ-r (~Reacts _ ...))
               ~MakesField)
          ...)
         ep-effects
  #:with τ (type-eval #`(Role (#,name--)
                          τ-a ...
                          τ-r ...))
  --------------------------------------------------------------
  [⊢ (syndicate:react (let- ([#,name-- (syndicate:current-facet-id)])
                        #,@ep-...))
     (⇒ : ★/t)
     (⇒ f (τ))])

(define-typed-syntax (field [x:id τ-f:type e:expr] ...) ≫
  #:fail-unless (stx-andmap flat-type? #'(τ-f ...)) "keep your uppity data outta my fields"
  [⊢ e ≫ e- (⇐ : τ-f)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "field initializers not allowed to have effects"
  #:with (x- ...) (generate-temporaries #'(x ...))
  #:with (τ ...) (stx-map type-eval #'((Field τ-f.norm) ...))
  #:with MF (type-eval #'MakesField)
  ----------------------------------------------------------------------
  [⊢ (field/intermediate [x x- τ e-] ...)
     (⇒ : ★/t)
     (⇒ ep (MF))])

(define-syntax (field/intermediate stx)
  (syntax-parse stx
    [(_ [x:id x-:id τ e-] ...)
     #'(syndicate:field [x- e-] ...)]))

(define-typed-syntax (assert e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τs (type-eval #'(Shares τ))
  -------------------------------------
  [⊢ (syndicate:assert e-) (⇒ : ★/t)
                           (⇒ ep (τs))])

(define-typed-syntax (stop facet-name:id cont ...) ≫
  [⊢ facet-name ≫ facet-name- (⇐ : FacetName)]
  [⊢ (begin #f cont ...) ≫ cont- (⇒ ep (~effs)) (⇒ s (~effs)) (⇒ f (~effs τ-f ...))]
  #:with τ (mk-Stop- #`(facet-name- τ-f ...))
  ---------------------------------------------------------------------------------
  [⊢ (syndicate:stop-facet facet-name- cont-) (⇒ : ★/t)
                                              (⇒ f (τ))])

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
  [(on (~literal start) s ...) ≫
   [⊢ (begin s ...) ≫ s- (⇒ ep (~effs))
                          (⇒ f (~effs τ-f ...))
                          (⇒ s (~effs τ-s ...))]
   #:with τ-r (type-eval #'(Reacts OnStart τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on-start s-) (⇒ : ★/t)
      (⇒ ep (τ-r))]]
  [(on (~literal stop) s ...) ≫
   [⊢ (begin s ...) ≫ s- (⇒ ep (~effs))
                          (⇒ f (~effs τ-f ...))
                          (⇒ s (~effs τ-s ...))]
   #:with τ-r (type-eval #'(Reacts OnStop τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on-stop s-) (⇒ : ★/t)
      (⇒ ep (τ-r))]]
  [(on (a/r:asserted-or-retracted p) s ...) ≫
   [⊢ p ≫ p-- (⇒ : τp)]
   #:fail-unless (pure? #'p--) "pattern not allowed to have effects"
   #:with ([x:id τ:type] ...) (pat-bindings #'p)
   [[x ≫ x- : τ] ... ⊢ (begin s ...) ≫ s-
                 (⇒ ep (~effs))
                 (⇒ f (~effs τ-f ...))
                 (⇒ s (~effs τ-s ...))]
   #:with p- (substs #'(x- ...) #'(x ...) (compile-syndicate-pattern #'p))
   #:with τ-r (type-eval #'(Reacts (a/r.react-con τp) τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on (a/r.syndicate-kw p-)
                    s-)
      (⇒ : ★/t)
      (⇒ ep (τ-r))]])

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

;; TODO - figure out why this needs different list identifiers
(define-for-syntax (compile-pattern pat list-binding bind-id-transformer exp-transformer)
    (define (l-e stx) (local-expand stx 'expression '()))
    (let loop ([pat pat])
      (syntax-parse pat
        #:datum-literals (tuple discard bind)
        [(tuple p ...)
         #`(#,list-binding 'tuple #,@(stx-map loop #'(p ...)))]
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
         ;; local expanding "expression-y" syntax allows variable references to transform
         ;; according to the mappings set up by turnstile.
         (exp-transformer (l-e pat))])))

(define-for-syntax (compile-syndicate-pattern pat)
  (compile-pattern pat
                   #'list-
                   (lambda (id) #`($ #,id))
                   identity))

(define-for-syntax (compile-match-pattern pat)
  (compile-pattern pat
                   #'list
                   identity
                   (lambda (exp) #`(==- #,exp))))

(define-typed-syntax (spawn τ-c:type s) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  ;; TODO: check that each τ-f is a Role
  [⊢ s ≫ s- (⇒ ep (~effs)) (⇒ s (~effs)) (⇒ f (~effs τ-f ...))]
  ;; TODO: s shouldn't refer to facets or fields!
  #:with (τ-i τ-o τ-a) (analyze-roles #'(τ-f ...))
  #:fail-unless (<: #'τ-o #'τ-c.norm)
                (format "Output ~a not valid in dataspace ~a" (type->str #'τ-o) (type->str #'τ-c.norm))
  #:fail-unless (<: #'τ-a
                    (type-eval #'(Actor τ-c.norm)))
                "Spawned actors not valid in dataspace"
  #:fail-unless (project-safe? (∩ (strip-? #'τ-o) #'τ-c.norm)
                               #'τ-i)
                "Not prepared to handle all inputs"
  #:with τ-final (type-eval #'(Actor τ-c.norm))
  --------------------------------------------------------------------------------------------
  [⊢ (syndicate:spawn (syndicate:on-start s-)) (⇒ : ★/t)
                                               (⇒ s (τ-final))])

(define-typed-syntax (dataspace τ-c:type s ...) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ ep (~effs)) (⇒ s (~effs τ-s ...)) (⇒ f (~effs))] ...
  #:with τ-actor (type-eval #'(Actor τ-c.norm))
  #:fail-unless (stx-andmap (lambda (t) (<: t #'τ-actor)) #'(τ-s ... ...))
                "Not all actors conform to communication type"
  #:with τ-ds-i (strip-inbound #'τ-c.norm)
  #:with τ-ds-o (strip-outbound #'τ-c.norm)
  #:with τ-relay (relay-interests #'τ-c.norm)
  -----------------------------------------------------------------------------------
  [⊢ (syndicate:dataspace s- ...) (⇒ : ★/t)
                                  (⇒ s ((Actor (U τ-ds-i τ-ds-o τ-relay))))])

(define-typed-syntax (set! x:id e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  [⊢ x ≫ x- (⇒ : (~Field τ-x:type))]
  #:fail-unless (<: #'τ #'τ-x) "Ill-typed field write"
  ----------------------------------------------------
  [⊢ (x- e-) (⇒ : ★/t)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derived Forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (during p s ...) ≫
  #:with inst-p (instantiate-pattern #'p)
  ----------------------------------------
  [≻ (on (asserted p)
         (start-facet during-inner
           (on (retracted inst-p)
               (stop during-inner))
           s ...))])

;; TODO - reconcile this with `compile-pattern`
(define-for-syntax (instantiate-pattern pat)
  (let loop ([pat pat])
    (syntax-parse pat
      #:datum-literals (tuple discard bind)
      [(tuple p ...)
       #`(tuple #,@(stx-map loop #'(p ...)))]
      [(k:kons1 p)
       #`(k #,(loop #'p))]
      [(bind x:id τ)
       #'x]
      [discard
       #'_]
      [(~constructor-exp ctor p ...)
       (define/with-syntax uctor (untyped-ctor #'ctor))
       #`(ctor #,@(stx-map loop #'(p ...)))]
      [_
       pat])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (ref x:id) ≫
  ;; #:do [(printf "reference info ~a\n" (syntax-debug-info #'x))]
  [⊢ x ≫ x- ⇒ (~Field τ)]
  ;; #:do [(printf "~a ≫ ~a\n" #'x #'x-)]
  ------------------------
  [⊢ (x-) (⇒ : τ)])

(define-typed-syntax (lambda ([x:id (~optional (~datum :)) τ:type] ...) body ...+) ≫
  [[x ≫ x- : τ] ... ⊢ (begin body ...) ≫ body- (⇒ : τ-e)
                (⇒ ep (~effs τ-ep ...))
                (⇒ s (~effs τ-s ...))
                (⇒ f (~effs τ-f ...))]
  ----------------------------------------
  [⊢ (lambda- (x- ...) body-) (⇒ : (→ τ ... (Computation (Value τ-e)
                                                         (EndPoints τ-ep ...)
                                                         (Roles τ-f ...)
                                                         (Spawns τ-s ...))))])

(define-typed-syntax (typed-app e_fn e_arg ...) ≫
  [⊢ e_fn ≫ e_fn- (⇒ : (~→ τ_in ... (~Computation (~Value τ-out)
                                                  (~Endpoints τ-ep ...)
                                                  (~Roles τ-f ...)
                                                  (~Spawns τ-s ...))))]
  #:fail-unless (pure? #'e_fn-) "expression not allowed to have effects"
  #:fail-unless (stx-length=? #'[τ_in ...] #'[e_arg ...])
                (num-args-fail-msg #'e_fn #'[τ_in ...] #'[e_arg ...])
  [⊢ e_arg ≫ e_arg- (⇐ : τ_in)] ...
  #:fail-unless (stx-andmap pure? #'(e_arg- ...)) "expressions not allowed to have effects"
  ------------------------------------------------------------------------
  [⊢ (#%app- e_fn- e_arg- ...) (⇒ : τ-out)
     (⇒ ep (τ-ep ...))
     (⇒ s (τ-s ...))
     (⇒ f (τ-f ...))])

(define-typed-syntax (tuple e:expr ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions not allowed to have effects"
  -----------------------
  [⊢ (list- 'tuple e- ...) (⇒ : (Tuple τ ...))])

(define-typed-syntax (select n:nat e:expr) ≫
  [⊢ e ≫ e- (⇒ : (~Tuple τ ...))]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:do [(define i (syntax->datum #'n))]
  #:fail-unless (< i (stx-length #'(τ ...))) "index out of range"
  #:with τr (list-ref (stx->list #'(τ ...)) i)
  --------------------------------------------------------------
  [⊢ (tuple-select n e-) (⇒ : τr)])

(define- (tuple-select n t)
  (list-ref- t (add1 n)))

;; it would be nice to abstract over these three
(define-typed-syntax (observe e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  ---------------------------------------------------------------------------
  [⊢ (syndicate:observe e-) (⇒ : (Observe τ))])

(define-typed-syntax (inbound e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  ---------------------------------------------------------------------------
  [⊢ (syndicate:inbound e-) (⇒ : (Inbound τ))])

(define-typed-syntax (outbound e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  ---------------------------------------------------------------------------
  [⊢ (syndicate:outbound e-) (⇒ : (Outbound τ))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (bind x:id τ:type) ≫
  ----------------------------------------
  [⊢ (error- 'bind "escaped") (⇒ : (Bind τ))])

(define-typed-syntax discard
  [_ ≫
   --------------------
   [⊢ (error- 'discard "escaped") (⇒ : Discard)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core-ish forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copied from stlc
(define-typed-syntax (ann e (~optional (~datum :)) τ:type) ≫
  [⊢ e ≫ e- (⇐ : τ.norm)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  ------------------------------------------------
  [⊢ e- (⇒ : τ.norm) ])

(define-syntax (define/intermediate stx)
  (syntax-parse stx
    [(_ x:id x-:id τ e)
     #'(define- x- e)]))

;; copied from ext-stlc
(define-typed-syntax define
  [(_ x:id (~datum :) τ:type e:expr) ≫
   [⊢ e ≫ e- ⇐ τ.norm]
   #:fail-unless (pure? #'e-) "expression must be pure"
   #:with x- (generate-temporary #'x)
   --------
   [⊢ (define/intermediate x x- τ.norm e-) (⇒ : ★/t)]]
  [(_ x:id e) ≫
   ;This won't work with mutually recursive definitions
   [⊢ e ≫ e- ⇒ τ]
   #:fail-unless (pure? #'e-) "expression must be pure"
   #:with x- (generate-temporary #'x)
   --------
   [⊢ (define/intermediate x x- τ e-) (⇒ : ★/t)]]
  ;; TODO - not sure how to get this to work with effects
  ;; right now `ann` forces the body to be pure
  [(_ (f [x (~optional (~datum :)) ty] ... (~or (~datum →) (~datum ->)) ty_out) e ...+) ≫
   [⊢ (lambda ([x : ty] ...) (ann (begin e ...) : ty_out)) ≫ e- (⇒ : fun-ty)]
   #:with f- (add-orig (generate-temporary #'f) #'f)
   --------
   [⊢ (define/intermediate f f- fun-ty e-) (⇒ : ★/t)]])

;; copied from ext-stlc
(define-typed-syntax if
  [(_ e_tst e1 e2) ⇐ τ-expected ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   #:fail-unless (pure? #'e_tst-) "expression must be pure"
   [⊢ e1 ≫ e1- (⇐ : τ-expected)
      (⇒ ep (~effs eps1 ...)) (⇒ f (~effs fs1 ...)) (⇒ s (~effs ss1 ...))]
   [⊢ e2 ≫ e2- (⇐ : τ-expected)
      (⇒ ep (~effs eps2 ...)) (⇒ f (~effs fs2 ...)) (⇒ s (~effs ss2 ...))]
   --------
   [⊢ (if- e_tst- e1- e2-)
      (⇒ ep (eps1 ... eps2 ...))
      (⇒ f (fs1 ... fs2 ...))
      (⇒ s (ss1 ... ss2 ...))]]
  [(_ e_tst e1 e2) ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   #:fail-unless (pure? #'e_tst-) "expression must be pure"
   [⊢ e1 ≫ e1- (⇒ : τ1)
      (⇒ ep (~effs eps1 ...)) (⇒ f (~effs fs1 ...)) (⇒ s (~effs ss1 ...))]
   [⊢ e2 ≫ e2- (⇒ : τ2)
      (⇒ ep (~effs eps2 ...)) (⇒ f (~effs fs2 ...)) (⇒ s (~effs ss2 ...))]
   #:with τ (type-eval #'(U τ1 τ2))
   --------
   [⊢ (if- e_tst- e1- e2-) (⇒ : τ)
      (⇒ ep (eps1 ... eps2 ...))
      (⇒ f (fs1 ... fs2 ...))
      (⇒ s (ss1 ... ss2 ...))]])

;; copied from ext-stlc
(define-typed-syntax begin
  [(_ e_unit ... e) ≫
   #:do [(define-values (e-... τ... ep-effs f-effs s-effs) (walk/bind #'(e_unit ... e)))]
   #:with τ (last τ...)
   --------
   [⊢ (begin- #,@e-...) (⇒ : τ)
      (⇒ ep (#,@ep-effs))
      (⇒ f (#,@f-effs))
      (⇒ s (#,@s-effs))]])

;; copied from ext-stlc
(define-typed-syntax let
  [(_ ([x e] ...) e_body ...) ⇐ τ_expected ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions must be pure"
   [[x ≫ x- : τ_x] ... ⊢ (begin e_body ...) ≫ e_body- (⇐ : τ_expected)
                   (⇒ ep (~effs eps ...))
                   (⇒ f (~effs fs ...))
                   (⇒ s (~effs ss ...))]
   ----------------------------------------------------------
   [⊢ (let- ([x- e-] ...) e_body-)
      (⇒ ep (eps ...))
      (⇒ f (fs ...))
      (⇒ s (ss ...))]]
  [(_ ([x e] ...) e_body ...) ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions must be pure"
   [[x ≫ x- : τ_x] ... ⊢ (begin e_body ...) ≫ e_body- (⇒ : τ_body)
                   (⇒ ep (~effs eps ...))
                   (⇒ f (~effs fs ...))
                   (⇒ s (~effs ss ...))]
   ----------------------------------------------------------
   [⊢ (let- ([x- e-] ...) e_body-) (⇒ : τ_body)
      (⇒ ep (eps ...))
      (⇒ f (fs ...))
      (⇒ s (ss ...))]])

;; copied from ext-stlc
(define-typed-syntax let*
  [(_ () e_body ...) ≫
   --------
   [≻ (begin e_body ...)]]
  [(_ ([x e] [x_rst e_rst] ...) e_body ...) ≫
   --------
   [≻ (let ([x e]) (let* ([x_rst e_rst] ...) e_body ...))]])

(define-typed-syntax (cond [pred:expr s ...+] ...+) ≫
  [⊢ pred ≫ pred- (⇐ : Bool)] ...
  #:fail-unless (stx-andmap pure? #'(pred- ...)) "predicates must be pure"
  [⊢ (begin s ...) ≫ s- (⇒ : τ-s)
                   (⇒ ep (~effs eps ...))
                   (⇒ f (~effs fs ...))
                   (⇒ s (~effs ss ...))] ...
  ------------------------------------------------
  [⊢ (cond- [pred- s-] ...) (⇒ : (U τ-s ...))
     (⇒ ep (eps ... ...))
     (⇒ f (fs ... ...))
     (⇒ s (ss ... ...))])

(define-typed-syntax (match e [p s ...+] ...+) ≫
  [⊢ e ≫ e- (⇒ : τ-e)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  #:with (([x τ] ...) ...) (stx-map pat-bindings #'(p ...))
  [[x ≫ x- : τ] ... ⊢ (begin s ...) ≫ s- (⇒ : τ-s)
                (⇒ ep (~effs eps ...))
                (⇒ f (~effs fs ...))
                (⇒ s (~effs ss ...))] ...
  ;; REALLY not sure how to handle p/p-/p.match-pattern,
  ;; particularly w.r.t. typed terms that appear in p.match-pattern
  [⊢ p ≫ p-- ⇒ τ-p] ...
  #:fail-unless (project-safe? #'τ-e (type-eval #'(U τ-p ...))) "possibly unsafe pattern match"
  #:fail-unless (stx-andmap pure? #'(p-- ...)) "patterns must be pure"
  #:with (p- ...) (stx-map (lambda (p x-s xs) (substs x-s xs (compile-match-pattern p)))
                           #'(p ...)
                           #'((x- ...) ...)
                           #'((x ...) ...))
  --------------------------------------------------------------
  [⊢ (match- e- [p- s-] ...
                [_ (error "incomplete pattern match")])
     (⇒ : (U τ-s ...))
     (⇒ ep (eps ... ...))
     (⇒ f (fs ... ...))
     (⇒ s (ss ... ...))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hmmm
(define-primop + (→ Int Int (Computation (Value Int) (Endpoints) (Roles) (Spawns))))
(define-primop - (→ Int Int (Computation (Value Int) (Endpoints) (Roles) (Spawns))))
(define-primop * (→ Int Int (Computation (Value Int) (Endpoints) (Roles) (Spawns))))
#;(define-primop and (→ Bool Bool Bool))
(define-primop or (→ Bool Bool (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop not (→ Bool (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop < (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop > (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop <= (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop >= (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))
(define-primop = (→ Int Int (Computation (Value Bool) (Endpoints) (Roles) (Spawns))))

(define-typed-syntax (/ e1 e2) ≫
  [⊢ e1 ≫ e1- (⇐ : Int)]
  [⊢ e2 ≫ e2- (⇐ : Int)]
  #:fail-unless (pure? #'e1-) "expression not allowed to have effects"
  #:fail-unless (pure? #'e2-) "expression not allowed to have effects"
  ------------------------
  [⊢ (exact-truncate- (/- e1- e2-)) (⇒ : Int)])

;; for some reason defining `and` as a prim op doesn't work
(define-typed-syntax (and e ...) ≫
  [⊢ e ≫ e- (⇐ : Bool)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions not allowed to have effects"
  ------------------------
  [⊢ (and- e- ...) (⇒ : Bool)])

(define-typed-syntax (equal? e1:expr e2:expr) ≫
  [⊢ e1 ≫ e1- (⇒ : τ1)]
  #:fail-unless (flat-type? #'τ1)
  (format "equality only available on flat data; got ~a" (type->str #'τ1))
  [⊢ e2 ≫ e2- (⇐ : τ1)]
  #:fail-unless (pure? #'e1-) "expression not allowed to have effects"
  #:fail-unless (pure? #'e2-) "expression not allowed to have effects"
  ---------------------------------------------------------------------------
  [⊢ (equal?- e1- e2-) (⇒ : Bool)])

(define-typed-syntax (displayln e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  ---------------
  [⊢ (displayln- e-) (⇒ : ★/t)])

(define-typed-syntax (printf e ...+) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  #:fail-unless (stx-andmap pure? #'(e- ...)) "expression not allowed to have effects"
  ---------------
  [⊢ (printf- e- ...) (⇒ : ★/t)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (print-type e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ep (~effs eps ...)) (⇒ f (~effs fs ...)) (⇒ s (~effs ss ...))]
  #:do [(displayln (type->str #'τ))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ ep (eps ...)) (⇒ f (fs ...)) (⇒ s (ss ...))])

(define-typed-syntax (print-role e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ep (~effs eps ...)) (⇒ f (~effs fs ...)) (⇒ s (~effs ss ...))]
  #:do [(for ([r (in-syntax #'(fs ...))])
          (displayln (type->str r)))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ ep (eps ...)) (⇒ f (fs ...)) (⇒ s (ss ...))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (list e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  #:fail-unless (all-pure? #'(e- ...)) "expressions must be pure"
  -------------------
  [⊢ (list- e- ...) ⇒ (List (U τ ...))])

(define-typed-syntax (for/fold [acc:id e-acc]
                               [x:id e-list]
                       e-body ...+) ≫
  [⊢ e-list ≫ e-list- ⇒ (~List τ-l)]
  #:fail-unless (pure? #'e-list-) "expression must be pure"
  [⊢ e-acc ≫ e-acc- ⇒ τ-a]
  #:fail-unless (pure? #'e-acc-) "expression must be pure"
  [[x ≫ x- : τ-l] [acc ≫ acc- : τ-a] ⊢ (begin e-body ...) ≫ e-body- ⇒ τ-b]
  #:fail-unless (pure? #'e-body-) "body must be pure"
  #:fail-unless (<: #'τ-b #'τ-a)
    "loop body doesn't match accumulator"
  -------------------------------------------------------
  [⊢ (for/fold- ([acc- e-acc-])
                ([x- (in-list- e-list-)])
       e-body-)
     ⇒ τ-b])

(define-typed-syntax (for ([x:id e-list] ...)
                       e-body ...+) ≫
  [⊢ e-list ≫ e-list- ⇒ (~List τ-l)] ...
  #:fail-unless (all-pure? #'(e-list- ...)) "expressions must be pure"
  [[x ≫ x- : τ-l] ... ⊢ (begin e-body ...) ≫ e-body- (⇒ : τ-b)
                  (⇒ ep (~effs eps ...))
                  (⇒ f (~effs fs ...))
                  (⇒ s (~effs ss ...))]
  -------------------------------------------------------
  [⊢ (for- ([x- (in-list- e-list-)] ...)
       e-body-) (⇒ : ★/t)
                (⇒ ep (eps ...))
                (⇒ f (fs ...))
                (⇒ s (ss ...))])

(define-typed-syntax (empty? e) ≫
  [⊢ e ≫ e- ⇒ (~List _)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  -----------------------
  [⊢ (empty?- e-) ⇒ Bool])

(define-typed-syntax (first e) ≫
  [⊢ e ≫ e- ⇒ (~List τ)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  -----------------------
  [⊢ (first- e-) ⇒ τ])

(define-typed-syntax (rest e) ≫
  [⊢ e ≫ e- ⇒ (~List τ)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  -----------------------
  [⊢ (rest- e-) ⇒ (List τ)])

(define-typed-syntax (member? e l) ≫
  [⊢ e ≫ e- ⇒ τe]
  #:fail-unless (pure? #'e-) "expression must be pure"
  [⊢ l ≫ l- ⇒ (~List τl)]
  #:fail-unless (pure? #'l-) "expression must be pure"
  #:fail-unless (<: #'τe #'τl) "incompatible list"
  ----------------------------------------
  [⊢ (member?- e- l-) ⇒ Bool])

(define- (member?- v l)
  (and- (member- v l) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (set e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  #:fail-unless (all-pure? #'(e- ...)) "expressions must be pure"
  ---------------
  [⊢ (set- e- ...) ⇒ (Set (U τ ...))])

(define-typed-syntax (set-count e) ≫
  [⊢ e ≫ e- ⇒ (~Set _)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  ----------------------
  [⊢ (set-count- e-) ⇒ Int])

(define-typed-syntax (set-add st v) ≫
  [⊢ st ≫ st- ⇒ (~Set τs)]
  #:fail-unless (pure? #'st-) "expression must be pure"
  [⊢ v ≫ v- ⇒ τv]
  #:fail-unless (pure? #'v-) "expression must be pure"
  -------------------------
  [⊢ (set-add- st- v-) ⇒ (Set (U τs τv))])

(define-typed-syntax (set-member? st v) ≫
  [⊢ st ≫ st- ⇒ (~Set τs)]
  #:fail-unless (pure? #'st-) "expression must be pure"
  [⊢ v ≫ v- ⇒ τv]
  #:fail-unless (pure? #'v-) "expression must be pure"
  #:fail-unless (<: #'τv #'τs)
    "type mismatch"
  -------------------------------------
  [⊢ (set-member?- st- v-) ⇒ Bool])

(define-typed-syntax (set-union st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set τ-st)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  -------------------------------------
  [⊢ (set-union- st0- st- ...) ⇒ (Set (U τ-st0 τ-st ...))])

(define-typed-syntax (set-intersect st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set τ-st)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  #:with τr (∩ #'τ-st0 (type-eval #'(U τ-st ...)))
  -------------------------------------
  [⊢ (set-intersect- st0- st- ...) ⇒ (Set τr)])

(define-typed-syntax (set-subtract st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set _)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  -------------------------------------
  [⊢ (set-subtract- st0- st- ...) ⇒ (Set τ-st0)])

(define-typed-syntax (list->set l) ≫
  [⊢ l ≫ l- ⇒ (~List τ)]
  #:fail-unless (pure? #'l-) "expression must be pure"
  -----------------------
  [⊢ (list->set- l-) ⇒ (Set τ)])

(define-typed-syntax (set->list s) ≫
  [⊢ s ≫ s- ⇒ (~Set τ)]
  #:fail-unless (pure? #'s-) "expression must be pure"
  -----------------------
  [⊢ (set->list- s-) ⇒ (List τ)])

(module+ test
  (check-type (set 1 2 3)
              : (Set Int)
              -> (set- 2 3 1))
  (check-type (set 1 "hello" 3)
              : (Set (U Int String))
              -> (set- "hello" 3 1))
  (check-type (set-count (set 1 "hello" 3))
              : Int
              -> 3)
  (check-type (set-union (set 1 2 3) (set "hello" "world"))
              : (Set (U Int String))
              -> (set- 1 2 3 "hello" "world"))
  (check-type (set-intersect (set 1 2 3) (set "hello" "world"))
              : (Set ⊥)
              -> (set-))
  (check-type (set-intersect (set 1 "hello" 3) (set #t "world" #f "hello"))
              : (Set String)
              -> (set- "hello")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;