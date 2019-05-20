#lang turnstile

(provide (all-defined-out)
         (for-syntax (all-defined-out))
         (for-meta 2 (all-defined-out)))
(require (only-in turnstile
                  [define-type-constructor define-type-constructor-]))

(require (prefix-in syndicate: syndicate/actor-lang))

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx syntax/parse racket/base))
(require (for-syntax turnstile/examples/util/filter-maximal))
(require (for-syntax macrotypes/type-constraints macrotypes/variance-constraints))
(require (for-syntax racket/struct-info))
(require macrotypes/postfix-in)
(require (rename-in racket/math [exact-truncate exact-truncate-]))
(require (postfix-in - racket/list))
(require (postfix-in - racket/set))
(require (postfix-in - racket/match))
(require (postfix-in - (only-in racket/format ~a)))


(module+ test
  (require rackunit)
  (require rackunit/turnstile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checking Conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; : describes the immediate result of evaluation
;; ν-ep key aggregates endpoint affects:
;;   `Shares`, `Reacts`, and `MakesField`
;; Note thar MakesField is only an effect, not a type
;; ν-f key aggregates facet effects (starting/stopping a facet) as `Role`s & `Stop`s and message sends, `Sends`
;; ν-s key aggregates spawned actors as `Actor`s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; certain metadata needs to be associated with each type, for the purpose of
;; making certain judgments and metafunctions extensible.

;; a isect-desc describes how a type (constructor) behaves with respect to
;; intersection, and is one of
;;   - BASE
;;   - CONTAINER-LIKE
;;   - PRODUCT-LIKE
(begin-for-syntax
  (define BASE 'base)
  (define CONTAINER-LIKE 'container-like)
  (define PRODUCT-LIKE 'product-like)

  ;; syntax property key
  (define isect-desc-key
    'isect-desc-key)

  (define-syntax-class isect-desc
    #:attributes (val)
    #:datum-literals (BASE CONTAINER-LIKE PRODUCT-LIKE)
    (pattern BASE
             #:attr val BASE)
    (pattern CONTAINER-LIKE
             #:attr val CONTAINER-LIKE)
    (pattern PRODUCT-LIKE
             #:attr val PRODUCT-LIKE))

  ;; Any -> Bool
  ;; recognize isect-descs
  (define (isect-desc? x)
    (member x (list BASE CONTAINER-LIKE PRODUCT-LIKE)))

  ;; syntax property key
  ;; syntax-transformer value
  (define type-cons-key
    'type-cons)

  ;; Type -> Bool
  ;; check if the type has a syntax property allowing us to create new instances
  (define (reassemblable? t)
    (and (syntax-property t type-cons-key) #t))

  ;; Type (Listof Type) -> Type
  ;; Create a new instance of the type with the given arguments
  ;; needs to have the type-cons-key
  (define (reassemble-type ty args)
    (define tycons (syntax-property ty type-cons-key))
    (unless tycons
      (error "expected to find type-cons-key"))
    (type-eval #`(#,tycons #,@args))))

(define-syntax (define-type-constructor+ stx)
  (syntax-parse stx
    [(_ Name:id
        #:arity op arity
        #:arg-variances variances
        #:isect-desc desc:isect-desc
        (~optional (~seq #:extra-info extra-info)))
     #:with Name- (mk-- #'Name)
     #:with NamePat (mk-~ #'Name)
     #:with NamePat- (mk-~ #'Name-)
     #:with mk (format-id #'Name "mk-~a-" (syntax-e #'Name))
     #:with mk- (format-id #'Name- "mk-~a-" (syntax-e #'Name-))
     (quasisyntax/loc stx
       (begin-
         (define-type-constructor- Name-
           #:arity op arity
           #:arg-variances variances
           #,@(if (attribute extra-info)
                  #'(#:extra-info extra-info)
                  #'()))
         (define-syntax (Name stx)
           (syntax-parse stx
             [(_ t (... ...))
              (set-stx-prop/preserved
               (set-stx-prop/preserved
                (syntax/loc stx
                  (Name- t (... ...)))
                #,isect-desc-key
                '#,(attribute desc.val))
               type-cons-key
               #'Name)]))
         (begin-for-syntax
           (define-syntax NamePat
             (pattern-expander
              (syntax-parser
                [(_ p (... ...))
                 #'(NamePat- p (... ...))]))))
         (define-for-syntax mk mk-)))]))

(begin-for-syntax
  ;; Syntax -> (Listof Variant)
  ;; make a list of the same length as the number of arguments of the given
  ;; (type) syntax, all covariant
  (define (mk-covariant ts)
    (for/list ([_ (sequence-tail (in-syntax ts) 1)])
      covariant)))

;; Define a type constructor that acts like a container:
;;   - covariant
;;   - has an empty element (i.e. intersection always non-empty)
(define-syntax (define-container-type stx)
  (syntax-parse stx
    [(_ Name:id #:arity op arity
        (~optional (~seq #:extra-info extra-info)))
     (quasisyntax/loc stx
       (define-type-constructor+ Name
         #:arity op arity
         #:arg-variances mk-covariant
         #:isect-desc CONTAINER-LIKE
         #,@(if (attribute extra-info)
                #'(#:extra-info extra-info)
                #'())))]))

;; Define a type constructor that acts like a container:
;;   - covariant
;;   - does not have an empty element (i.e. intersection may be empty)
(define-syntax (define-product-type stx)
  (syntax-parse stx
    [(_ Name:id #:arity op arity
        (~optional (~seq #:extra-info extra-info)))
     (quasisyntax/loc stx
       (define-type-constructor+ Name
         #:arity op arity
         #:arg-variances mk-covariant
         #:isect-desc PRODUCT-LIKE
         #,@(if (attribute extra-info)
                #'(#:extra-info extra-info)
                #'())))]))

(define-binding-type Role #:arity >= 0 #:bvs = 1)
(define-type-constructor Shares #:arity = 1)
(define-type-constructor Sends #:arity = 1)
(define-type-constructor Reacts #:arity >= 1)
(define-type-constructor Know #:arity = 1)
(define-type-constructor ¬Know #:arity = 1)
(define-type-constructor Stop #:arity >= 1)
(define-type-constructor Field #:arity = 1)
(define-type-constructor Bind #:arity = 1)
;; keep track of branches for facet effects
;; (Branch (Listof (Listof Type)))
(define-type-constructor Branch #:arity >= 0)
;; sequence of effects
(define-type-constructor Effs #:arity >= 0)
(define-base-types OnStart OnStop OnDataflow MakesField)
(define-for-syntax field-prop-name 'fields)
(define-type-constructor Actor #:arity = 1)

(define-product-type Message #:arity = 1)
(define-product-type Tuple #:arity >= 0)
(define-product-type Observe #:arity = 1)
(define-product-type Inbound #:arity = 1)
(define-product-type Outbound #:arity = 1)
(define-container-type AssertionSet #:arity = 1)
(define-container-type Patch #:arity = 2)

;; functions and type abstractions
(define-binding-type ∀)
(define-type-constructor → #:arity > 0)

;; for describing the RHS
;; a value and a description of the effects
(define-type-constructor Computation #:arity = 4)
(define-type-constructor Value #:arity = 1)
(define-type-constructor Endpoints #:arity >= 0)
(define-type-constructor Roles #:arity >= 0)
(define-type-constructor Spawns #:arity >= 0)


(define-base-types Discard ★/t FacetName)

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

;; (SyntaxListof Type) -> Type
;; The input types are already expanded/normalized
;; avoids namespace module mismatch issue in some cases
(define-for-syntax (mk-U- tys)
  (define tys- (prune+sort tys))
  (if (= 1 (stx-length tys-))
      (stx-car tys-)
      (mk-U*- tys-)))

(define-syntax (U stx)
  (syntax-parse stx
    [(_ . tys)
     ;; canonicalize by expanding to U*, with only (sorted and pruned) leaf tys
     #:with ((~or (~U* ty1- ...) ty2-) ...) (stx-map (current-type-eval) #'tys)
     #:with tys- (prune+sort #'(ty1- ... ... ty2- ...))
     (if (= 1 (stx-length #'tys-))
         (stx-car #'tys-)
         (syntax/loc stx (U* . tys-)))]))

(define-simple-macro (→fn ty-in ... ty-out)
  (→ ty-in ... (Computation (Value ty-out)
                            (Endpoints)
                            (Roles)
                            (Spawns))))

(begin-for-syntax
  (define-syntax ~→fn
    (pattern-expander
     (syntax-parser
       [(_ ty-in:id ... ty-out)
        #'(~→ ty-in ... (~Computation (~Value ty-out)
                                      (~Endpoints)
                                      (~Roles)
                                      (~Spawns)))])))

  ;; matching possibly polymorphic types
  (define-syntax ~?∀
    (pattern-expander
     (lambda (stx)
       (syntax-case stx ()
         [(?∀ vars-pat body-pat)
          #'(~or (~∀ vars-pat body-pat)
                 (~and (~not (~∀ _ _))
                       (~parse vars-pat #'())
                       body-pat))])))))

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
         (quasisyntax/loc stx
           (#,transformer e ...))]))))

(define-syntax (define-constructor* stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (Cons:id : TyCons:id slot:id ...) clause ...)
     #'(define-constructor (Cons slot ...)
         #:type-constructor TyCons
         clause ...)]))

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
         (define-product-type TypeCons
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

;; (require-struct chicken #:as Chicken #:from "some-mod.rkt") will
;;  - extract the struct-info for chicken, and ensure that it is immutable, has a set number of fields
;;  - determine the number of slots, N, chicken has
;;  - define the type constructor (Chicken ...N), with the extra info used by define-constructor above
;;  - define chicken+, a turnstile type rule that checks uses of chicken
;;  - bind chicken to a user-ctor struct
;; TODO: this implementation shares a lot with that of define-constructor
(define-syntax (require-struct stx)
  (syntax-parse stx
    [(_ ucons:id #:as ty-cons:id #:from lib)
     (with-syntax* ([TypeCons #'ty-cons]
                    [MakeTypeCons (format-id #'TypeCons "make-~a" #'TypeCons)]
                    [GetTypeParams (format-id #'TypeCons "get-~a-type-params" #'TypeCons)]
                    [TypeConsExpander (format-id #'TypeCons "~~~a" #'TypeCons)]
                    [TypeConsExtraInfo (format-id #'TypeCons "~a-extra-info" #'TypeCons)]
                    [Cons- (format-id #'ucons "~a/checked" #'ucons)]
                    [orig-struct-info (generate-temporary #'ucons)]
                    [type-tag (generate-temporary #'ucons)])
       (quasisyntax/loc stx
         (begin-
           (require- (only-in- lib [ucons orig-struct-info]))
           (begin-for-syntax
             (define info (syntax-local-value #'orig-struct-info))
             (unless (struct-info? info)
               (raise-syntax-error #f "expected struct" #'#,stx #'ucons))
             (match-define (list desc cons pred accs muts sup) (extract-struct-info info))
             (when (false? (last accs))
               (raise-syntax-error #f "number of slots must be exact" #'#,stx #'ucons))
             (unless (equal? #t sup)
               (raise-syntax-error #f "structs with super-type not supported" #'#,stx #'ucons))
             (define arity (length accs)))
           (define-syntax (TypeConsExtraInfo stx)
             (syntax-parse stx
               [(_ X (... ...)) #'('type-tag 'MakeTypeCons 'GetTypeParams)]))
           (define-type-constructor TypeCons
             ;; issue: arity needs to parse as an exact-nonnegative-integer
             ;; fix: check arity in MakeTypeCons
             #:arity >= 0
             #:extra-info 'TypeConsExtraInfo)
           (define-syntax (MakeTypeCons stx)
             (syntax-parse stx
               [(_ t (... ...))
                #:fail-unless (= arity (stx-length #'(t (... ...)))) "arity mismatch"
                #'(TypeCons t (... ...))]))
           (define-syntax (GetTypeParams stx)
             (syntax-parse stx
               [(_ (TypeConsExpander t (... ...)))
                #'(t (... ...))]))
           (define-typed-syntax (Cons- e (... ...)) ≫
             #:fail-unless (= arity (stx-length #'(e (... ...)))) "arity mismatch"
             [⊢ e ≫ e- (⇒ : τ)] (... ...)
             #:fail-unless (all-pure? #'(e- (... ...))) "expressions must be pure"
             ----------------------
             [⊢ (#%app- orig-struct-info e- (... ...)) (⇒ : (TypeCons τ (... ...)))])
           (define-syntax ucons
             (user-ctor #'Cons- #'orig-struct-info)))))]))

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
;; Require & Provide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Import and ascribe a type from an untyped module
;; TODO: this is where contracts would need to go
(define-syntax (require/typed stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ lib [name:id : ty:type] ...)
     #:with (name- ...) (format-ids "~a-" #'(name ...))
     (syntax/loc stx
       (begin-
         (require (only-in lib [name name-] ...))
         (define-syntax name
           (make-variable-like-transformer (add-orig (assign-type #'name- #'ty #:wrap? #f) #'name)))
         ...))]))

;; Format identifiers in the same way
;; FormatString (SyntaxListOf Identifier) -> (Listof Identifier)
(define-for-syntax (format-ids fmt ids)
  (for/list ([id (in-syntax ids)])
    (format-id id fmt id)))

;; (SyntaxListof (SyntaxList Identifier Type Identifier)) -> (Listof Identifier)
;; For each triple (name- ty name),
;; assign the ty to name- with the orig name
(define-for-syntax (assign-types los)
  (for/list ([iti (in-syntax los)])
    (match-define (list name- ty name) (syntax->list iti))
    (add-orig (assign-type name- ty #:wrap? #f) name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conveniences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (SyntaxListof (SyntaxListof Type)) -> (U (SyntaxListof Branch) #'())
(define-for-syntax (make-Branch tys*)
  (syntax-parse tys*
    [()
     #'()]
    [(() ...)
     #'()]
    [((ty ...) ...)
     (define effs
       (for/list ([tys (in-syntax tys*)])
         (mk-Effs- (syntax->list tys))))
     #`(#,(mk-Branch- effs))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax

  ;; constructors with arity one
  (define-syntax-class kons1
    (pattern (~or (~datum observe)
                  (~datum inbound)
                  (~datum outbound)
                  (~datum message))))

  (define (kons1->constructor stx)
    (syntax-parse stx
      #:datum-literals (observe inbound outbound)
      [observe #'syndicate:observe]
      [inbound #'syndicate:inbound]
      [outbound #'syndicate:outbound]
      [message #'syndicate:message]))

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
  ((current-typecheck-relation) t (type-eval #'(U*))))

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
     ;; since (Observe X) can match (Message X):
     ;; doing this specifically for the intersection operation in the spawn rule, need to check other
     ;; uses
     [(~Observe τ) #'(U τ (Message τ))]
     [_ #'(U*)])))

;; similar to strip- fns, but leave non-message types as they are
(define-for-syntax (prune-message t)
  (type-eval
   (syntax-parse t
     [(~U* τ ...) #`(U #,@(stx-map prune-message #'(τ ...)))]
     [~★/t #'★/t]
     [(~Message τ) #'τ]
     [_ t])))

(define-for-syntax (strip-inbound t)
  (type-eval
   (syntax-parse t
     [(~U* τ ...) #`(U #,@(stx-map strip-inbound #'(τ ...)))]
     [~★/t #'★/t]
     [(~Inbound τ) #'τ]
     [_ #'(U*)])))

(define-for-syntax (strip-outbound t)
  (type-eval
   (syntax-parse t
     [(~U* τ ...) #`(U #,@(stx-map strip-outbound #'(τ ...)))]
     [~★/t #'★/t]
     [(~Outbound τ) #'τ]
     [_ #'(U*)])))

(define-for-syntax (relay-interests t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U* τ ...) #`(U #,@(stx-map relay-interests #'(τ ...)))]
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
    [(~Branch (~Effs τ-r ...) ...)
     #:with (τi τo τa) (analyze-roles #'(τ-r ... ...))
     (values #'τi #'τo #'τa)]
    [(~Stop name:id τ-r ...)
     #:with (τi τo τa) (analyze-roles #'(τ-r ...))
     (values #'τi #'τo #'τa)]
    [(~Actor τc)
     (values (mk-U*- '()) (mk-U*- '()) t)]
    [(~Sends τ-m)
     (values (mk-U*- '()) (type-eval #'(Message τ-m)) (mk-U*- '()))]
    [(~Role (name:id)
       (~or (~Shares τ-s)
            (~Sends τ-m)
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
             (type-eval #`(U τ-s ... (Message τ-m) ... #,@os #,@(stx-map pattern-sub-type pat-types)))
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
  (syntax-parse pt
    [(~Message τ)
     (define t (replace-bind-and-discard-with-★ #'τ))
     (type-eval #`(Observe #,t))]
    [τ
     #:when (bot? #'τ)
     #'τ]
    [τ
     (define t (replace-bind-and-discard-with-★ #'τ))
     (type-eval #`(Observe #,t))]))

;; TODO : can potentially use something like `subst` for this
(define-for-syntax (replace-bind-and-discard-with-★ t)
  (syntax-parse t
    [(~Bind _)
     (type-eval #'★/t)]
    [~Discard
     (type-eval #'★/t)]
    [(~U* τ ...)
     (mk-U- (stx-map replace-bind-and-discard-with-★ #'(τ ...)))]
    [(~Any/bvs τ-cons () τ ...)
     #:when (reassemblable? t)
     (define subitems (for/list ([t (in-syntax #'(τ ...))])
                        (replace-bind-and-discard-with-★ t)))
     (reassemble-type t subitems)]
    [_ t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping and Judgments on Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
 (define-syntax ~Base
   (pattern-expander
    (syntax-parser
      [(_ nm:id)
       #'((~literal #%plain-app) nm)])))

 ;; Type Type -> Bool
 ;; subtyping
 (define (<: t1 t2)
   (syntax-parse #`(#,t1 #,t2)
     [(_ ~★/t)
      (flat-type? t1)]
     [((~U* τ1 ...) _)
      (stx-andmap (lambda (t) (<: t t2)) #'(τ1 ...))]
     [(_ (~U* τ2:type ...))
      (stx-ormap (lambda (t) (<: t1 t)) #'(τ2 ...))]
     [((~Actor τ1) (~Actor τ2))
      (and (<: #'τ1 #'τ2)
           (<: (∩ (strip-? #'τ1) #'τ2) #'τ1))]
     [((~→fn τ-in1 ... τ-out1) (~→fn τ-in2 ... τ-out2))
      (and (stx-length=? #'(τ-in1 ...) #'(τ-in2 ...))
           (stx-andmap <: #'(τ-in2 ...) #'(τ-in1 ...))
           (<: #'τ-out1 #'τ-out2))]
     [(~Discard _)
      #t]
     [(X:id Y:id)
      (free-identifier=? #'X #'Y)]
     [((~∀ (X:id ...) τ1) (~∀ (Y:id ...) τ2))
      #:when (stx-length=? #'(X ...) #'(Y ...))
      #:with τ2-X/Y (substs #'(X ...) #'(Y ...) #'τ2)
      (<: #'τ1 #'τ2-X/Y)]
     [((~Base τ1:id) (~Base τ2:id))
      (free-identifier=? #'τ1 #'τ2)]
     [((~Any/bvs τ-cons1 () τ1 ...) (~Any/bvs τ-cons2 () τ2 ...))
      #:when (free-identifier=? #'τ-cons1 #'τ-cons2)
      #:do [(define variances (syntax-property #'τ-cons1 'arg-variances))]
      #:when variances
      #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
      (for/and ([ty1 (in-syntax #'(τ1 ...))]
                [ty2 (in-syntax #'(τ2 ...))]
                [var (in-list variances)])
        (match var
          [(== covariant)
           (<: ty1 ty2)]
          [(== contravariant)
           (<: ty2 ty1)]
          [(== invariant)
           (and (<: ty1 ty2)
                (<: ty2 ty1))]
          [(== irrelevant)
           #t]))]
     ;; TODO: clauses for Roles, effectful functions, and so on
     [_
      #f]))

 ;; shortcuts for mapping
 (define ((<:l l) r)
   (<: l r))

 (define ((<:r r) l)
   (<: l r)))

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; MODIFYING GLOBAL TYPECHECKING STATE!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(begin-for-syntax
  (current-typecheck-relation <:))

;; Flat-Type Flat-Type -> Type
;; Intersection
(define-for-syntax (∩ t1 t2)
  (unless (and (flat-type? t1) (flat-type? t2))
    (error '∩ "expected two flat-types"))
  (syntax-parse #`(#,t1 #,t2)
    [(_ ~★/t)
     t1]
    [(~★/t _)
     t2]
    [((~U* τ1:type ...) _)
     (type-eval #`(U #,@(stx-map (lambda (t) (∩ t t2)) #'(τ1 ...))))]
    [(_ (~U* τ2:type ...))
     (type-eval #`(U #,@(stx-map (lambda (t) (∩ t1 t)) #'(τ2 ...))))]
    [(X:id Y:id)
     #:when (free-identifier=? #'X #'Y)
     #'X]
    [((~AssertionSet τ1) (~AssertionSet τ2))
     #:with τ12 (∩ #'τ1 #'τ2)
     (type-eval #'(AssertionSet τ12))]
    ;; Also, using <: is OK, even though <: refers to ∩, because <:'s use of ∩ is only
    ;; in the Actor case.
    [((~Base τ1:id) (~Base τ2:id))
     #:when (free-identifier=? #'τ1 #'τ2)
     t1]
    [((~Any/bvs τ-cons1 () τ1 ...) (~Any/bvs τ-cons2 () τ2 ...))
     #:when (free-identifier=? #'τ-cons1 #'τ-cons2)
     #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
     #:do [(define desc (syntax-property t1 isect-desc-key))]
     #:when desc
     (define slots (stx-map ∩ #'(τ1 ...) #'(τ2 ...)))
     (match desc
       [(== BASE)
        (error "this isn't right")]
       [(== CONTAINER-LIKE)
        (reassemble-type t1 slots)]
       [(== PRODUCT-LIKE)
        (if (ormap bot? slots)
            (type-eval #'(U))
            (reassemble-type t1 slots))])]
    [_ (type-eval #'(U))]))

;; Type Type -> Bool
;; first type is the contents of the set/dataspace
;; second type is the type of a pattern
(define-for-syntax (project-safe? t1 t2)
  ;; TODO - not sure how to handle type variables
  (define (project-safe* t1 t2)
    (syntax-parse #`(#,t1 #,t2)
      [(_ (~Bind τ2))
       (and (finite? t1) (<: t1 #'τ2))]
      [(_ ~Discard)
       #t]
      [(_ ~★/t)
       #t]
      [((~U* τ1:type ...) _)
       (stx-andmap (lambda (t) (project-safe? t t2)) #'(τ1 ...))]
      [(_ (~U* τ2:type ...))
       (stx-andmap (lambda (t) (project-safe? t1 t)) #'(τ2 ...))]
      [((~Any/bvs τ-cons1 () τ1 ...) (~Any/bvs τ-cons2 () τ2 ...))
       #:when (free-identifier=? #'τ-cons1 #'τ-cons2)
       #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
       #:do [(define desc (syntax-property t1 isect-desc-key))]
       #:when (equal? desc PRODUCT-LIKE)
       (stx-andmap project-safe? #'(τ1 ...) #'(τ2 ...))]
      [_ #t]))
  (if (overlap? t1 t2)
      (project-safe* t1 t2)
      #t))

;; AssertionType PatternType -> Bool
;; Is it possible for things of these two types to match each other?
;; Flattish-Type = Flat-Types + ★/t, Bind, Discard (assertion and pattern types)
(define-for-syntax (overlap? t1 t2)
  (define t22 (replace-bind-and-discard-with-★ t2))
  (not (<: (∩ t1 t22) (mk-U*- '()))))

;; Flattish-Type -> Bool
(define-for-syntax (finite? t)
  (syntax-parse t
    [~★/t #f]
    [(~U* τ:type ...)
     (stx-andmap finite? #'(τ ...))]
    ;; TODO - this is questionable. maybe need a kind for assertions?
    [X:id
     #t]
    [(~Base _) #t]
    [(~Any/bvs τ-cons () τ ...)
     (stx-andmap finite? #'(τ ...))]))

;; PatternType -> Type
(define-for-syntax (pattern-matching-assertions t)
  (syntax-parse t
    [(~Bind τ)
     #'τ]
    [~Discard
     (type-eval #'★/t)]
    [(~U* τ ...)
     (type-eval #`(U #,@(stx-map pattern-matching-assertions #'(τ ...))))]
    [(~Any/bvs τ-cons () τ ...)
     #:when (reassemblable? t)
     (define subitems (for/list ([t (in-syntax #'(τ ...))])
                        (pattern-matching-assertions t)))
     (reassemble-type t subitems)]
    [_ t]))

;; it's ok for x to respond to strictly more events than y
(define-for-syntax (condition-covers? x y)
  (or
   ;; covers Start,Stop,Dataflow
   (type=? x y)
   (syntax-parse #`(#,x #,y)
     [((~Know τ1) (~Know τ2))
      (<: (pattern-matching-assertions #'τ2)
          (pattern-matching-assertions #'τ1))]
     [((~¬Know τ1) (~¬Know τ2))
      (<: (pattern-matching-assertions #'τ2)
          (pattern-matching-assertions #'τ1))]
     [((~Message τ1) (~Message τ2))
      (<: (pattern-matching-assertions #'τ2)
          (pattern-matching-assertions #'τ1))]
     [_ #f])))

;; RoleType RoleType -> Bool
;; Check that role r implements role spec (possibly does more)
(define-for-syntax (role-implements? r spec)
  (syntax-parse #`(#,r #,spec)
    ;; TODO: cases for unions, stop
    [((~Role (x:id) (~or (~Shares τ-s1) (~Sends τ-m1) (~Reacts τ-if1 τ-then1 ...)) ...)
      (~Role (y:id) (~or (~Shares τ-s2) (~Sends τ-m2) (~Reacts τ-if2 τ-then2 ...)) ...))
     #:when (free-identifier=? #'x #'y)
     (and
      ;; for each assertion in the spec, there must be a suitable assertion in the actual
      ;; TODO: this kinda ignores numerosity, can one assertion in r cover multiple assertions in spec?
      (for/and [(s2 (in-syntax #'(τ-s2 ...)))]
        (stx-ormap (<:l s2) #'(τ-s1 ...)))
      ;; similar for messages
      (for/and [(m2 (in-syntax #'(τ-m2 ...)))]
        (stx-ormap (<:l m2) #'(τ-m1 ...)))
      (for/and [(s2 (in-syntax #'((τ-if2 (τ-then2 ...)) ...)))]
        (define/syntax-parse (τ-if2 (τ-then2 ...)) s2)
        (for/or [(s1 (in-syntax #'((τ-if1 (τ-then1 ...)) ...)))]
          (define/syntax-parse (τ-if1 (τ-then1 ...)) s1)
          ;; the event descriptors need to line up
          (and (condition-covers? #'τ-if1 #'τ-if2)
               ;; and for each specified response to the event, there needs to be a similar one in the
               ;; the actual
               (stx-andmap (lambda (s) (stx-ormap (lambda (r) (role-implements? r s)) #'(τ-then1 ...)))
                           #'(τ-then2 ...))))))]
    [((~Role (x:id) _ ...)
      (~Role (y:id) _ ...))
     (role-implements? (subst #'y #'x r) spec)]
    [((~Stop x:id τ1 ...)
      (~Stop y:id τ2 ...))
     (and
      (free-identifier=? #'x #'y)
      (for/and ([t2 (in-syntax #'(τ2 ...))])
        (for/or ([t1 (in-syntax #'(τ1 ...))])
          (role-implements? t1 t2))))]
    ;; seems like this check might be in the wrong place
    [((~Sends τ-m1)
      (~Sends τ-m2))
     (<: #'τ-m1 #'τ-m2)]
    [((~Actor _)
      (~Actor _))
     ;; spawned actor OK in specified dataspace
     (<: r spec)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DesugaredSyntax EffectName -> (Syntaxof Effect ...)
(define-for-syntax (get-effect e- eff)
  (or (syntax-property e- eff) #'()))

;; DesugaredSyntax EffectName -> Bool
(define-for-syntax (effect-free? e- eff)
  (define prop (syntax-property e- eff))
  (or (false? prop) (stx-null? prop)))

;; DesugaredSyntax -> Bool
(define-for-syntax (pure? e-)
  (for/and ([key (in-list '(ν-ep ν-f ν-s))])
    (effect-free? e- key)))

;; (SyntaxOf DesugaredSyntax ...) -> Bool
(define-for-syntax (all-pure? es)
  (stx-andmap pure? es))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lambdas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (lambda ([x:id (~optional (~datum :)) τ:type] ...) body ...+) ≫
  [[x ≫ x- : τ] ... ⊢ (begin body ...) ≫ body- (⇒ : τ-e)
                (⇒ ν-ep (~effs τ-ep ...))
                (⇒ ν-s (~effs τ-s ...))
                (⇒ ν-f (~effs τ-f ...))]
  ----------------------------------------
  [⊢ (lambda- (x- ...) body-) (⇒ : (→ τ ... (Computation (Value τ-e)
                                                         (Endpoints τ-ep ...)
                                                         (Roles τ-f ...)
                                                         (Spawns τ-s ...))))])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (Λ (tv:id ...) e) ≫
  [([tv ≫ tv- :: #%type] ...) () ⊢ e ≫ e- ⇒ τ]
  --------
  ;; can't use internal mk-∀- constructor here
  ;; - will cause the bound-id=? quirk to show up
  ;;   (when subsequent tyvar refs are expanded with `type` stx class)
  ;; - requires converting type= and subst to use free-id=?
  ;;   (which is less performant)
  [⊢ e- ⇒ (∀ (tv- ...) τ)])

(define-typed-syntax inst
  [(_ e τ:type ...) ≫
   #:fail-unless (stx-andmap instantiable? #'(τ.norm ...))
                 "types must be instantiable"
   [⊢ e ≫ e- ⇒ (~∀ tvs τ_body)]
   #:fail-unless (pure? #'e-) "expression must be pure"
   --------
   [⊢ e- ⇒ #,(substs #'(τ.norm ...) #'tvs #'τ_body)]]
  [(_ e) ≫ --- [≻ e]])

;; Type -> Bool
;; determine if a type is suitable for instantiating a variable
(define-for-syntax (instantiable? ty)
  (and (flat-type? ty)
       (finite? ty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequencing & Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For Debugging
(define-for-syntax DEBUG-BINDINGS? #f)

(define-for-syntax (int-def-ctx-bind-type-rename x x- t ctx)
  (when DEBUG-BINDINGS?
    (printf "adding to context ~a\n" (syntax-debug-info x)))
  (syntax-local-bind-syntaxes (list x-) #f ctx)
  (syntax-local-bind-syntaxes (list x)
                              #`(make-rename-transformer
                                 (add-orig (assign-type #'#,x- #'#,t #:wrap? #f) #'#,x))
                              ctx))

(define-for-syntax (add-bindings-to-ctx e- def-ctx)
  (syntax-parse e-
        #:literals (erased field/intermediate define/intermediate begin-)
        [(erased (field/intermediate (x:id x-:id τ e-) ...))
         (for ([orig-name (in-syntax #'(x ... ))]
               [new-name (in-syntax #'(x- ...))]
               [field-ty (in-syntax #'(τ ...))])
           (int-def-ctx-bind-type-rename orig-name new-name field-ty def-ctx))]
        [(erased (define/intermediate x:id x-:id τ e-))
         (int-def-ctx-bind-type-rename #'x #'x- #'τ def-ctx)]
        #;[(erased (begin- e ...))
         (for ([e (in-syntax #'(e ...))])
           (add-bindings-to-ctx e def-ctx))]
        [_ (void)]))

(define-for-syntax (display-ctx-bindings ctx)
  (printf "context:\n")
  (for ([x (in-list (internal-definition-context-binding-identifiers ctx))])
    (printf ">>~a\n" (syntax-debug-info x))))

;; -> (Values e-... (Listof Type) (Listof EndpointEffects) (Listof FacetEffects) (Listof SpawnEffects))
;; recognizes local binding forms
;; (field/intermediate [x e] ...
;; (define/intermediate x x- τ e)
(define-for-syntax (walk/bind e...
                              [def-ctx (syntax-local-make-definition-context)]
                              [unique (gensym 'walk/bind)])
  (define-values (rev-e-... rev-τ... ep-effects facet-effects spawn-effects)
    (let loop ([e... (syntax->list e...)]
               [rev-e-... '()]
               [rev-τ... '()]
               [ep-effects '()]
               [facet-effects '()]
               [spawn-effects '()])
      (match e...
        ['()
         (values rev-e-... rev-τ... ep-effects facet-effects spawn-effects)]
        [(cons e more)
         (when (and DEBUG-BINDINGS?
                    (identifier? e))
           (display-ctx-bindings def-ctx)
           (printf "expanding ~a\n" (syntax-debug-info e)))
         (define e- (local-expand e (list unique) (list #'erased #'begin) def-ctx))
         (syntax-parse e-
           #:literals (begin)
           [(begin e ...)
            (loop (append (syntax->list #'(e ...)) more)
                  rev-e-...
                  rev-τ...
                  ep-effects
                  facet-effects
                  spawn-effects)]
           [_
            (define τ (syntax-property e- ':))
            (define-values (ep-effs f-effs s-effs)
              (values (syntax->list (get-effect e- 'ν-ep))
                      (syntax->list (get-effect e- 'ν-f))
                      (syntax->list (get-effect e- 'ν-s))))
            (add-bindings-to-ctx e- def-ctx)
            (loop more
                  (cons e- rev-e-...)
                  (cons τ rev-τ...)
                  (append ep-effs ep-effects)
                  (append f-effs facet-effects)
                  (append s-effs spawn-effects))])])))
  (values (reverse rev-e-...)
          (reverse rev-τ...)
          ep-effects
          facet-effects
          spawn-effects))

(define-syntax (field/intermediate stx)
  (syntax-parse stx
    [(_ [x:id x-:id τ e-] ...)
     #'(syndicate:field [x- e-] ...)]))


(define-syntax (define/intermediate stx)
  (syntax-parse stx
    [(_ x:id x-:id τ e)
     ;; including a syntax binding for x allows for module-top-level references
     ;; (where walk/bind won't replace further uses) and subsequent provides
     #'(begin-
         (define-syntax x
           (make-variable-like-transformer (add-orig (assign-type #'x- #'τ #:wrap? #f) #'x)))
         (define- x- e))]))

;; copied from ext-stlc
(define-typed-syntax define
  [(_ x:id (~datum :) τ:type e:expr) ≫
   [⊢ e ≫ e- ⇐ τ.norm]
   #:fail-unless (pure? #'e-) "expression must be pure"
   #:with x- (generate-temporary #'x)
   #:with x+ (syntax-local-identifier-as-binding #'x)
   --------
   [⊢ (define/intermediate x+ x- τ.norm e-) (⇒ : ★/t)]]
  [(_ x:id e) ≫
   ;This won't work with mutually recursive definitions
   [⊢ e ≫ e- ⇒ τ]
   #:fail-unless (pure? #'e-) "expression must be pure"
   #:with x- (generate-temporary #'x)
   #:with x+ (syntax-local-identifier-as-binding #'x)
   --------
   [⊢ (define/intermediate x+ x- τ e-) (⇒ : ★/t)]]
  [(_ (f [x (~optional (~datum :)) ty:type] ...
         (~or (~datum →) (~datum ->)) ty_out:type)
         e ...+) ≫
   [⊢ (lambda ([x : ty] ...) (begin e ...)) ≫ e- (⇒ : (~and fun-ty
                                                            (~→ (~not (~Computation _ ...)) ...
                                                                (~Computation (~Value τ-v)
                                                                              _ ...))))]
   #:fail-unless (<: #'τ-v #'ty_out.norm)
     (format "expected different return type\n got ~a\n expected ~a\n"
       #'τ-v #'ty_out
       #;(type->str #'τ-v)
       #;(type->str #'ty_out))
   #:with f- (add-orig (generate-temporary #'f) #'f)
   --------
   [⊢ (define/intermediate f f- fun-ty e-) (⇒ : ★/t)]]
  [(_ (f [x (~optional (~datum :)) ty] ...)
         e ...+) ≫
   ----------------------------
   [≻ (define (f [x ty] ... -> ★/t) e ...)]]
  ;; Polymorphic definitions
  [(_ ((~datum ∀) (X:id ...)
                  (f [x (~optional (~datum :)) ty] ...
                     (~or (~datum →) (~datum ->)) ty_out))
      e ...+) ≫
   #:with e+ #'(Λ (X ...)
                  (lambda ([x : ty] ...)
                    (begin e ...)))
   [[X ≫ X- :: #%type] ... ⊢ e+ ≫ e-
                       (⇒ : (~and res-ty
                                  (~∀ (Y ...)
                                      (~→ (~not (~Computation _ ...)) ...
                                          (~Computation (~Value τ-v)
                                                        _ ...)))))]
   #:fail-unless (<: (type-eval #'(∀ (Y ...) τ-v))
                     (type-eval #'(∀ (X ...) ty_out)))
                 (format "expected different return type\n got ~a\n expected ~a\n"
                         #'τ-v #'ty_out)
   #:with f- (add-orig (generate-temporary #'f) #'f)
   -------------------------------------------------------
   [⊢ (define/intermediate f f- res-ty e-) (⇒ : ★/t)]]
  [(_ ((~datum ∀) (X:id ...)
                  (f [x (~optional (~datum :)) ty] ...))
      e ...+) ≫
   --------------------------------------------------
   [≻ (define (∀ (X ...) (f [x ty] ... -> ★/t)) e ...)]])

(define-typed-syntax begin
  [(_ e_unit ... e) ≫
   #:do [(define-values (e-... τ... ep-effs f-effs s-effs) (walk/bind #'(e_unit ... e)))]
   #:with τ (last τ...)
   --------
   [⊢ (let- () #,@e-...) (⇒ : τ)
      (⇒ ν-ep (#,@ep-effs))
      (⇒ ν-f (#,@f-effs))
      (⇒ ν-s (#,@s-effs))]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequencing & Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax #%app
  ;; Polymorphic, Pure Function - Perform Local Inference
  [(_ e_fn e_arg ...) ≫
   ;; compute fn type (ie ∀ and →)
   [⊢ e_fn ≫ e_fn- ⇒ (~∀ Xs (~→fn tyX_in ... tyX_out))]
   ;; successfully matched a polymorphic fn type, don't backtrack
   #:cut
   #:with tyX_args #'(tyX_in ... tyX_out)
   ;; solve for type variables Xs
   #:with [[e_arg- ...] Xs* cs] (solve #'Xs #'tyX_args this-syntax)
   ;; make sure types are legal
   #:with tyXs (inst-types/cs #'Xs* #'cs #'Xs)
   #:fail-unless (for/and ([ty (in-syntax #'tyXs)])
                   (instantiable? ty))
                 "type variables must be flat and finite"
   ;; instantiate polymorphic function type
   #:with [τ_in ... τ_out] (inst-types/cs #'Xs* #'cs #'tyX_args)
   #:with (unsolved-X ...) (find-free-Xs #'Xs* #'τ_out)
   ;; arity check
   #:fail-unless (stx-length=? #'[τ_in ...] #'[e_arg ...])
                 (num-args-fail-msg #'e_fn #'[τ_in ...] #'[e_arg ...])
   ;; purity check
   #:fail-unless (all-pure? #'(e_fn- e_arg- ...)) "expressions must be pure"
   ;; compute argument types
   #:with (τ_arg ...) (stx-map typeof #'(e_arg- ...))
   ;; typecheck args
   [τ_arg τ⊑ τ_in #:for e_arg] ...
   #:with τ_out* (if (stx-null? #'(unsolved-X ...))
                     #'τ_out
                     (syntax-parse #'τ_out
                       [(~?∀ (Y ...) τ_out)
                        #:fail-unless (→? #'τ_out)
                        (mk-app-poly-infer-error this-syntax #'(τ_in ...) #'(τ_arg ...) #'e_fn)
                        (for ([X (in-list (syntax->list #'(unsolved-X ...)))])
                          (unless (covariant-X? X #'τ_out)
                            (raise-syntax-error
                             #f
                             (mk-app-poly-infer-error this-syntax #'(τ_in ...) #'(τ_arg ...) #'e_fn)
                             this-syntax)))
                        (mk-∀- #'(unsolved-X ... Y ...) #'(τ_out))]))
   --------
   [⊢ (#%plain-app- e_fn- e_arg- ...) ⇒ τ_out*]]
  ;; All Other Functions
  [(_ e_fn e_arg ...) ≫
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
      (⇒ ν-ep (τ-ep ...))
      (⇒ ν-s (τ-s ...))
      (⇒ ν-f (τ-f ...))]])

(begin-for-syntax
  ;; find-free-Xs : (Stx-Listof Id) Type -> (Listof Id)
  ;; finds the free Xs in the type
  (define (find-free-Xs Xs ty)
    (for/list ([X (in-stx-list Xs)]
               #:when (stx-contains-id? ty X))
      X))

  ;; (SyntaxListOf ID) Type -> Bool
  ;; checks if the type contains any variables under unions
  (define (tyvar-under-union? Xs ty)
    (syntax-parse ty
      [(~U* _ ...)
       (for/or ([X (in-syntax Xs)])
         (stx-contains-id? ty X))]
      [(~Base _) #f]
      [X:id #f]
      [(~Any/bvs _ _ τ ...)
       (for/or ([ty2 (in-syntax #'(τ ...))])
         (tyvar-under-union? Xs ty2))]
      [_
       (type-error #:src (get-orig ty)
                   #:msg "tyvar-under-union?: unrecognized-type: ~a"
                   ty)]))

  ;; solve for Xs by unifying quantified fn type with the concrete types of stx's args
  ;;   stx = the application stx = (#%app e_fn e_arg ...)
  ;;   tyXs = input and output types from fn type
  ;;          ie (typeof e_fn) = (-> . tyXs)
  ;; It infers the types of arguments from left-to-right,
  ;; and it expands and returns all of the arguments.
  ;; It returns list of 3 values if successful, else throws a type error
  ;;  - a list of all the arguments, expanded
  ;;  - a list of all the type variables
  ;;  - the constraints for substituting the types
  (define (solve Xs tyXs stx)
    (syntax-parse tyXs
      [(τ_inX ... τ_outX)
       ;; generate initial constraints with expected type and τ_outX
       #:with (~?∀ Vs expected-ty)
              (and (get-expected-type stx)
                   ((current-type-eval) (get-expected-type stx)))
       (define initial-cs
         (if (and (syntax-e #'expected-ty) (stx-null? #'Vs))
             (add-constraints Xs '() (list (list #'expected-ty #'τ_outX)))
             '()))
       (syntax-parse stx
         [(_ e_fn . args)
          (define-values (as- cs)
              (for/fold ([as- null] [cs initial-cs])
                        ([a (in-stx-list #'args)]
                         [tyXin (in-stx-list #'(τ_inX ...))])
                (define ty_in (inst-type/cs/orig Xs cs tyXin datum=?))
                (when (tyvar-under-union? Xs ty_in)
                  (type-error #:src a
                              #:msg (format "can't infer types with unions: ~a\nraw: ~a"
                                            (type->str ty_in) ty_in)))
                (define/with-syntax [a- ty_a]
                  (infer+erase (if (null? (find-free-Xs Xs ty_in))
                                   (add-expected-type a ty_in)
                                   a)))
                (when (tyvar-under-union? Xs #'ty_a)
                  (type-error #:src a
                              #:msg (format "can't infer types with unions: ~a\nraw: ~a"
                                            (type->str #'ty_a) #'ty_a)))
                (values
                 (cons #'a- as-)
                 (add-constraints Xs cs (list (list ty_in #'ty_a))
                                  (list (list (inst-type/cs/orig
                                               Xs cs ty_in
                                               datum=?)
                                              #'ty_a))))))

          (list (reverse as-) Xs cs)])]))

  (define (mk-app-poly-infer-error stx expected-tys given-tys e_fn)
    (format (string-append
             "Could not infer instantiation of polymorphic function ~s.\n"
             "  expected: ~a\n"
             "  given:    ~a")
            (syntax->datum (get-orig e_fn))
            (string-join (stx-map type->str expected-tys) ", ")
            (string-join (stx-map type->str given-tys) ", ")))

  ;; covariant-Xs? : Type -> Bool
  ;; Takes a possibly polymorphic type, and returns true if all of the
  ;; type variables are in covariant positions within the type, false
  ;; otherwise.
  (define (covariant-Xs? ty)
    (syntax-parse ((current-type-eval) ty)
      [(~?∀ Xs ty)
       (for/and ([X (in-stx-list #'Xs)])
         (covariant-X? X #'ty))]))

  ;; find-X-variance : Id Type [Variance] -> Variance
  ;; Returns the variance of X within the type ty
  (define (find-X-variance X ty [ctxt-variance covariant])
    (car (find-variances (list X) ty ctxt-variance)))

  ;; covariant-X? : Id Type -> Bool
  ;; Returns true if every place X appears in ty is a covariant position, false otherwise.
  (define (covariant-X? X ty)
    (variance-covariant? (find-X-variance X ty covariant)))

  ;; contravariant-X? : Id Type -> Bool
  ;; Returns true if every place X appears in ty is a contravariant position, false otherwise.
  (define (contravariant-X? X ty)
    (variance-contravariant? (find-X-variance X ty covariant)))

  ;; find-variances : (Listof Id) Type [Variance] -> (Listof Variance)
  ;; Returns the variances of each of the Xs within the type ty,
  ;; where it's already within a context represented by ctxt-variance.
  (define (find-variances Xs ty [ctxt-variance covariant])
    (syntax-parse ty
      [A:id
       (for/list ([X (in-list Xs)])
         (cond [(free-identifier=? X #'A) ctxt-variance]
               [else irrelevant]))]
      [(~Any tycons)
       (stx-map (λ _ irrelevant) Xs)]
      [(~?∀ () (~Any tycons τ ...))
       #:when (get-arg-variances #'tycons)
       #:when (stx-length=? #'[τ ...] (get-arg-variances #'tycons))
       (define τ-ctxt-variances
         (for/list ([arg-variance (in-list (get-arg-variances #'tycons))])
           (variance-compose ctxt-variance arg-variance)))
       (for/fold ([acc (stx-map (λ _ irrelevant) Xs)])
                 ([τ (in-stx-list #'[τ ...])]
                  [τ-ctxt-variance (in-list τ-ctxt-variances)])
         (map variance-join
              acc
              (find-variances Xs τ τ-ctxt-variance)))]
      [ty
       #:when (not (for/or ([X (in-list Xs)])
                     (stx-contains-id? #'ty X)))
       (stx-map (λ _ irrelevant) Xs)]
      [_ (stx-map (λ _ invariant) Xs)])))
