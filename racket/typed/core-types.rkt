#lang turnstile

(provide (except-out (all-defined-out) → ∀ Role)
         (rename-out [→+ →]
                     [∀+ ∀]
                     [Role+Body Role])
         (for-syntax (except-out (all-defined-out) ~→ ~∀ ~Role)
                     (rename-out [~→+ ~→]
                                 [~∀+ ~∀]
                                 [~Role+Body ~Role]))
         (for-meta 2 (all-defined-out)))
(require (only-in turnstile
                  [define-type-constructor define-type-constructor-]
                  [type? type?-]
                  [get-arg-variances get-arg-variances-]))

(require turnstile/typedefs)
(begin-for-syntax
  ;; turnstile/typedefs sets it to #t, which breaks things
  (current-use-stop-list? #f))

(require (prefix-in syndicate: syndicate/actor-lang))

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx syntax/parse racket/base))
(require (for-syntax turnstile/examples/util/filter-maximal))
(require (for-syntax (prefix-in ttc: turnstile/type-constraints)
                     (prefix-in mtc: macrotypes/type-constraints)
                     (prefix-in mvc: macrotypes/variance-constraints)))
#;(require (only-in (for-syntax macrotypes/typecheck-core) get-orig))
(require (for-syntax racket/struct-info
                     syntax/id-table))
(require macrotypes/postfix-in)
(require (rename-in racket/math [exact-truncate exact-truncate-]))
(require (postfix-in - racket/list))
(require (postfix-in - racket/set))
(require (postfix-in - racket/match))
(require (postfix-in - (only-in racket/format ~a)))
(require (for-syntax "syntax-serializer.rkt"))


(module+ test
  (require rackunit)
  (require rackunit/turnstile))

(begin-for-syntax
  (current-use-stop-list? #f))

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

(begin-for-syntax
  (struct type-metadata (isec cons) #:transparent)
  ;; (MutableHashOf Symbol type-metadata)
  (define TypeInfo# (make-hash))
  ;; Identifier isect-desc TypeCons -> Void
  (define (set-type-info! ty-cons isec cons)
    (when (hash-has-key? TypeInfo# ty-cons)
      ;; TODO
      #f)
    (hash-set! TypeInfo#
               ty-cons
               (type-metadata isec cons)))
  ;; Identifier -> Symbol
  ;; XYZ-.*
  ;; based on the convention used by turnstile *shrug*
  (define (un- id)
    (define match?
      (regexp-match #px"^(\\S*)-\\S*$" (symbol->string (syntax-e id))))
    (and match? (string->symbol (second match?))))

  ;; Identifier -> (U #f type-metadata)
  (define (get-type-info ty-cons)
    (hash-ref TypeInfo# (un- ty-cons) #f))

  ;; Identifier -> (U #f isec-desc)
  (define (get-type-isec-desc ty-cons)
    (define result? (get-type-info ty-cons))
    (and result? (type-metadata-isec result?)))
  ;; Identifier -> (U #f TypeCons)
  (define (get-type-cons ty-cons)
    (define result? (get-type-info ty-cons))
    (and result? (type-metadata-cons result?)))

  ;; a isect-desc describes how a type (constructor) behaves with respect to
  ;; intersection, and is one of
  ;;   - BASE
  ;;   - CONTAINER-LIKE
  ;;   - PRODUCT-LIKE
  (define BASE 'base)
  (define CONTAINER-LIKE 'container-like)
  (define PRODUCT-LIKE 'product-like)

  ;; syntax property key
  #;(define isect-desc-key
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

  ;; Identifier -> Bool
  ;; check if the type has a syntax property allowing us to create new instances
  (define (reassemblable? t)
    (and (get-type-cons t) #t))

  ;; Identifier (Listof Type) -> Type
  ;; Create a new instance of the type with the given arguments
  ;; needs to have the type-cons-key
  (define (reassemble-type ty args)
    (define tycons (get-type-cons ty))
    (unless tycons
      (error "expected to find type-cons-key"))
    (tycons args)))

(begin-for-syntax
  (define ((mk-ctor-rewriter Name-) stx)
    (syntax-parse stx
      [(_ . ts)
       (quasisyntax/loc stx
         (#,Name- . ts))]))
  (begin-for-syntax
    (define ((mk-ctor-rewriter Name-) stx)
      (syntax-parse stx
        [(_ . ts)
         (quasisyntax/loc stx
           (#,Name- . ts))])))
  )

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
         (define-type-constructor Name-
           #:arity op arity
           #:arg-variances variances
           #,@(if (attribute extra-info)
                  #'(#:extra-info extra-info)
                  #'()))
         (define-syntax Name (mk-ctor-rewriter #'Name-))
         (begin-for-syntax
           (set-type-info! 'Name '#,(attribute desc.val) mk-)
           (define-syntax NamePat
             (pattern-expander
              (mk-ctor-rewriter #'NamePat-))))
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

;; Define a type constructor that acts like a product:
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

(define-type Type : Type)

(begin-for-syntax

  (define (Type? stx)
    (syntax-parse stx
      [~Type #t]
      [_ #f]))

  (define (new-type? t)
    (or (type?- t)
        (Type? (detach t ':))))
  #;(require racket/trace)
  #;(trace new-type?)

  (current-type? new-type?))

(begin-for-syntax
  (define-generic-type-method get-arg-variances-data #:default #f)
  (define-generic-type-method get-extra-info-data #:default #f)

  (define (retrieve/apply meth ty)
    (define fn (meth ty))
    (and fn
         (syntax-parse ty
           [(~Any/new τcons τ ...)
            (fn #'(τcons τ ...))])))

  (define (get-arg-variances/new ty)
    (retrieve/apply get-arg-variances-data ty))

  (define (get-extra-info/new ty)
    (retrieve/apply get-extra-info-data ty))

  (define (get-arg-variances ty)
    (or (get-arg-variances/new ty)
        (get-arg-variances- ty)))


  ;; ID Nat -> (Listof ID)
  (define (make-arity-domain op arity)
    (define prefix (make-list arity #'Type))
    (syntax-parse op #:datum-literals (>= > =)
      [=
       prefix]
      [>
       (append prefix (list #'Type #'Type #'*))]
      [>=
       (append prefix (list #'Type #'*))]))

  ;; PatternExpander (Syntax-Listof ID) ID -> Pattern
  (define ((make-type-recognizer name-) ty)
    (syntax-parse ty
      [(~Any/new τcons . rst)
       (free-identifier=? #'τcons name-)])))

(define-syntax (define-type-constructor stx)
  (syntax-parse stx
    [(_ Name:id #:arity op arity:nat
                (~optional (~seq #:arg-variances variances))
                (~optional (~seq #:extra-info extra-info)))
     #:with Name- (mk-- #'Name)
     #:with mk- (mk-mk #'Name-)
     #:with Name? (mk-? #'Name)
     #:with dom (make-arity-domain #'op (syntax-e #'arity))
     #:do [
           (define arg-var-meth #'(~? (get-arg-variances-data variances)
                                      ()))
           (define extra-info-meth #'(~? (get-extra-info-data extra-info)
                                         ()))
           (define implements? (if (or (attribute variances) (attribute extra-info))
                                   #'(#:implements)
                                   #'()))]
     #`(begin-
         (define-type Name : #,@#'dom -> Type
           #,@implements?
           #,@arg-var-meth
           #,@extra-info-meth)
         (define-for-syntax (mk- args)
           ((current-type-eval) #`(Name #,@args)))
         (define-for-syntax Name?
           (make-type-recognizer #'Name-)))]))

(define-simple-macro (define-base-type Name:id)
  (define-type Name : Type))

(define-simple-macro (define-base-types Name:id ...)
  (begin- (define-base-type Name) ...))

(define-base-types Discard ★/t)

(define-type FacetName : FacetName)

#;(define-type-constructor? Shares #:arity = 1)

#;(define-binding-type Role #:arity >= 0 #:bvs = 1)
(define-type Role #:with-binders [X : FacetName] : Type -> Type)
(define-type RoleBody : Type * -> Type)
(define-type-constructor Shares #:arity = 1)
(define-type-constructor Sends #:arity = 1)
(define-type-constructor Realizes #:arity = 1)
(define-type-constructor Reacts #:arity >= 1)
(define-type-constructor Asserted #:arity = 1)
(define-type-constructor Retracted #:arity = 1)
(define-type-constructor Know #:arity = 1)
(define-type-constructor Forget #:arity = 1)
(define-product-type Realize #:arity = 1)
#;(define-type-constructor Stop #:arity >= 1)
(define-type Stop : FacetName Type * -> Type)
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

#;(define-product-type Message #:arity = 1)
(define-product-type Tuple #:arity >= 0)
#;(define-product-type Observe #:arity = 1)
#;(define-product-type Inbound #:arity = 1)
#;(define-product-type Outbound #:arity = 1)
(define-container-type AssertionSet #:arity = 1)
(define-container-type Patch #:arity = 2)

;; functions and type abstractions
#;(define-binding-type ∀)
(define-type ∀ #:with-binders [X : Type] : Type -> Type)
(define-type-constructor → #:arity > 0)

(define-simple-macro (→+ in ... out)
  (→ out in ...))

(begin-for-syntax
  ;; because rest types are *trailing*, define a convenience pattern expander for var-arity domain of →
  (define-syntax ~→+
    (pattern-expander
     (syntax-parser
       [(_ I ... O)
        #'(~→ O I ...)]))))

(define-syntax-parser ∀+
  [(_ () ty) #'ty]
  [(_ (X:id Y ...) ty)
   #'(∀ (X : Type) (∀+ (Y ...) ty))])

(begin-for-syntax

  (define (flatten-∀ ty)
    (define-values (body vars)
      (let loop ([ty ty]
                 [vars/rev '()])
        (syntax-parse ty
          [(~∀ (X : _) τ)
           (loop #'τ (cons #'X vars/rev))]
          [τ
           (values #'τ (reverse vars/rev))])))
    #`(#,vars #,body))

  (define-syntax ~∀+
    (pattern-expander
     (syntax-parser
       [(_ vars-pat ty-pat)
        #'(~and (~∀ (_ : _) _)
                TY
                (~parse (vars-pat ty-pat) (flatten-∀ #'TY)))]))))

(define-simple-macro (Role+Body (x:id) ty ...)
  (Role (x : FacetName)
        (RoleBody ty ...)))

(begin-for-syntax
  (define-syntax ~Role+Body
    (pattern-expander
     (syntax-parser
       [(_ var-pat . ty-pat)
        (syntax/loc this-syntax
          (~and (~Role (internal-name : _)
                       (~RoleBody . tys))
                (~parse var-pat #'(internal-name))
                (~parse ty-pat #'tys)))]))))


;; for describing the RHS
;; a value and a description of the effects
(define-type-constructor Computation #:arity = 4)
(define-type-constructor Value #:arity = 1)
(define-type-constructor Endpoints #:arity >= 0)
(define-type-constructor Roles #:arity >= 0)
(define-type-constructor Spawns #:arity >= 0)



(define-for-syntax (type-eval t)
  ((current-type-eval) t))

(define-type-constructor U* #:arity >= 0)

(define-for-syntax ((mk-type-alias-rewriter xs body) stx)
  (syntax-parse stx
    [(_ ty ...)
     (type-eval (substs #'(ty ...) xs body))]))

;; τ.norm in 1st case causes "not valid type" error when referring to ⊥ in another file.
;; however, this version expands the type at every reference, incurring a potentially large
;; overhead---2x in the case of book-club.rkt
;; (copied from ext-stlc example)
(define-syntax define-type-alias
  (syntax-parser
    [(_ alias:id τ:type)
     #:with serialized-τ (serialize-syntax #'τ.norm)
     #'(define-syntax- alias
         (make-variable-like-transformer (deserialize-syntax #'serialized-τ)))]
    [(_ (f:id x:id ...) ty)
     #'(define-syntax- f (mk-type-alias-rewriter #'(x ...) #'ty))]))

(define-type-alias ⊥ (U*))
(define-type-alias Unit (Tuple))

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
  (→+ ty-in ... (Computation (Value ty-out)
                             (Endpoints)
                             (Roles)
                             (Spawns))))

(begin-for-syntax
  (define-syntax ~Base
    (pattern-expander
     (syntax-parser
       [(_ nm:id)
        #'((~literal #%plain-app) nm)])))


  (define-syntax ~→fn
    (pattern-expander
     (syntax-parser
       [(_ ty-in:id ... ty-out)
        #'(~→+ ty-in ... (~Computation (~Value ty-out)
                                       (~Endpoints)
                                       (~Roles)
                                       (~Spawns)))])))

  ;; matching possibly polymorphic types
  (define-syntax ~?∀
    (pattern-expander
     (lambda (stx)
       (syntax-case stx ()
         [(?∀ vars-pat body-pat)
          #'(~or (~∀+ vars-pat body-pat)
                 (~and (~not (~∀+ _ _))
                       (~parse vars-pat #'())
                       body-pat))])))))

;; shorthand for writing function types
(define-syntax-parser proc
  [(_ (~optional (~seq #:forall (X:id ...)))
      ty-in ...
      (~or (~datum ->) (~datum →))
      ty-out
      (~or (~optional (~seq #:spawns (s ...+)))
           (~optional (~seq #:roles (r ...+)))
           (~optional (~seq #:endpoints (e ...+))))
      ...)
   #:with spawns (if (attribute s) #'(s ...) #'())
   #:with roles (if (attribute r) #'(r ...) #'())
   #:with endpoints (if (attribute e) #'(e ...) #'())
   #:with body #`(→+ ty-in ... (Computation (Value ty-out)
                                            (Endpoints #,@#'endpoints)
                                            (Roles #,@#'roles)
                                            (Spawns #,@#'spawns)))
   (if (attribute X)
       #'(∀+ (X ...) body)
       #'body)])

(begin-for-syntax
  (define-syntax ~proc
    (pattern-expander
     (syntax-parser
       [(_ (~optional (~seq #:forall (X:id ...)))
           ty-in ...
           (~or (~datum ->) (~datum →))
           ty-out
           (~or (~optional (~seq #:spawns s))
                (~optional (~seq #:roles r))
                (~optional (~seq #:endpoints e)))
           ...)
        #:with spawns (if (attribute s) #'(s) #'())
        #:with roles (if (attribute r) #'(r) #'())
        #:with endpoints (if (attribute e) #'(e) #'())
        #:with body #`(~→+ ty-in ... (~Computation (~Value ty-out)
                                                  (~Endpoints #,@#'endpoints)
                                                  (~Roles #,@#'roles)
                                                  (~Spawns #,@#'spawns)))
        (if (attribute X)
            #'(~∀+ (X ...) body)
            #'body)]))))

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

  (struct user-ctor (typed-ctor untyped-ctor type-tag)
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

(begin-for-syntax
  (define ((mk-type-params-fetcher TypeCons) ty)
    (syntax-parse ty
      [(_ (~Any/new τcons t ...))
       #:when (free-identifier=? #'τcons TypeCons)
       #'(t ...)]))

  (define ((mk-constructor-type-rule arity StructName TypeCons) stx)
    (syntax-parse/typecheck stx
      [(_ e ...) ≫
       #:fail-unless (= arity (stx-length #'(e ...))) "arity mismatch"
       [⊢ e ≫ e- (⇒ : τ)] ...
       #:fail-unless (all-pure? #'(e- ...)) "expressions must be pure"
       ----------------------
       [⊢ (#%app- #,StructName e- ...) (⇒ : (#,TypeCons τ ...))]])))

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
         (define-for-syntax (TypeConsExtraInfo stx)
           (list #'type-tag #'MakeTypeCons #'GetTypeParams)
           #;(syntax-parse stx
             [(_ X (... ...)) #'(#%app- 'type-tag 'MakeTypeCons 'GetTypeParams)]))
         (define-product-type TypeCons
           #:arity = #,arity
           #:extra-info TypeConsExtraInfo)
         (define-type-alias Alias AliasBody) ...
         (define-syntax MakeTypeCons (mk-ctor-rewriter #'TypeCons))
         (define-syntax GetTypeParams (mk-type-params-fetcher #'TypeCons))
         (define-syntax Cons
           (user-ctor #'Cons- #'StructName 'type-tag))
         (define-syntax Cons- (mk-constructor-type-rule #,arity #'StructName #'TypeCons)))]))

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
           (define-for-syntax (TypeConsExtraInfo stx)
             (list #'type-tag #'MakeTypeCons #'GetTypeParams)
             )
           (define-product-type TypeCons
             ;; issue: arity needs to parse as an exact-nonnegative-integer
             ;; fix: check arity in MakeTypeCons
             #:arity >= 0
             #:extra-info TypeConsExtraInfo)
           (define-syntax MakeTypeCons (mk-ctor-rewriter #'TypeCons))
           (define-syntax GetTypeParams (mk-type-params-fetcher #'TypeCons))
           (define-syntax Cons- (mk-constructor-type-rule arity #'orig-struct-info #'TypeCons))
           (define-syntax ucons
             (user-ctor #'Cons- #'orig-struct-info 'type-tag)))))]))

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
                (~Any/new _ . rst))])))

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
    (get-extra-info/new (type-eval t)))

  (define (get-type-tag t)
    (match (get-extra-info/new t)
      [(list tag _ _) tag]))

  (define (get-type-args t)
    (match  (get-extra-info/new t)
      [(list _ _ get)
       (define f (syntax-local-value get))
       (syntax->list (f #`(#,get #,t)))]))

  (define (make-cons-type t args)
    (match  (get-extra-info/new t)
      [(list _ mk _)
       (define f (syntax-local-value mk))
       (type-eval (f #`(#,mk #,@args)))]))

  (define (ctor-id? stx)
    (and (identifier? stx)
         (user-ctor? (syntax-local-value stx (const #f)))))

  (define (untyped-ctor stx)
    (user-ctor-untyped-ctor (syntax-local-value stx (const #f))))

  ;; requires (ctor-id? stx)
  ;; fetch the type tag
  (define (ctor-type-tag stx)
    (user-ctor-type-tag (syntax-local-value stx (const #f)))))

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
     #:with (serialized-ty ...) (for/list ([t (in-syntax #'(ty.norm ...))])
                                  (serialize-syntax t))
     (syntax/loc stx
       (begin-
         (require (only-in lib [name name-] ...))
         (define-syntax name
           (make-variable-like-transformer
            (add-orig (assign-type #'name- (deserialize-syntax #'serialized-ty)
                                   #:wrap? #f) #'name)))
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

(require-struct observe #:as Observe #:from syndicate/actor-lang)
(require-struct inbound #:as Inbound #:from syndicate/actor-lang)
(require-struct outbound #:as Outbound #:from syndicate/actor-lang)
(require-struct message #:as Message #:from syndicate/actor-lang)

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
  ((current-typecheck-relation) t (mk-U*- '())))

(define-for-syntax bot
  #;#'(U)
  (mk-U*- '()))

(define-for-syntax (flat-type? τ)
  (syntax-parse τ
    [(~→+ i ... o) #f]
    [(~Actor τ) #f]
    [(~Role+Body (_) _ ...) #f]
    [_ #t]))

(define-for-syntax (strip-? t)
  (syntax-parse t
    [(~U* τ ...) (mk-U- (stx-map strip-? #'(τ ...)))]
    [~★/t (type-eval #'★/t)]
    ;; since (Observe X) can match (Message X):
    ;; doing this specifically for the intersection operation in the spawn rule, need to check other
    ;; uses
    [(~Observe τ) (mk-U- (list #'τ (mk-Message- #'(τ))))]
    [_ (mk-U*- '())]))

;; similar to strip- fns, but leave non-message types as they are
(define-for-syntax (prune-message t)
  (syntax-parse t
    [(~U* τ ...) (mk-U- (stx-map prune-message #'(τ ...)))]
    [~★/t (type-eval #'★/t)]
    [(~Message τ) #'τ]
    [_ t]))

(define-for-syntax (strip-inbound t)
  (syntax-parse t
    [(~U* τ ...) (mk-U- (stx-map strip-inbound #'(τ ...)))]
    [~★/t (type-eval #'★/t)]
    [(~Inbound τ) #'τ]
    [_ (mk-U*- '())]))

(define-for-syntax (strip-outbound t)
  (syntax-parse t
    [(~U* τ ...) (mk-U- (stx-map strip-outbound #'(τ ...)))]
    [~★/t (type-eval #'★/t)]
    [(~Outbound τ) #'τ]
    [_ (mk-U*- '())]))

(define-for-syntax (relay-interests t)
  (syntax-parse t
    [(~U* τ ...) (mk-U- (stx-map relay-interests #'(τ ...)))]
    [~★/t (type-eval #'★/t)]
    [(~Observe (~Inbound τ)) (mk-Observe- #'(τ))]
    [_ (mk-U*- '())]))

;; (SyntaxOf RoleType ...) -> (Syntaxof InputType OutputType SpawnType)
(define-for-syntax (analyze-roles rs)
  (define-values (lis los lis/i los/i lss)
    (for/fold ([is '()]
               [os '()]
               [is/i '()]
               [os/i '()]
               [ss '()])
              ([r (in-syntax rs)])
      (define-values (i o i/i o/i s) (analyze-role-input/output r))
      (values (cons i is) (cons o os) (cons i/i is/i) (cons o/i os/i) (cons s ss))))
  #`(#,(mk-U- lis)
     #,(mk-U- los)
     #,(mk-U- lis/i)
     #,(mk-U- los/i)
     #,(mk-U- lss)))

;; Wanted test case, but can't use it bc it uses things defined for-syntax
#;(module+ test
 (let ([r (type-eval #'(Role+Body (x) (Shares Int)))])
   (syntax-parse (analyze-role-input/output r)
     [(τ-i τ-o)
      (check-true (type=? #'τ-o (type-eval #'Int)))])))

;; RoleType -> (Values ExternalInputType ExternalOutputType
;;                     InternalInputType InternalOutputType
;;                     SpawnType)
(define-for-syntax (analyze-role-input/output t)
  (syntax-parse t
    [(~Branch (~Effs τ-r ...) ...)
     #:with (τ-i τ-o τ-i/i τ-o/i τ-a) (analyze-roles #'(τ-r ... ...))
     (values #'τ-i #'τ-o #'τ-i/i #'τ-o/i #'τ-a)]
    [(~Stop name:id τ-r ...)
     #:with (τ-i τ-o τ-i/i τ-o/i τ-a) (analyze-roles #'(τ-r ...))
     (values #'τ-i #'τ-o #'τ-i/i #'τ-o/i #'τ-a)]
    [(~Actor τc)
     (values bot bot bot bot t)]
    [(~Sends τ-m)
     (values bot (mk-Message- #'(τ-m)) bot bot bot)]
    [(~Realizes τ-m)
     (values bot bot bot (mk-Realize- #'(τ-m)) bot)]
    [(~Role+Body (name:id)
       (~or (~Shares τ-s)
            (~Know τ-k)
            (~Sends τ-m)
            (~Realizes τ-rlz)
            (~Reacts τ-if τ-then ...)) ...
       (~and (~Role+Body _ _ ...) sub-role) ...)
     #:with (msg ...) (for/list ([m (in-syntax #'(τ-m ...))])
                        (mk-Message- (list m)))
     #:with (rlz ...) (for/list ([r (in-syntax #'(τ-rlz ...))])
                        (mk-Realize- (list r)))
     (define-values (is/e os/e is/i os/i ss)
       (for/fold ([ins '()]
                  [outs '()]
                  [ins/int '()]
                  [outs/int '()]
                  [spawns '()])
                 ([t (in-syntax #'(τ-then ... ... sub-role ...))])
         (define-values (i o i/i o/i s) (analyze-role-input/output t))
         (values (cons i ins) (cons o outs) (cons i/i ins/int) (cons o/i outs/int) (cons s spawns))))
     (define-values (ifs/ext ifs/int) (partition external-evt? (stx->list #'(τ-if ...))))
     (define pat-types/ext (map event-desc-type ifs/ext))
     (define pat-types/int (map event-desc-type ifs/int))
     (values (mk-U- #`(#,@is/e #,@pat-types/ext))
             (mk-U- #`(τ-s ... msg ... #,@os/e #,@(map pattern-sub-type pat-types/ext)))
             (mk-U- #`(#,@is/i #,@pat-types/int))
             (mk-U- #`(τ-k ... rlz ... #,@os/i #,@(map pattern-sub-type pat-types/int)))
             (mk-U- ss))]))

;; EventType -> Bool
;; recognize external events (assertions and messages)
(define-for-syntax (external-evt? evt)
  (syntax-parse evt
    [(~Asserted τ) #t]
    [(~Retracted τ) #t]
    [(~Message τ) #t]
    [_ #f]))

;; EventDescriptorType -> Type
(define-for-syntax (event-desc-type desc)
  (syntax-parse desc
    [(~Asserted τ) #'τ]
    [(~Retracted τ) #'τ]
    [(~Message τ) desc]
    [(~Know τ) #'τ]
    [(~Forget τ) #'τ]
    [(~Realize τ) desc]
    [_ (mk-U*- '())]))

;; PatternType -> Type
(define-for-syntax (pattern-sub-type pt)
  (syntax-parse pt
    [(~or (~Message τ)
          (~Realize τ))
     (define t (replace-bind-and-discard-with-★ #'τ))
     (mk-Observe- (list t))]
    [τ
     #:when (bot? #'τ)
     #'τ]
    [τ
     (define t (replace-bind-and-discard-with-★ #'τ))
     (mk-Observe- (list t))]))

;; TODO : can potentially use something like `subst` for this
(define-for-syntax (replace-bind-and-discard-with-★ t)
  (syntax-parse t
    [(~Bind _)
     (type-eval #'★/t)]
    [~Discard
     (type-eval #'★/t)]
    [(~U* τ ...)
     (mk-U- (stx-map replace-bind-and-discard-with-★ #'(τ ...)))]
    [(~Any/new τ-cons τ ...)
     #:when (reassemblable? #'τ-cons)
     (define subitems (for/list ([t (in-syntax #'(τ ...))])
                        (replace-bind-and-discard-with-★ t)))
     (reassemble-type #'τ-cons subitems)]
    [_ t]))

;; Type -> String
(define-for-syntax (type->strX ty)
  ;; Identifier -> String
  ;; this won't work for any names with numbers in them :\
  (define (un-gensym x)
    (define GENSYMED #px"^(\\D*)\\d*$")
    (second (regexp-match GENSYMED (symbol->string (syntax-e x)))))
  ;; (Listof String) -> String
  (define (paren-join xs)
    (string-join xs
                 #:before-first "("
                 #:after-last ")"))
  (syntax-parse ty
    [X:id
     (un-gensym #'X)]
    [(~U* τ ...)
     (paren-join (cons "U" (stx-map type->strX #'(τ ...))))]
    [(~Base x)
     (un-gensym #'x)]
    [(~Role+Body (x:id) τ ...)
     (define nm (un-gensym #'x))
     (define body (stx-map type->strX #'(τ ...)))
     (paren-join (list* "Role" (format "(~a)" nm) body))]
    [(~∀+ (X ...) τ)
     (define vars
       (paren-join (stx-map type->strX #'(X ...))))
     (paren-join (list "∀" vars (type->strX #'τ)))]
    [(~Any/new τ-cons τ ...)
     (define ctor (un-gensym #'τ-cons))
     (define body (stx-map type->strX #'(τ ...)))
     (paren-join (cons ctor body))]
    [(~Any/bvs τ-cons (X ...) τ ...)
     (define ctor (un-gensym #'τ-cons))
     (define body (stx-map type->strX #'(τ ...)))
     (define desc
       (cond
         [(empty? (syntax->list #'(X ...)))
          (list* ctor body)]
         [else
          (define vars
            (paren-join (stx-map type->strX #'(X ...))))
          (list* ctor vars body)]))
     (paren-join desc)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping and Judgments on Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define trace-sub? (make-parameter #f))

 ;; Type Type -> Bool
 ;; subtyping
 (define (<: t1 t2)
   (when (trace-sub?)
     (unless (syntax-parse t1
               [~Type #t]
               [_ #f])
       (printf "~a\n<:\n~a\n" t1 t2)))
   (syntax-parse #`(#,t1 #,t2)
     [(_ ~★/t)
      (flat-type? t1)]
     [((~U* τ1 ...) _)
      (stx-andmap (lambda (t) (<: t t2)) #'(τ1 ...))]
     [(_ (~U* τ2 ...))
      (stx-ormap (lambda (t) (<: t1 t)) #'(τ2 ...))]
     [((~Actor τ1) (~Actor τ2))
      (and (<: #'τ1 #'τ2)
           (<: (∩ (strip-? #'τ1) #'τ2) #'τ1))]
     [((~proc τ-in1 ... -> τ-out1 #:spawns (~seq S1 ...)
                                  #:roles (~seq R1 ...)
                                  #:endpoints (~seq E1 ...))
       (~proc τ-in2 ... -> τ-out2 #:spawns (~seq S2 ...)
                                  #:roles (~seq R2 ...)
                                  #:endpoints (~seq E2 ...)))
      (and (stx-length=? #'(τ-in1 ...) #'(τ-in2 ...))
           (stx-andmap <: #'(τ-in2 ...) #'(τ-in1 ...))
           (<: #'τ-out1 #'τ-out2)
           (<: (mk-Actor- (list (mk-U*- #'(S1 ...))))
               (mk-Actor- (list (mk-U*- #'(S2 ...)))))
           (<: (mk-U*- #'(R1 ...))
               (mk-U*- #'(R2 ...)))
           (<: (mk-U*- #'(E1 ...))
               (mk-U*- #'(E2 ...))))]
     [(~Discard _)
      #t]
     [(X:id Y:id)
      (or (free-identifier=? #'X #'Y)
          #;(let ()
            (printf "X:\n")
            (pretty-print (syntax-debug-info (values #;syntax-local-introduce #'X)))
            (pretty-print (identifier-binding #'X))
            (printf ":\n")
            (pretty-print (syntax-debug-info (values #;syntax-local-introduce #'Y)))
            (pretty-print (identifier-binding #'Y))
            #f))]
     [((~∀+ (X:id ...) τ1) (~∀+ (Y:id ...) τ2))
      #:when (stx-length=? #'(X ...) #'(Y ...))
      #:with τ2-X/Y (substs #'(X ...) #'(Y ...) #'τ2)
      ;; #:do [(displayln "∀ <: ∀")
            ;; (displayln #'τ2-X/Y)]
      (<: #'τ1 #'τ2-X/Y)]
     [((~Base τ1:id) (~Base τ2:id))
      (or (free-identifier=? #'τ1 #'τ2)
          #;(let ()
            (printf "τ1:\n")
            (pretty-print (syntax-debug-info (values #;syntax-local-introduce #'τ1)))
            (pretty-print (identifier-binding #'τ1))
            (printf "τ2:\n")
            (pretty-print (syntax-debug-info (values #;syntax-local-introduce #'τ2)))
            (pretty-print (identifier-binding #'τ2))
            #f))]
     [((~Role+Body (x) _ ...) (~Role+Body (y) _ ...))
      ;; Extremely Coarse subtyping for Role types
      (type=? t1 t2)]
     ;; TODO: clauses for Roles, effectful functions, and so on
     [((~Any/new τ-cons1 τ1 ...) (~Any/new τ-cons2 τ2 ...))
      #:when (free-identifier=? #'τ-cons1 #'τ-cons2)
      #:do [(define variances (get-arg-variances t1))]
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
     [_
      (type=? t1 t2)]))

 ;; shortcuts for mapping
 (define ((<:l l) r)
   (<: l r))

 (define ((<:r r) l)
   (<: l r)))

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; MODIFYING GLOBAL TYPECHECKING STATE!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(begin-for-syntax
  (current-typecheck-relation <:)
  (current-check-relation <:))

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
     (mk-U- (stx-map (lambda (t) (∩ t t2)) #'(τ1 ...)))]
    [(_ (~U* τ2:type ...))
     (mk-U- (stx-map (lambda (t) (∩ t1 t)) #'(τ2 ...)))]
    [(X:id Y:id)
     #:when (free-identifier=? #'X #'Y)
     #'X]
    ;; Also, using <: is OK, even though <: refers to ∩, because <:'s use of ∩ is only
    ;; in the Actor case.
    [((~Base τ1:id) (~Base τ2:id))
     #:when (free-identifier=? #'τ1 #'τ2)
     t1]
    [((~Any/new τ-cons1 τ1 ...) (~Any/new τ-cons2 τ2 ...))
     #:when (free-identifier=? #'τ-cons1 #'τ-cons2)
     #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
     #:do [(define desc (get-type-isec-desc #'τ-cons1))]
     #:when desc
     (define slots (stx-map ∩ #'(τ1 ...) #'(τ2 ...)))
     (match desc
       [(== BASE)
        (error "this isn't right")]
       [(== CONTAINER-LIKE)
        (reassemble-type #'τ-cons1 slots)]
       [(== PRODUCT-LIKE)
        (if (ormap bot? slots)
            (mk-U*- '())
            (reassemble-type #'τ-cons1 slots))])]
    [_ (mk-U*- '())]))

;; Type Type -> Bool
;; first type is the contents of the set/dataspace
;; second type is the type of a pattern
(define-for-syntax (project-safe? t1 t2)
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
      [((~Any/new τ-cons1 τ1 ...) (~Any/new τ-cons2 τ2 ...))
       #:when (free-identifier=? #'τ-cons1 #'τ-cons2)
       #:when (stx-length=? #'(τ1 ...) #'(τ2 ...))
       #:do [(define desc (get-type-isec-desc #'τ-cons1))]
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
    [X:id
     #t]
    [(~Base _) #t]
    [(~Any/new τ-cons τ ...)
     (stx-andmap finite? #'(τ ...))]))

;; PatternType -> Type
(define-for-syntax (pattern-matching-assertions t)
  (syntax-parse t
    [(~Bind τ)
     #'τ]
    [~Discard
     (type-eval #'★/t)]
    [(~U* τ ...)
     (mk-U- (stx-map pattern-matching-assertions #'(τ ...)))]
    [(~Any/new τ-cons τ ...)
     #:when (reassemblable? #'τ-cons)
     (define subitems (for/list ([t (in-syntax #'(τ ...))])
                        (pattern-matching-assertions t)))
     (reassemble-type #'τ-cons subitems)]
    [_ t]))

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
  [[x ≫ x- : τ] ... ⊢ (block body ...) ≫ body- (⇒ : τ-e)
                (⇒ ν-ep (~effs τ-ep ...))
                (⇒ ν-s (~effs τ-s ...))
                (⇒ ν-f (~effs τ-f ...))]
  ----------------------------------------
  [⊢ (lambda- (x- ...) body-) (⇒ : (→+ τ ... (Computation (Value τ-e)
                                                          (Endpoints τ-ep ...)
                                                          (Roles τ-f ...)
                                                          (Spawns τ-s ...))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (Λ (tv:id ...) e) ≫
  [([tv ≫ tv- : Type] ...) () ⊢ e ≫ e- ⇒ τ]
  --------
  ;; can't use internal mk-∀- constructor here
  ;; - will cause the bound-id=? quirk to show up
  ;;   (when subsequent tyvar refs are expanded with `type` stx class)
  ;; - requires converting type= and subst to use free-id=?
  ;;   (which is less performant)
  [⊢ e- ⇒ (∀+ (tv- ...) τ)])

(define-typed-syntax inst
  [(_ e τ:type ...) ≫
   #:cut
   [⊢ e ≫ e- ⇒ (~∀+ tvs τ_body)]
   #:fail-unless (stx-andmap instantiable? #'tvs #'(τ.norm ...))
                 "types must be instantiable"
   #:fail-unless (pure? #'e-) "expression must be pure"
   --------
   [⊢ e- ⇒ #,(substs #'(τ.norm ...) #'tvs #'τ_body)]]
  [(_ e) ≫ --- [≻ e]])

;; Identifier Type -> Bool
;; determine if a type is suitable for instantiating a variable
;; only row variables may be instantiated with effectful/higher-order types
(define-for-syntax (instantiable? x ty)
  (or (row-variable? x)
      (and (flat-type? ty)
           (finite? ty))))

#;(begin-for-syntax
  (require racket/trace)
  (trace instantiable?))

(begin-for-syntax
  ;; CONVENTION: Type variables for effects are prefixed with ρ
  (define (row-variable? x)
    (and (identifier? x)
         (char=? (string-ref (symbol->string (syntax-e x)) 0) #\ρ)))

  (define-syntax-class row-id
    #:attributes ()
    (pattern x:id #:when (row-variable? #'x))))

;; instantiate row variables with types from procedure arguments
;; BRITTLE?
(define-typed-syntax (call/inst e:expr args:expr ...) ≫
  [⊢ e ≫ e- (⇒ : (~∀+ (X:row-id ...) τ))]
  [⊢ args ≫ args- (⇒ : τ-a)] ...
  #:fail-unless (all-pure? #'(e- args- ...))
                "expressions must be pure"
  ;; ordering pretty arbitrary
  #:with ((~or (~proc _ ... -> _
                      #:spawns (~seq S ...)
                      #:roles (~seq R ...)
                      #:endpoints (~seq E ...))
              _) ...) #'(τ-a ...)
  #:with (SS ...) (if (attribute S) #'(S ... ...) #'())
  #:with (RR ...) (if (attribute R) #'(R ... ...) #'())
  #:with (EE ...) (if (attribute E) #'(E ... ...) #'())
  #:fail-unless (stx-length=? #'(X ...) #'(SS ... RR ... EE ...))
                "found the wrong number of effects"
  -------------------------------------------------------
  [≻ ((inst e- SS ... RR ... EE ...) args- ...)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequencing & Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For Debugging
(define-for-syntax DEBUG-BINDINGS? #f)

(define-for-syntax (int-def-ctx-bind-type-rename x x- t ctx)
  (when DEBUG-BINDINGS?
    (printf "adding to context ~a\n" (syntax-debug-info x)))
  (define serialized-ty (serialize-syntax t))
  (syntax-local-bind-syntaxes (list x-) #f ctx)
  (syntax-local-bind-syntaxes (list x)
                              #`(make-rename-transformer
                                 (add-orig
                                  (attach #'#,x- ': (deserialize-syntax #'#,serialized-ty))
                                  #'#,x)
                                 #;(add-orig (assign-type #'#,x- #'#,t #:wrap? #f) #'#,x))
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

(define-for-syntax KIND-TAG ':)

(define-syntax (define/intermediate stx)
  (syntax-parse stx
    [(_ x:id x-:id τ e)
     ;; including a syntax binding for x allows for module-top-level references
     ;; (where walk/bind won't replace further uses) and subsequent provides
     #:with serialized-τ (serialize-syntax #'τ)
     #'(begin-
         (define-syntax x
           (make-variable-like-transformer (add-orig (attach #'x- ': (deserialize-syntax #'serialized-τ)) #'x)))
         (define- x- e))]))

;; copied from ext-stlc
(define-typed-syntax define
  [(_ x:id (~datum :) τ:type e:expr) ≫
   #:cut
   [⊢ e ≫ e- (⇐ : τ.norm) (⇒ ν-ep (~effs τ-ep ...)) (⇒ ν-f (~effs τ-f ...)) (⇒ ν-s (~effs τ-s ...))]
   #:with x- (generate-temporary #'x)
   #:with x+ (syntax-local-identifier-as-binding #'x)
   --------
   [⊢ (erased (define/intermediate x+ x- τ.norm e-))
      (⇒ : ★/t)
      (⇒ ν-ep (τ-ep ...))
      (⇒ ν-f (τ-f ...))
      (⇒ ν-s (τ-s ...))]]
  [(_ x:id e) ≫
   #:cut
   ;This won't work with mutually recursive definitions
   [⊢ e ≫ e- (⇒ : τ) (⇒ ν-ep (~effs τ-ep ...)) (⇒ ν-f (~effs τ-f ...)) (⇒ ν-s (~effs τ-s ...))]
   #:with x- (generate-temporary #'x)
   #:with x+ (syntax-local-identifier-as-binding #'x)
   --------
   [⊢ (erased (define/intermediate x+ x- τ e-))
      (⇒ : ★/t)
      (⇒ ν-ep (τ-ep ...))
      (⇒ ν-f (τ-f ...))
      (⇒ ν-s (τ-s ...))]]
  [(_ (f [x (~optional (~datum :)) ty:type] ...
         (~or (~datum →) (~datum ->)) ty_out:type)
         e ...+) ≫
   #:cut
   [⊢ (lambda ([x : ty] ...) (block e ...)) ≫ e- (⇒ : (~and fun-ty
                                                            (~→+ (~not (~Computation _ _ _ _)) ...
                                                                (~Computation (~Value τ-v) _ _ _))))]
   #:fail-unless (<: #'τ-v #'ty_out.norm)
     (format "expected different return type\n got ~a\n expected ~a\n"
       #'τ-v #'ty_out
       #;(type->str #'τ-v)
       #;(type->str #'ty_out))
   #:with f- (add-orig (generate-temporary #'f) #'f)
   --------
   [⊢ (erased (define/intermediate f f- fun-ty e-)) (⇒ : ★/t)]]
  [(_ (f [x (~optional (~datum :)) ty] ...)
         e ...+) ≫
   #:cut
   ----------------------------
   [≻ (define (f [x ty] ... -> ★/t) e ...)]]
  ;; Polymorphic definitions
  [(_ ((~datum ∀) (X:id ...)
                  (f [x (~optional (~datum :)) ty] ...
                     (~or (~datum →) (~datum ->)) ty_out))
      e ...+) ≫
   #:cut
   #:do [(displayln 'A)]
   #:with e+ #'(Λ (X ...)
                  (lambda ([x : ty] ...)
                    (block e ...)))
   #:do [(displayln 'B)]
   [[X ≫ X- : Type] ... ⊢ e+ ≫ e- (⇒ : TTTT)
                       #;(⇒ : (~and res-ty
                                  (~∀+ (Y ...)
                                      (~→ (~not (~Computation _ _ _ _)) ...
                                          (~Computation (~Value τ-v) _ _ _)))))]
   #:do [(displayln 'C)
         (local-require turnstile/typedefs)
         (pretty-print (resugar-type #'TTTT))]
   #:with (~and res-ty
                (~∀+ (Y ...)
                     (~→+ (~not (~Computation _ _ _ _)) ...
                          (~Computation (~Value τ-v) _ _ _)))) #'TTTT
   #:do [(displayln 'D)]
   #:with ty_out- (substs #'(X- ...) #'(X ...) #'ty_out)
   #:with actual (type-eval #'(∀+ (Y ...) τ-v))
   #:with expected (type-eval #'(∀+ (X- ...) ty_out-))
   #:fail-unless (<: #'actual #'expected)
                 (format "expected different return type\n got ~a\n expected ~a\n"
                         (resugar-type #'actual) (resugar-type #'expected))
   #:do [(displayln 'E)]
   #:with f- (add-orig (generate-temporary #'f) #'f)
   -------------------------------------------------------
   [⊢ (erased (define/intermediate f f- res-ty e-)) (⇒ : ★/t)]]
  [(_ ((~datum ∀) (X:id ...)
                  (f [x (~optional (~datum :)) ty] ...))
      e ...+) ≫
   #:cut
   --------------------------------------------------
   [≻ (define (∀ (X ...) (f [x ty] ... -> ★/t)) e ...)]])

(define-typed-syntax block
  [(_ e_unit ... e) ≫
   #:do [(define-values (e-... τ... ep-effs f-effs s-effs) (walk/bind #'(e_unit ... e)))]
   #:with τ (last τ...)
   --------
   [⊢ (let- () #,@e-...) (⇒ : τ)
      (⇒ ν-ep (#,@ep-effs))
      (⇒ ν-f (#,@f-effs))
      (⇒ ν-s (#,@s-effs))]])


(define-typed-syntax begin
  [(_ e_unit ... e) ≫
   #:do [(define-values (e-... τ... ep-effs f-effs s-effs) (walk/bind #'(e_unit ... e)))]
   #:with τ (last τ...)
   --------
   [⊢ (begin- #,@e-...) (⇒ : τ)
      (⇒ ν-ep (#,@ep-effs))
      (⇒ ν-f (#,@f-effs))
      (⇒ ν-s (#,@s-effs))]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequencing & Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax #%app
  ;; Polymorphic, Effectful Function - Perform Simple Matching on Argument Types
  [(_ e_fn e_arg ...) ≫
   [⊢ e_fn ≫ e_fn- (⇒ : (~∀+ (X:row-id ...+) τ))]
   ---------------------------------------------
   [≻ (call/inst e_fn- e_arg ...)]]
  ;; Polymorphic, Pure Function - Perform Local Inference
  [(_ e_fn e_arg ...) ≫
   ;; compute fn type (ie ∀ and →)
   [⊢ e_fn ≫ e_fn- ⇒ (~∀+ Xs (~→fn tyX_in ... tyX_out))]
   ;; successfully matched a polymorphic fn type, don't backtrack
   #:cut
   #:do [(printf "A\n")]
   #:with tyX_args #'(tyX_in ... tyX_out)
   ;; solve for type variables Xs
   #:with [[e_arg- ...] Xs* cs] (solve #'Xs #'tyX_args this-syntax)
   ;; make sure types are legal
   #:with tyXs (ttc:inst-types/cs #'Xs* #'cs #'Xs)
   #:fail-unless (for/and ([ty (in-syntax #'tyXs)]
                           [x (in-syntax #'Xs)])
                   (instantiable? x ty))
                 "type variables must be flat and finite"
   ;; instantiate polymorphic function type
   #:do [(printf "B\n")]
   #:with [τ_in ... τ_out] (ttc:inst-types/cs #'Xs* #'cs #'tyX_args)
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
   #:do [(printf "C\n")]
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
                        (type-eval #'(∀+ (unsolved-X ... Y ...) τ_out))]))
   #:do [(printf "D\n")]
   --------
   [⊢ (#%plain-app- e_fn- e_arg- ...) ⇒ τ_out*]]
  ;; All Other Functions
  [(_ e_fn e_arg ...) ≫
   #:cut
   [⊢ e_fn ≫ e_fn- (⇒ : (~→+ τ_in ... (~Computation (~Value τ-out)
                                                            (~Endpoints τ-ep ...)
                                                            (~Roles τ-f ...)
                                                            (~Spawns τ-s ...))))]
   ;; TODO - don't know why this cut is needed for error messages
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
      [(~or* (~Any/new _ τ ...)
             (~Any/bvs _ _ τ ...))
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
             (ttc:add-constraints Xs '() (list (list #'expected-ty #'τ_outX)))
             '()))
       (syntax-parse stx
         [(_ e_fn . args)
          (define-values (as- cs)
              (for/fold ([as- null] [cs initial-cs])
                        ([a (in-stx-list #'args)]
                         [tyXin (in-stx-list #'(τ_inX ...))])
                (define ty_in (ttc:inst-type/cs/orig Xs cs tyXin datum=?))
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
                 (ttc:add-constraints Xs cs (list (list ty_in #'ty_a))
                                  (list (list (ttc:inst-type/cs/orig
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
      [(~Any/new tycons)
       (stx-map (λ _ irrelevant) Xs)]
      [(~?∀ () (~Any/new tycons τ ...))
       #:when (get-arg-variances #'tycons)
       #:when (stx-length=? #'[τ ...] (get-arg-variances #'tycons))
       (define τ-ctxt-variances
         (for/list ([arg-variance (in-list (get-arg-variances #'tycons))])
           (variance-compose ctxt-variance arg-variance)))
       (for/fold ([acc (stx-map (λ _ irrelevant) Xs)])
                 ([τ (in-stx-list #'[τ ...])]
                  [τ-ctxt-variance (in-list τ-ctxt-variances)])
         (map mvc:variance-join
              acc
              (find-variances Xs τ τ-ctxt-variance)))]
      [ty
       #:when (not (for/or ([X (in-list Xs)])
                     (stx-contains-id? #'ty X)))
       (stx-map (λ _ irrelevant) Xs)]
      [_ (stx-map (λ _ invariant) Xs)])))

#;(begin-for-syntax
  (define t #'Unit)
  (define t- ((current-type-eval) t))
  (displayln ((current-type?) t-))
  (define tt (syntax-parse (detach t- ':)
               [(#%plain-app x) #'x]))
  (pretty-print (syntax-debug-info tt)))

#;(begin-for-syntax
  (define t #'(→ Unit Unit))
  #;(define t #'(Actor Unit))
  (define t- ((current-type-eval) t))
  (values #;displayln ((current-type?) t-))
  (printf "flat-type? ~a\n" (flat-type? t-)))
