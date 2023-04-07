#lang turnstile

(provide (except-out (all-defined-out) → ∀ Role)
         (rename-out [→+ →]
                     [∀+ ∀]
                     [Role+Body Role]
                     [Role Role/internal]
                     [∀ ∀/internal]
                     [→ →/internal])
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
(require (for-syntax racket/provide-transform)
         racket/provide-syntax)


(module+ test
  (require rackunit)
  (require rackunit/turnstile))

(define-for-syntax KIND-TAG ':)

#;(require (for-syntax "syntax-serializer.rkt"))
(define-for-syntax (lazy-serialize t) t)
(define-for-syntax (lazy-deserialize t [ty #'Type])
  (define TYPE (type-eval ty))
  (define FN (type-eval #'FacetName))
  (let loop ([t t])
    (syntax-parse t
      #:literals (#%plain-app #%plain-lambda list)
      [_:id
       (attach t KIND-TAG TYPE)]
      [(#%plain-app tycons τ-in (#%plain-lambda (X) τ-out))
       #:do [(define var-ty (if (equal? 'Role (syntax-e #'typecons)) FN TYPE))]
       #:with τ-in- (attach (loop #'τ-in) KIND-TAG var-ty)
       #:with X- (attach #'X KIND-TAG var-ty)
       #:with τ-out- (loop #'τ-out)
       (define reconstructed
         (quasisyntax/loc t
           (#%plain-app tycons τ-in- (#%plain-lambda (X-) τ-out-))))
       (attach (add-orig reconstructed t) KIND-TAG TYPE)]
      [(#%plain-app tycons (~or* (~seq ty ... (#%plain-app (~and lst list) . more-tys))
                                 (~seq ty ...)) )
       #:with more-tys- (if (attribute more-tys) (stx-map loop #'more-tys) #'())
       (define reconstructed
         (quasisyntax/loc t
           (#%plain-app tycons
                        #,@(stx-map loop #'(ty ...))
                        (~? (#%plain-app lst . more-tys-)))))
       (define with-kind (attach (add-orig reconstructed t) KIND-TAG TYPE))
       ;; propagate the field name used by VarAssert
       (if (syntax-property t FN-KEY)
           (attach with-kind FN-KEY (syntax-property t FN-KEY))
           with-kind)])))
(define-for-syntax serialize-syntax lazy-serialize)
(define-for-syntax deserialize-syntax lazy-deserialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Checking Conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; : describes the immediate result of evaluation
;; ν describes the effects
(define-for-syntax EFF-KEY 'ν)

;; OLD
;; ν-ep key aggregates endpoint affects:
;;   `Shares`, `Reacts`, and `MakesField`
;; Note thar MakesField is only an effect, not a type
;; ν-f key aggregates facet effects (starting/stopping a facet) as `Role`s & `Stop`s and message sends, `Sends`
;; ν-s key aggregates spawned actors as `Actor`s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Renaming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (define-typed-variable-rename+ x:id (~datum ≫) x-:id (~datum :) τ:type)
    #:with serialized-τ (serialize-syntax #'τ.norm)
    (define-syntax x
      (make-variable-like-transformer (add-orig (attach #'x- ': (deserialize-syntax #'serialized-τ)) #'x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; certain metadata needs to be associated with each type, for the purpose of
;; making certain judgments and metafunctions extensible.

(begin-for-syntax
  (struct type-metadata (isec cons arity) #:transparent)

  ;; an Arity is one of
  ;;   - (arity-eq Nat)
  ;;   - (arity-ge Nat)
  (struct arity-eq (n) #:prefab)
  (struct arity-ge (n) #:prefab)
  (define (arity-gt n) (arity-ge (add1 n)))

  ;; (MutableHashOf Symbol type-metadata)
  (define TypeInfo# (make-hash))
  ;; Identifier isect-desc TypeCons -> Void
  (define (set-type-info! ty-cons isec cons arity)
    (when (hash-has-key? TypeInfo# ty-cons)
      ;; TODO
      #f)
    (hash-set! TypeInfo#
               ty-cons
               (type-metadata isec cons arity)))

  ;; Identifier -> (U #f type-metadata)
  (define (get-type-info ty-cons)
    (hash-ref TypeInfo# (syntax-e ty-cons) #f))

  ;; Identifier -> (U #f isec-desc)
  (define (get-type-isec-desc ty-cons)
    (define result? (get-type-info ty-cons))
    (and result? (type-metadata-isec result?)))

  ;; Identifier -> (U #f TypeCons)
  (define (get-type-cons ty-cons)
    (define result? (get-type-info ty-cons))
    (and result? (type-metadata-cons result?)))

  ;; Identifier -> (U #f Arity)
  (define (get-type-arity ty-cons)
    (define result? (get-type-info ty-cons))
    (and result? (type-metadata-arity result?)))

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

  (define-splicing-syntax-class arity-desc
    #:attributes (op n arity)
    #:datum-literals (= >= >)
    (pattern (~seq (~and = op) n:nat)
             #:attr arity (arity-eq (syntax-e #'n)))
    (pattern (~seq (~and >= op) n:nat)
             #:attr arity (arity-ge (syntax-e #'n)))
    (pattern (~seq (~and > op) n:nat)
             #:attr arity (arity-gt (syntax-e #'n))))

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
    (get-type-cons t))

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
        #:arity arity:arity-desc
        #:arg-variances variances
        #:isect-desc desc:isect-desc
        (~optional (~seq #:extra-info extra-info))
        (~optional (~seq #:implements meths ...)))
     #:with Name- (mk-- #'Name)
     #:with NamePat (mk-~ #'Name)
     #:with NamePat- (mk-~ #'Name-)
     #:with mk (format-id #'Name "mk-~a-" (syntax-e #'Name))
     #:with mk- (format-id #'Name- "mk-~a-" (syntax-e #'Name-))
     (quasisyntax/loc stx
       (begin-
         (define-type-constructor Name
           #:arity arity.op arity.n
           #:arg-variances variances
           (~? (~@ #:extra-info extra-info))
           (~? (~@ #:implements meths ...)))
         (begin-for-syntax
           (set-type-info! 'Name '#,(attribute desc.val) mk #,(attribute arity.arity)))))]))

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
    [(_ Name:id #:arity arity:arity-desc
        (~optional (~seq #:extra-info extra-info))
        (~optional (~seq #:implements meths ...)))
     (quasisyntax/loc stx
       (define-type-constructor+ Name
         #:arity arity.op arity.n
         #:arg-variances mk-covariant
         #:isect-desc CONTAINER-LIKE
         (~? (~@ #:extra-info extra-info))
         (~? (~@ #:implements meths ...))))]))

;; Define a type constructor that acts like a product:
;;   - covariant
;;   - does not have an empty element (i.e. intersection may be empty)
(define-syntax (define-product-type stx)
  (syntax-parse stx
    [(_ Name:id #:arity arity:arity-desc
        (~optional (~seq #:extra-info extra-info))
        (~optional (~seq #:implements meths ...)))
     (quasisyntax/loc stx
       (define-type-constructor+ Name
         #:arity arity.op arity.n
         #:arg-variances mk-covariant
         #:isect-desc PRODUCT-LIKE
         (~? (~@ #:extra-info extra-info))
         (~? (~@ #:implements meths ...))))]))

(define-type Type : Type)

(begin-for-syntax

  (define (Type? stx)
    (syntax-parse stx
      [~Type #t]
      [_ #f]))

  (define (new-type? t)
    (or (type?- t)
        (Type? (detach t ':))))

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
       (append prefix (list #'Type #'*))])))

(define-syntax (define-type-constructor stx)
  (syntax-parse stx
    [(_ Name:id #:arity arity:arity-desc
                (~optional (~seq #:arg-variances variances))
                (~optional (~seq #:extra-info extra-info))
                (~optional (~seq #:implements meths ...)))
     #:with Name- (mk-- #'Name)
     #:with mk- (mk-mk #'Name-)
     #:with Name? (mk-? #'Name)
     #:with dom (make-arity-domain #'arity.op (syntax-e #'arity.n))
     #:do [
           (define implements? (if (or (attribute variances) (attribute extra-info) (attribute meths))
                                   #'(#:implements)
                                   #'()))]
     #`(begin-
         (define-type Name : #,@#'dom -> Type
           #,@implements?
           (~? (~@ get-arg-variances-data variances))
           (~? (~@ get-extra-info-data extra-info))
           (~? (~@ meths ...)))
         (define-for-syntax (mk- args)
           ((current-type-eval) #`(Name #,@args))))]))

(define-simple-macro (define-base-type Name:id)
  (define-type Name : Type))

(define-simple-macro (define-base-types Name:id ...)
  (begin- (define-base-type Name) ...))

(define-base-types ★/t)
(define-type-constructor Discard #:arity = 1)

(define-type FacetName : FacetName)

(define-type RoleBody : Type * -> Type)
(define-type Role #:with-binders [X : FacetName] : Type -> Type
  #:implements get-resugar-info
  (syntax-parser
    [(~Role (nm : _) (~RoleBody body ...))
     (list* #'Role (list #'nm) (stx-map resugar-type #'(body ...)))]))
(define-for-syntax (Role? stx)
  (syntax-parse stx
    [(~Role (_ : _) _) #t]
    [_ #f]))

(define-type StartableFacet : StartableFacet)
(define-type Start : StartableFacet -> Type)
(define-type WithStartableFacet #:with-binders [X : StartableFacet] : Type -> Type)
(define-type FacetImpl : StartableFacet Type -> Type)
(define-type FacetImpls : Type * -> Type)
(define-type WSFBody : Type StartableFacet -> Type)

(define-for-syntax (WithStartableFacet? stx)
  (syntax-parse stx
    [(~WithStartableFacet (_ : _) _) #t]
    [_ #f]))

(define-for-syntax (TypeStartsFacet? t)
  (or (Role? t) (WithStartableFacet? t)))

(define-syntax (WithFacets stx)
  (syntax-parse stx
    [(WithFacets ([X:id FacetBody] ...+) Y:id)
     #:with Bodys #'(FacetImpls [FacetImpl X FacetBody] ...)
     (syntax/loc stx
       (WithStartableFacets [X ...] (WSFBody Bodys Y)))]))

(define-syntax (WithStartableFacets stx)
  (syntax-parse stx
    [(_ [] Body)
     (syntax/loc stx
       Body)]
    [(_ [X Xs ...] Body)
     (syntax/loc stx
       (WithStartableFacet [X : StartableFacet]
         (WithStartableFacets [Xs ...] Body)))]))

(begin-for-syntax
  (define (flatten-startable-facets ty)
    (define-values (body vars)
      (let loop ([ty ty]
                 [vars/rev '()])
        (syntax-parse ty
          [(~WithStartableFacet (X : _) τ)
           (loop #'τ (cons #'X vars/rev))]
          [τ
           (values #'τ (reverse vars/rev))])))
    #`(#,vars #,body))

  (define-syntax ~WithStartableFacets
    (pattern-expander
     (syntax-parser
       [(_ vars-pat impls-pat body-pat)
        #'(~and (~WithStartableFacet (_ : _) _)
                TY
                (~parse (vars-pat (~WSFBody impls-pat body-pat))
                        (flatten-startable-facets #'TY)))])))

  (define-syntax ~WithFacets
    (pattern-expander
     (syntax-parser
       [(_ impls-pat body-pat)
        #'(~and (~WithStartableFacets [_ (... ...)] [~FacetImpls (~FacetImpl name impl) (... ...)] body)
                (~parse impls-pat
                        #'([name impl] (... ...)))
                (~parse body-pat
                        #'body))]))))

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
(define-base-types OnStart OnStop OnDataflow)

;; (MakesField x τ τ0)
(define-type-constructor MakesField #:arity = 3
  #:implements get-resugar-info
  (syntax-parser
    [(~and stx (~MakesField _ τ τ0))
     (list #'MakesField (get-orig-field-name #'stx) (resugar-type #'τ) (resugar-type #'τ0))]))
(begin-for-syntax
  (define FN-KEY 'FIELD-NAME)

  (define (as-type x)
    (with-syntax ([x* x])
      (syntax-parse/typecheck null
        [_ ≫
           [[x* ≫ _ : Type] ⊢ x* ≫ x-]
           ---
           (≻ x-)])))

  (define (mk-MakesField x t t0)
    (define x-T (as-type x))
    (attach (type-eval #`(MakesField #,x-T #,t #,t0)) FN-KEY x))

  (define (get-orig-field-name MF)
    (detach MF FN-KEY)))

;; (ReadsField x)
(define-type-constructor ReadsField #:arity = 1
  #:implements get-resugar-info
  (syntax-parser
    [(~and stx (~ReadsField _))
     (list #'ReadsField (get-orig-field-name #'stx))]))
;; need the original name so that we can re-typecheck an assert expression with
;; different types for that name. Could also subst the original name in after
;; constructing a valid type.
(begin-for-syntax
  (define (mk-ReadsField x)
    (define x-T (as-type x))
    (attach (type-eval #`(ReadsField #,x-T)) FN-KEY x)))

;; (WritesField x τ)
(define-type-constructor WritesField #:arity = 2
  #:implements get-resugar-info
  (syntax-parser
    [(~and stx (~WritesField _ τ))
     (list #'WritesField (get-orig-field-name #'stx) (resugar-type #'τ))]))

(begin-for-syntax
  (define (mk-WritesField x t)
    (define x-T (as-type x))
    (attach (type-eval #`(WritesField #,x-T #,t)) FN-KEY x)))

;; (VarAssert x [--> τ-field τ-assert])
(define-type VarAssert : Type Type Type * -> Type
  #:implements get-resugar-info
  (syntax-parser
    [(~and stx (~VarAssert _ t1 ts ...))
     (list* #'VarAssert (get-orig-field-name #'stx) (resugar-type #'t1) (stx-map resugar-type #'(ts ...)))]))

(define-type-constructor --> #:arity = 2)

(begin-for-syntax
  (define (mk-VarAssert x t ts)
    (when (false? x) (printf "\n\n————FALSE————\n\n"))
    (define x-T (as-type x))
    (attach (type-eval #`(VarAssert #,x-T #,t #,@ts)) FN-KEY x)))


(define-type-constructor Actor #:arity = 1)
(define-type-constructor ActorWithRole #:arity >= 1)
;; usage: (ActorWithRole τc τr)
;;  τc is the communication type
;;  τr is the Role type of the root facet
(begin-for-syntax
  (define-syntax ~AnyActor
    (pattern-expander
    (syntax-parser
      [(_ τc-pat)
       #'(~or* (~Actor τc-pat)
               (~ActorWithRole τc-pat _))])))

  (define (AnyActor? t)
    (syntax-parse t
      [(~AnyActor _) #t]
      [_ #f]))

  )

#;(define-product-type Message #:arity = 1)
(define-product-type Tuple #:arity >= 0)
#;(define-product-type Observe #:arity = 1)
#;(define-product-type Inbound #:arity = 1)
#;(define-product-type Outbound #:arity = 1)
(define-container-type AssertionSet #:arity = 1)

;; functions and type abstractions
(define-for-syntax (resugar-∀ ty)
  (syntax-parse (flatten-∀ ty)
    [((X ...) body)
     (list #'∀ (syntax->list #'(X ...)) (resugar-type #'body))]))

(define-type ∀ #:with-binders [X : Type] : Type -> Type
  #:implements get-resugar-info
  resugar-∀)

(define-type-constructor → #:arity > 0
  #:implements get-resugar-info
  (syntax-parser
    [(~→ o i ...)
     (cons #'→ (stx-map resugar-type #'(i ... o)))]))

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

(define-type-constructor FnResult #:arity >= 1)


#;(begin-for-syntax
  (define type-eval0 (current-type-eval))
  (current-type-eval (lambda args
                       (parameterize ([current-use-stop-list? #f])
                         (apply type-eval0 args)))))

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
     (syntax/loc this-syntax
       (define-syntax- alias
         (make-variable-like-transformer (deserialize-syntax #'serialized-τ))))]
    [(_ (f:id x:id ...) ty)
     (syntax/loc this-syntax
       (define-syntax- f (mk-type-alias-rewriter #'(x ...) #'ty)))]))

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
  (syntax-parse tys
    [((~or (~U* ty1- ...) ty2-) ...)
     (define tys- (prune+sort #'(ty1- ... ... ty2- ...)))
     (if (= 1 (stx-length tys-))
         (stx-car tys-)
         (mk-U*- tys-))]))

(define-syntax (U stx)
  (syntax-parse stx
    [(_ . tys)
     ;; canonicalize by expanding to U*, with only (sorted and pruned) leaf tys
     #:with ((~or (~U* ty1- ...) ty2-) ...) (stx-map (current-type-eval) #'tys)
     #:with tys- (prune+sort #'(ty1- ... ... ty2- ...))
     (if (= 1 (stx-length #'tys-))
         (stx-car #'tys-)
         (syntax/loc stx (U* . tys-)))]))

;; Listof Type -> Type
(define-for-syntax (mk-U tys)
  (type-eval #`(U #,@tys)))

(define-base-types True False)
(define-type-alias Bool (U True False))


(define-simple-macro (→fn ty-in ... ty-out)
  (→+ ty-in ... (FnResult ty-out)))

(define-simple-macro (-> ty-in ... ty-out) (→fn ty-in ... ty-out))

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
        #'(~→+ ty-in ... (~FnResult ty-out))])))

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
      (~or* (~datum ->) (~datum →))
      ty-out
      (~alt (~optional (~seq #:spawns (s ...+)))
            (~optional (~seq #:roles (r ...+)))
            (~optional (~seq #:endpoints (e ...+)))
            (~optional (~seq #:effects (~or* (F ...+)
                                             (~seq F ...+)))))
      ...)
   #:with spawns (if (attribute s) #'(s ...) #'())
   #:with roles (if (attribute r) #'(r ...) #'())
   #:with endpoints (if (attribute e) #'(e ...) #'())
   #:with effects (if (attribute F) #'(F ...) #'())
   #:with body #`(→+ ty-in ... (FnResult ty-out #,@#'endpoints #,@#'roles #,@#'spawns #,@#'effects))
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
           (~optional (~seq #:effects F)))
        #:with effects (if (attribute F) #'(F) #'())
        #:with body #`(~→+ ty-in ... (~FnResult ty-out #,@#'effects))
        (if (attribute X)
            #'(~∀+ (X ...) body)
            #'body)]))))

;; for looking at the "effects"
(begin-for-syntax
  (define-syntax ~effs
    (pattern-expander
     (syntax-parser
       [(_ eff ...)
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

;; Default type constructor naming convention: camel-cased, with a T at the end
;; (assuming that data constructors are lowercase, hyphenated by convention)
;; e.g. book-club -> BookClubT

(begin-for-syntax
  ;; Identifier -> (list Identifier Identifier)
  (define (camel-case-T nm)
    (define nm/s (symbol->string (syntax-e nm)))
    (define nm/camel (string-append* (map string-titlecase (string-split nm/s "-"))))
    (list (format-id nm "~aT" nm/camel)
          (format-id nm "~a" nm/camel)))

  ;; (Parameterof (Identifier -> (list Identifier Identifier)))
  (define current-type-constructor-convention
    (make-parameter camel-case-T)))

(begin-for-syntax
  (define-splicing-syntax-class type-constructor-decl
    (pattern (~seq #:type-constructor TypeCons:id))
    (pattern (~seq) #:attr TypeCons #f))

  (define-syntax-class slot-decl
    #:attributes (name type)
    (pattern name:id #:attr type #f)
    (pattern [name:id (~optional (~datum :)) type]))

  ;; typed-ctor   : ID; the name of function implementing the type rule
  ;; untyped-ctor : ID; the name of the constructor for the (run time) struct
  ;; type-tag     : ID; a unique tag for instances of this type
  ;; type-ctor    : ID: the name of the type constructor for instances of this struct
  ;; field-ids    : (Listof ID): the names of each field accessor
  ;; field-tys    : (Listof (U #f Syntax)): the default type (serialized) of each field, if known
  (struct user-ctor (typed-ctor untyped-ctor type-tag type-ctor field-ids field-tys)
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
    [(_ (Cons:id : TyCons:id slot:slot-decl ...) clause ...)
     #'(define-constructor (Cons slot ...)
         #:type-constructor TyCons
         clause ...)]
    [(_ (Cons:id slot:slot-decl ...) clause ...)
     #:with (TyCons Ty) ((current-type-constructor-convention) #'Cons)
     (define provided-tys (attribute slot.type))
     (define all-provided? (andmap values provided-tys))
     (quasisyntax/loc stx
       (define-constructor (Cons slot ...)
         #:type-constructor TyCons
         clause ...
         #,@(if all-provided?
                #'(#:with Ty (TyCons slot.type ...))
                #'())))]))

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
       [⊢ e ≫ e- (⇒ : τ) (⇒ ν (~effs eff ...))] ...
       ----------------------
       [⊢ (#%app- #,StructName e- ...)
          (⇒ : (#,TypeCons τ ...))
          (⇒ ν (eff ... ...))]])))

(define-for-syntax ((resugar-ctor ty-cons) t)
  ;; because typedefs defines 0-arity constructors as base types,
  ;; make a custom resugar that always parenthesizes constructors
  (syntax-parse t
    [(~Any/new _ args ...)
     (cons ty-cons (stx-map resugar-type #'(args ...)))]))

(define-syntax (define-constructor stx)
  (syntax-parse stx
    [(_ (Cons:id slot:slot-decl ...)
        ty-cons:type-constructor-decl
        (~seq #:with
              Alias AliasBody) ...)
     #:with TypeCons (or (attribute ty-cons.TypeCons) (format-id stx "~a/t" (syntax-e #'Cons)))
     #:with MakeTypeCons (format-id #'TypeCons "make-~a" #'TypeCons)
     #:with GetTypeParams (format-id #'TypeCons "get-~a-type-params" #'TypeCons)
     #:with TypeConsExpander (format-id #'TypeCons "~~~a" #'TypeCons)
     #:with TypeConsExtraInfo (format-id #'TypeCons "~a-extra-info" #'TypeCons)
     #:with (StructName Cons- type-tag) (generate-temporaries #'(Cons Cons Cons))
     #:with (accessor ...) (for/list ([slot-name (in-syntax #'(slot.name ...))])
                             (format-id slot-name "~a-~a" #'Cons slot-name))
     #:with (accessor- ...) (for/list ([slot-name (in-syntax #'(slot.name ...))])
                              (format-id #'StructName "~a-~a" #'StructName slot-name))
     #:with (acc-defs ...) (mk-accessors #'(accessor ...) #'(accessor- ...) #'TypeCons #'(slot.name ...))
     #:with (field-ty? ...) (for/list ([ty? (in-list (attribute slot.type))])
                              (if ty?
                                  (serialize-syntax (type-eval ty?))
                                  #'#f))
     (define arity (stx-length #'(slot ...)))
     #`(begin-
         (struct- StructName (slot.name ...) #:reflection-name 'Cons #:transparent)
         (define-for-syntax (TypeConsExtraInfo stx)
           (list #'type-tag #'MakeTypeCons #'GetTypeParams))
         (define-product-type TypeCons
           #:arity = #,arity
           #:extra-info TypeConsExtraInfo
           #:implements get-resugar-info (resugar-ctor #'TypeCons))
         (define-type-alias Alias AliasBody) ...
         (define-syntax MakeTypeCons (mk-ctor-rewriter #'TypeCons))
         (define-syntax GetTypeParams (mk-type-params-fetcher #'TypeCons))
         (define-syntax Cons
           (user-ctor #'Cons-
                      #'StructName
                      'type-tag
                      #'TypeCons
                      (list #'accessor ...)
                      (list #'field-ty? ...)))
         (define-syntax Cons- (mk-constructor-type-rule #,arity #'StructName #'TypeCons))
         acc-defs ...)]))

(define-for-syntax (mk-accessors accessors accessors- TypeCons slots)
  (for/list ([accessor (in-syntax accessors)]
             [accessor- (in-syntax accessors-)]
             [slot (in-syntax slots)])
    (quasisyntax/loc TypeCons
      (define-typed-variable-rename+ #,accessor ≫ #,accessor- : (∀+ #,slots (→fn (#,TypeCons #,@slots) #,slot))))))

(define-for-syntax ((define-struct-accs accs/rev field-accs? TypeCons lib) stx)
  (syntax-parse stx
    [(_ ucons:id)
     (define cleaned-accs (cleanup-accs #'ucons accs/rev))
     (define accs (if (empty? field-accs?)
                      cleaned-accs
                      (format-all #'ucons field-accs?)))
     (define accs- (map mk-- cleaned-accs))
     (define slots (generate-temporaries accs))
     (define renames (for/list ([acc (in-list cleaned-accs)]
                                [acc- (in-list accs-)])
                       #`[#,acc #,acc-]))
     (quasisyntax/loc TypeCons
       (begin-
         (require- (only-in- #,lib #,@renames))
         #,@(mk-accessors accs accs- TypeCons slots)))]))

(define-for-syntax (format-all ucons accs)
  (for/list ([acc (in-list accs)])
    (format-id ucons "~a" (syntax-e acc))))

(define-for-syntax (cleanup-accs ucons accs/rev)
  (format-all ucons (reverse accs/rev)))

;; (require-struct chicken #:as Chicken #:from "some-mod.rkt") will
;;  - extract the struct-info for chicken, and ensure that it is immutable, has a set number of fields
;;  - determine the number of slots, N, chicken has
;;  - define the type constructor (Chicken ...N), with the extra info used by define-constructor above
;;  - define chicken+, a turnstile type rule that checks uses of chicken
;;  - bind chicken to a user-ctor struct
;; TODO: this implementation shares a lot with that of define-constructor
(define-syntax (require-struct stx)
  (syntax-parse stx
    [(_ ucons:id
        (~optional (~seq #:as ty-cons:id))
        #:from lib
        (~optional (~seq slot:slot-decl ...))
        (~optional (~and omit-accs #:omit-accs)))
     ;; TBH I'm not sure why I don't need to SLIAB TypeCons and Cons-
     #:with (TypeCons Ty) #`(~? (ty-cons ty-cons) #,((current-type-constructor-convention) #'ucons))
     #:with MakeTypeCons (format-id #'TypeCons "make-~a" #'TypeCons)
     #:with GetTypeParams (format-id #'TypeCons "get-~a-type-params" #'TypeCons)
     #:with TypeConsExpander (format-id #'TypeCons "~~~a" #'TypeCons)
     #:with TypeConsExtraInfo (format-id #'TypeCons "~a-extra-info" #'TypeCons)
     #:with Cons- (format-id #'ucons "~a/checked" #'ucons)
     #:with orig-struct-info (generate-temporary #'ucons)
     #:with type-tag (generate-temporary #'ucons)
     #:with (field-ty? ...) (for/list ([ty? (in-list (attribute slot.type))])
                              (if ty?
                                  #`#,(serialize-syntax (type-eval ty?))
                                  #f))
     #:with (field-acc ...) (for/list ([name? (in-list (attribute slot.name))])
                              (if name?
                                  (format-id #'ucons "~a-~a" #'ucons name?)
                                  #f))
     (quasisyntax/loc stx
       (begin-
         (require- (only-in- lib [ucons orig-struct-info]))
         (begin-for-syntax
           (define info (syntax-local-value #'orig-struct-info))
           (unless (struct-info? info)
             (raise-syntax-error #f "expected struct" #'#,stx #'ucons))
           (match-define (list desc cons pred accs/rev muts sup) (extract-struct-info info))
           (when (and (cons? accs/rev) (false? (last accs/rev)))
             (raise-syntax-error #f "number of slots must be exact" #'#,stx #'ucons))
           (unless (boolean? sup)
             (raise-syntax-error #f "structs with super-type not supported" #'#,stx #'ucons))
           (define arity (length accs/rev))
           (define field-tys (list #'field-ty? ...))
           (define field-accs? (list #'field-acc ...))
           (define slots-given (length field-tys))
           (unless (or (zero? slots-given)
                       (equal? slots-given arity))
             (raise-syntax-error
              #f
              (format "incorrect number of slots specified, given ~a expected ~a" slots-given arity)
              #'#,stx
              #'(slot ...)))
           )
         (define-syntax finish-type-defs
           (finish-require-struct-typedef #'lib
                                          #'Cons-
                                          #'TypeConsExtraInfo
                                          #'type-tag
                                          #'MakeTypeCons
                                          #'GetTypeParams
                                          #'orig-struct-info
                                          #'accs/rev
                                          arity
                                          #,(and (attribute omit-accs) #t)
                                          (list #'field-ty? ...)
                                          #'field-tys
                                          #'field-accs?))
         (finish-type-defs ucons TypeCons Ty)))]))

;; This is so that the arity of the struct can be included in the generated typedef
(define-for-syntax ((finish-require-struct-typedef lib
                                                   Cons-
                                                   TypeConsExtraInfo
                                                   type-tag
                                                   MakeTypeCons
                                                   GetTypeParams
                                                   orig-struct-info
                                                   accs/rev
                                                   arity
                                                   omit-accs?
                                                   field-tys?
                                                   field-tys-nm
                                                   field-accs?)
                    stx)
  (syntax-parse stx
    [(_ ucons:id TypeCons:id Ty:id)
     #:do [(define all-types-given? (and (equal? arity (length field-tys?))
                                         (andmap values field-tys?)))
           (define define-ty-alias?
             (and all-types-given?
                  (not (bound-identifier=? #'TypeCons #'Ty))))]
     #:with field-accs #`(if (empty? #,field-accs?)
                             (cleanup-accs #'ucons #,accs/rev)
                             (format-all #'ucons #,field-accs?))
     #:with field-tys (if all-types-given?
                          field-tys-nm
                          #`(list #,@(make-list arity #f)))
     (quasisyntax/loc #'ucons
       (begin-
         (define-for-syntax (#,TypeConsExtraInfo stx)
           (list #'#,type-tag #'#,MakeTypeCons #'#,GetTypeParams))
         (define-product-type TypeCons
           #:arity = #,arity
           #:extra-info #,TypeConsExtraInfo
           #:implements get-resugar-info (resugar-ctor #'TypeCons))
         (define-syntax #,MakeTypeCons (mk-ctor-rewriter #'TypeCons))
         (define-syntax #,GetTypeParams (mk-type-params-fetcher #'TypeCons))
         (define-syntax #,Cons- (mk-constructor-type-rule #,arity #'#,orig-struct-info #'TypeCons))
         (define-syntax ucons
           (user-ctor #'#,Cons-
                      #'#,orig-struct-info
                      '#,type-tag
                      #'TypeCons
                      field-accs
                      field-tys))
         #,(unless omit-accs?
             (quasisyntax/loc #'ucons
               (begin-
                 (define-syntax mk-struct-accs
                   (define-struct-accs #,accs/rev #,field-accs? #'TypeCons #'#,lib))
                 (mk-struct-accs ucons))))
         #,(when define-ty-alias?
             #`(define-type-alias Ty (TypeCons #,@field-tys?)))))]))

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

  #;(define (inspect t)
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
    (user-ctor-type-tag (syntax-local-value stx (const #f))))

  ;; requires (ctor-id? stx)
  ;; fetch the field types
  (define (ctor-field-tys stx)
    (define tys (user-ctor-field-tys (syntax-local-value stx (const #f))))
    (for/list ([ty? (in-list tys)])
      (and ty? (deserialize-syntax ty?))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require & Provide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define-syntax-class opaque-require-clause
    #:datum-literals (= > >=)
    #:attributes (type-definition)
    (pattern [#:opaque ty-name:id]
             #:attr type-definition #'(define-base-type ty-name))
    (pattern [#:opaque ty-name:id #:arity (~and op (~or* = > >=)) arity:nat]
             #:attr type-definition #'(define-product-type ty-name #:arity op arity)))

  (define-syntax-class alias-require-clause
    #:attributes (header ty)
    (pattern [#:alias ty-name:id ty]
             #:attr header #'ty-name)
    (pattern [#:alias (ty-name:id fmls:id ...) ty]
             #:attr header #'(ty-name fmls ...)))

  (define-splicing-syntax-class maybe-omit-accs
    #:attributes (omit?)
    (pattern #:omit-accs #:attr omit? #t)
    (pattern (~seq) #:attr omit? #f))

  (define-syntax-class struct-require-clause
    #:datum-literals (:)
    #:attributes (Cons TyCons omit-accs [slot 1] [slot.name 1] [slot.type 1])
    (pattern [#:struct Cons:id #:as TyCons:id
              (~optional (~seq slot:slot-decl ...))
              (~optional (~and omit-accs #:omit-accs))])
    (pattern [#:struct Cons:id
              (~optional (~seq slot:slot-decl ...))
              (~optional (~and omit-accs #:omit-accs))]
             #:attr TyCons #f))

  (define-syntax-class require/typed-clause
    #:datum-literals (:)
    #:attributes (expansion rename)
    (pattern [name:id : ty]
             #:with name- (format-id #'name "~a-" #'name)
             #:attr rename #'[name name-]
             #:attr expansion (lambda (lib)
                                #'(define-syntax name
                                    (make-variable-like-transformer
                                     (add-orig (assign-type #'name- (deserialize-syntax (serialize-syntax (type-eval #'ty)))
                                                            #:wrap? #f) #'name)))))
    (pattern struct-clause:struct-require-clause
             #:attr rename #f
             #:attr expansion (lambda (lib)
                                #`(require-struct struct-clause.Cons
                                                  (~? (~@ #:as struct-clause.TyCons))
                                                  #:from #,lib
                                                  (~? (~@ struct-clause.slot ...))
                                                  (~? struct-clause.omit-accs))))
    (pattern opaque-clause:opaque-require-clause
             #:attr rename #f
             #:attr expansion (lambda (lib)
                                #'opaque-clause.type-definition))
    (pattern alias-clause:alias-require-clause
             #:attr rename #f
             #:attr expansion (lambda (lib)
                                #'(define-type-alias alias-clause.header alias-clause.ty))))
  )

;; Import and ascribe a type from an untyped module
;; TODO: this is where contracts would need to go
(define-syntax (require/typed stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ lib import:require/typed-clause ...)
     #:with renames (stx-filter values (attribute import.rename))
     (quasisyntax/loc stx
       (begin-
         (require (only-in lib . renames))
         #,@(stx-map (lambda (e) (e #'lib)) (attribute import.expansion))))]))

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

(define-syntax struct-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ ctor:id)
        (define val (syntax-local-value #'ctor (const #f)))
        (unless (user-ctor? val)
          (raise-syntax-error #f (format "not a constructor: ~a" (syntax-e #'ctor)) this-syntax))
        (define accs (user-ctor-field-ids val))
        (for/list ([f (in-list (list* #'ctor (user-ctor-type-ctor val) accs))])
          (make-export f (syntax-e f) (syntax-local-phase-level) #f f))]))))

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

(require-struct observe #:as Observe #:from syndicate/patch)
(require-struct inbound #:as Inbound #:from syndicate/protocol/standard-relay)
(require-struct outbound #:as Outbound #:from syndicate/protocol/standard-relay)
(require-struct message #:as Message #:from syndicate/core)

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
    [(~AnyActor τ) #f]
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
    [(~ActorWithRole _ τ-r)
     (define-values (τ-i τ-o τ-i/i τ-o/i τ-a) (analyze-role-input/output #'τ-r))
     (values τ-i τ-o bot bot (mk-U- (list t τ-a)))]
    [(~AnyActor τc)
     (values bot bot bot bot t)]
    [(~Sends τ-m)
     (values bot (mk-Message- #'(τ-m)) bot bot bot)]
    [(~Realizes τ-m)
     (values bot bot bot (mk-Realize- #'(τ-m)) bot)]
    [(~Start _)
     (values bot bot bot bot bot)]
    [(~or* (~ReadsField _)
           (~WritesField _ _)
           (~MakesField _ _ _))
     (values bot bot bot bot bot)]
    [(~WithFacets ([nm impl] ...) fst)
     (apply values (syntax->list (analyze-roles #'(impl ...))))]
    [(~or* (~Shares τ-s)
           (~VarAssert _ [~--> _ τ-s] _ ...))
     (values bot #'τ-s bot bot bot)]
    [(~Know τ-k)
     (values bot bot bot #'τ-k bot)]
    [(~Reacts τ-if τ-then ...)
     #:with (τ-i τ-o τ-i/i τ-o/i τ-a) (analyze-roles #'(τ-then ...))
     (define pat-type (event-desc-type #'τ-if))
     (define sub-type (pattern-sub-type pat-type))
     (define-values (i ii)
       (if (external-evt? #'τ-if)
           (values (mk-U- #`(τ-i #,pat-type)) #'τ-i/i)
           (values #'τ-i (mk-U- #`(τ-i/i #,pat-type)))))
     (define-values (o oi)
       (if (external-evt? #'τ-if)
           (values (mk-U- #`(τ-o #,sub-type)) #'τ-o/i)
           (values #'τ-o (mk-U- #`(τ-o/i #,sub-type)))))
     (values i o ii oi #'τ-a)]
    [(~Role+Body (_) EP ...)
     (apply values (syntax->list (analyze-roles (flatten-effects #'(EP ...)))))]))

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
    [(~Discard τ)
     (type-eval #'★/t)]
    [(~U* τ ...)
     (mk-U- (stx-map replace-bind-and-discard-with-★ #'(τ ...)))]
    [(~Any/new τ-cons τ ...)
     #:when (reassemblable? #'τ-cons)
     (define subitems (for/list ([t (in-syntax #'(τ ...))])
                        (replace-bind-and-discard-with-★ t)))
     (reassemble-type #'τ-cons subitems)]
    [_ t]))

;; Type -> Bool
;; to prevent observing the linkage assertions used by during/spawn,
;; disallow ?★ and ??★
(define-for-syntax (allowed-interest? t)
  (not (or (<: (type-eval #'(Observe ★/t)) t)
           (<: (type-eval #'(Observe (Observe ★/t))) t))))

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

;; Type -> String
(define-for-syntax (type->datum ty)
  ;; Identifier -> symbol
  ;; this won't work for any names with numbers in them :\
  (define (un-gensym x)
    (define GENSYMED #px"^(\\D*)\\d*$")
    (string->symbol (second (regexp-match GENSYMED (symbol->string (syntax-e x))))))
  ;; (Listof String) -> String
  (define (paren-join xs)
    (string-join xs
                 #:before-first "("
                 #:after-last ")"))
  (syntax-parse ty
    [X:id
     (un-gensym #'X)]
    [(~U* τ ...)
     `(U ,@(stx-map type->datum #'(τ ...)))]
    [(~Base x)
     (un-gensym #'x)]
    [(~Role+Body (x:id) τ ...)
     (define nm (un-gensym #'x))
     (define body (stx-map type->datum #'(τ ...)))
     `(Role (#,nm) . body)]
    [(~∀+ (X ...) τ)
     (define vars (stx-map type->datum #'(X ...)))
     `(∀ ,vars ,(type->datum #'τ))]
    [(~Any/new τ-cons τ ...)
     (define ctor (un-gensym #'τ-cons))
     (define body (stx-map type->datum #'(τ ...)))
     `(,ctor . body)
     (cons ctor body)]
    [(~Any/bvs τ-cons (X ...) τ ...)
     (define ctor (un-gensym #'τ-cons))
     (define body (stx-map type->datum #'(τ ...)))
     (cond
       [(empty? (syntax->list #'(X ...)))
        (cons ctor body)]
       [else
        (define vars
          (stx-map type->datum #'(X ...)))
        (list* ctor vars body)])]))

(define-for-syntax (pretty-type->strX ty)
  (pretty-format (type->datum ty) #:mode 'display))

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
     [((~AnyActor τ1) (~AnyActor τ2))
      (and (<: #'τ1 #'τ2)
           (<: (∩ (strip-? #'τ1) #'τ2) #'τ1))]
     [((~proc τ-in1 ... -> τ-out1 #:effects (~seq F1 ...))
       (~proc τ-in2 ... -> τ-out2 #:effects (~seq F2 ...)))
      (and (stx-length=? #'(τ-in1 ...) #'(τ-in2 ...))
           (stx-andmap <: #'(τ-in2 ...) #'(τ-in1 ...))
           (<: #'τ-out1 #'τ-out2)
           (check-effects #'(F1 ...) #'(F2 ...)))]
     [((~Discard τ1) _)
      (<: #'τ1 t2)]
     [(X:id Y:id)
      (if (free-identifier=? #'X #'Y)
          #t
          #f #;(begin (pretty-print (syntax-debug-info #'X))
                 (pretty-print (syntax-debug-info #'Y))
                 #f))]
     [((~∀+ (X:id ...) τ1) (~∀+ (Y:id ...) τ2))
      #:when (stx-length=? #'(X ...) #'(Y ...))
      #:with τ2-X/Y (substs #'(X ...) #'(Y ...) #'τ2)
      (<: #'τ1 #'τ2-X/Y)]
     [((~Base τ1:id) (~Base τ2:id))
      (free-identifier=? #'τ1 #'τ2)]
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

 ;; subtyping for effect sequences based on their effect on the actors
 ;; inputs/outputs/spawns
 (define (check-effects effs1 effs2)
   (syntax-parse (list (analyze-roles (flatten-effects effs1))
                       (analyze-roles (flatten-effects effs2)))
     [((is1 os1 is/i1 os/i1 ss1)
       (is2 os2 is/i2 os/i2 ss2))
      (and (<: #'is2 #'is1)
           (<: #'os1 #'os2)
           (<: #'is/i2 #'is/i1)
           (<: #'os/i1 #'os/i2)
           (<: #'ss1 #'ss2))]))

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
      [(_ (~Discard τ2))
       (<: t1 #'τ2)]
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
    [(~Discard _)
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
  (or (false? prop)
      (stx-null? prop)
      (and (stx-list? prop)
           (stx-andmap ReadsField? prop))))

;; DesugaredSyntax -> Bool
(define-for-syntax (pure? e-)
  (effect-free? e- EFF-KEY))

;; (SyntaxOf DesugaredSyntax ...) -> Bool
(define-for-syntax (all-pure? es)
  (stx-andmap pure? es))

(begin-for-syntax
  ;; (Syntax -> Bool) Syntax String -> Void
  (define (ensure! p stx msg #:src [src stx])
    (unless (p stx)
      (type-error #:src src #:msg (format "~a\nGot:~a\n" msg stx))))

  ;; (Syntax -> Bool) (Sequenceof Syntax) String -> Void
  (define (ensure-all! p stxs msg #:src [src #f])
    (for ([stx (in-list stxs)])
      (ensure! p stx msg #:src (or src stx))))

  ;; (SyntaxListof EffectType) -> (SyntaxListof EffectType)
  (define (flatten-effects effs)
    (let loop ([work (syntax->list effs)]
               [effs '()])
      (match work
        ['() effs]
        [(cons eff more)
         (syntax-parse eff
           [(~or* (~Branch F ...)
                  (~Effs F ...))
            (loop (append (syntax->list #'(F ...)) more)
                  effs)]
           [_
            (loop more (cons eff effs))])])))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lambdas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: It's annoying that this is here instead of roles.rkt
(begin-for-syntax
  (define current-facet-name (make-parameter #f)))

(define-syntax (lambda stx)
  (parameterize ([current-facet-name #f])
    (syntax-parse/typecheck stx
      [(lambda ([x:id (~optional (~datum :)) τ:type] ...) body ...+) ≫
       [[x ≫ x- : τ] ... ⊢ (block body ...) ≫ body-
                                            (⇒ : τ-e)
                                            (⇒ ν (~effs eff ...))]
                ----------------------------------------
                [⊢ (lambda- (x- ...) body-) (⇒ : (→+ τ ... (FnResult τ-e eff ...)))]])))

(define-simple-macro (λ args body ...+) (lambda args body ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (Λ (tv:id ...) e) ≫
  [([tv ≫ tv- : Type] ...) () ⊢ e ≫ e-
                           (⇒ : τ)
                           (⇒ ν (~effs eff ...))]
  --------
  ;; can't use internal mk-∀- constructor here
  ;; - will cause the bound-id=? quirk to show up
  ;;   (when subsequent tyvar refs are expanded with `type` stx class)
  ;; - requires converting type= and subst to use free-id=?
  ;;   (which is less performant)
  [⊢ e- (⇒ : (∀+ (tv- ...) τ))
        (⇒ ν (eff ...))])

(define-typed-syntax inst
  [(_ e τ:type ...) ≫
   #:cut
   [⊢ e ≫ e- (⇒ : (~∀+ tvs τ_body)) (⇒ ν (~effs eff ...))]
   #:fail-unless (stx-andmap instantiable? #'tvs #'(τ.norm ...))
                 "types must be instantiable"
   #:fail-unless (pure? #'e-) "expression must be pure"
   --------
   [⊢ e- (⇒ : #,(substs #'(τ.norm ...) #'tvs #'τ_body))
         (⇒ ν #,(substs #'(τ.norm ...) #'tvs #'(eff ...)))]]
  [(_ e) ≫ --- [≻ e]])

;; Identifier Type -> Bool
;; determine if a type is suitable for instantiating a variable
;; only row variables may be instantiated with effectful/higher-order types
(define-for-syntax (instantiable? x ty)
  (or (row-variable? x)
      (and (flat-type? ty)
           (finite? ty))))

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
  #:with ((~alt (~proc _ ... -> _ #:effects (~seq F ...))
                _) ...) #'(τ-a ...)
  #:with (FF ...) (if (attribute F) #'(F ... ...) #'())
  #:fail-unless (stx-length=? #'(X ...) #'(FF ...))
                "found the wrong number of effects"
  -------------------------------------------------------
  [≻ ((inst e- FF ...) args- ...)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequencing & Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For Debugging
(define-for-syntax DEBUG-BINDINGS? #f)

(define-for-syntax (int-def-ctx-bind-type-rename x x- t ctx)
  (when DEBUG-BINDINGS?
    (printf "adding to context:\n") (pretty-display (syntax-debug-info x)))
  ;; at some point these serialize/deserialze-syntax calls seemed to fix an issue, but
  ;; in principle it doesn't seem like they should be necessary and things seem to be
  ;; working w/o them *shrug*
  (define serialized-ty (values #;serialize-syntax t))
  (syntax-local-bind-syntaxes (list x-) #f ctx)
  (syntax-local-bind-syntaxes (list x)
                              #`(make-rename-transformer
                                 (add-orig
                                  (attach #'#,x- ': (values #;deserialize-syntax #'#,serialized-ty))
                                  #'#,x))
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

;; -> (Values e-... (Listof Type) (Listof Effect))
;; recognizes local binding forms
;; (field/intermediate [x e] ...
;; (define/intermediate x x- τ e)
(define-for-syntax (walk/bind e...
                              [def-ctx (syntax-local-make-definition-context)]
                              [unique (gensym 'walk/bind)])
  (define-values (rev-e-... rev-τ... effects)
    (let loop ([e... (syntax->list e...)]
               [rev-e-... '()]
               [rev-τ... '()]
               [effects '()])
      (match e...
        ['()
         (values rev-e-... rev-τ... effects)]
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
                  effects)]
           [_
            (define τ (syntax-property e- ':))
            (define effs (syntax->list (get-effect e- EFF-KEY)))
            (add-bindings-to-ctx e- def-ctx)
            (loop more
                  (cons e- rev-e-...)
                  (cons τ rev-τ...)
                  (append effs effects))])])))
  (values (reverse rev-e-...)
          (reverse rev-τ...)
          effects))

(define-syntax (field/intermediate stx)
  (syntax-parse stx
    [(_ [x:id x-:id τ e-] ...)
     #'(syndicate:field [x- e-] ...)]))

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

(define-typed-syntax define
  [(_ x:id (~datum :) τ:type e:expr) ≫
   #:cut
   [⊢ e ≫ e- (⇐ : τ.norm) (⇒ ν (~effs eff ...))]
   #:with x- (generate-temporary #'x)
   #:with x+ (syntax-local-identifier-as-binding #'x)
   --------
   [⊢ (erased (define/intermediate x+ x- τ.norm e-))
      (⇒ : ★/t)
      (⇒ ν (eff ...))]]
  [(_ x:id e) ≫
   #:cut
   ;This won't work with mutually recursive definitions
   [⊢ e ≫ e- (⇒ : τ) (⇒ ν (~effs eff ...))]
   #:with x- (generate-temporary #'x)
   #:with x+ (syntax-local-identifier-as-binding #'x)
   --------
   [⊢ (erased (define/intermediate x+ x- τ e-))
      (⇒ : ★/t)
      (⇒ ν (eff ...))]]
  [(_ (f [x (~optional (~datum :)) ty:type] ...
         (~or (~datum →) (~datum ->)) ty_out:type)
         e ...+) ≫
   #:cut
   [⊢ (lambda ([x : ty] ...) (block e ...)) ≫ e- (⇒ : (~and fun-ty
                                                            (~→ (~FnResult τ-v _ ...)
                                                                _ ...)))]
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
   #:with e+ #'(lambda ([x : ty] ...)
                 (block e ...))
              #;#'(Λ (X ...)
                  (lambda ([x : ty] ...)
                    (block e ...)))
   [[X ≫ X- : Type] ... ⊢ e+ ≫ e- (⇒ : (~and res-ty (~→ (~FnResult τ-v _ ...) _ ...)))]
   ;; #:with ty_out- (substs #'(X- ...) #'(X ...) #'ty_out)
   ;; #:with actual (substs #'(X ...) #'(X- ...) #'τ-v) #;(type-eval #'(∀+ (Y ...) τ-v))
   ;; #:with expected (type-eval #'(∀+ (X- ...) ty_out-))
   #:with τ-v+ (substs #'(X ...) #'(X- ...) #'τ-v)
   #:with actual (type-eval #'(∀+ (X ...) τ-v+))
   #:with expected (type-eval #'(∀+ (X ...) ty_out))
   #:fail-unless (<: #'actual #'expected)
                 (format "expected different return type\n got ~a\n expected ~a\n"
                         (resugar-type #'actual) (resugar-type #'expected))
   #:with f- (add-orig (generate-temporary #'f) #'f)
   #:with final-ty (type-eval #'(∀+ (X- ...) res-ty))
   -------------------------------------------------------
   [⊢ (erased (define/intermediate f f- final-ty e-)) (⇒ : ★/t)]]
  [(_ ((~datum ∀) (X:id ...)
                  (f [x (~optional (~datum :)) ty] ...))
      e ...+) ≫
   #:cut
   --------------------------------------------------
   [≻ (define (∀ (X ...) (f [x ty] ... -> ★/t)) e ...)]])

(define-typed-syntax block
  [(_ e_unit ... e) ≫
   #:do [(define-values (e-... τ... effs) (walk/bind #'(e_unit ... e)))]
   #:with τ (last τ...)
   --------
   [⊢ (let- () #,@e-...) (⇒ : τ)
      (⇒ ν (#,@effs))]])


(define-typed-syntax begin
  [(_ e_unit ... e) ≫
   #:do [(define-values (e-... τ... effs) (walk/bind #'(e_unit ... e)))]
   #:with τ (last τ...)
   --------
   [⊢ (begin- #,@e-...) (⇒ : τ)
      (⇒ ν (#,@effs))]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequencing & Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax #%app
  ;; Polymorphic, Effectful Function - Perform Simple Matching on Argument Types
  [(_ e_fn e_arg ...) ≫
   [⊢ e_fn ≫ e_fn- (⇒ : (~∀+ (X:row-id ...+) τ))]
   #:cut
   #:fail-unless (pure? #'e_fn-) "function expression must be pure"
   ---------------------------------------------
   [≻ (call/inst e_fn- e_arg ...)]]
  ;; Polymorphic, Pure Function - Perform Local Inference
  [(_ e_fn e_arg ...) ≫
   ;; compute fn type (ie ∀ and →)
   [⊢ e_fn ≫ e_fn- ⇒ (~∀+ Xs (~→fn tyX_in ... tyX_out))]
   ;; successfully matched a polymorphic fn type, don't backtrack
   #:cut
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
   --------
   [⊢ (#%plain-app- e_fn- e_arg- ...) ⇒ τ_out*]]
  ;; All Other Functions
  [(_ e_fn e_arg ...) ≫
   #:cut
   [⊢ e_fn ≫ e_fn- (⇒ : (~→+ τ_in ... (~FnResult τ-out F ...)))]
   ;; TODO - don't know why this cut is needed for error messages
   #:fail-unless (pure? #'e_fn-) "expression not allowed to have effects"
   #:fail-unless (stx-length=? #'[τ_in ...] #'[e_arg ...])
                 (num-args-fail-msg #'e_fn #'[τ_in ...] #'[e_arg ...])
   [⊢ e_arg ≫ e_arg- (⇐ : τ_in)] ...
   #:fail-unless (all-pure? #'(e_arg- ...)) "expressions not allowed to have effects"
   ------------------------------------------------------------------------
   [⊢ (#%app- e_fn- e_arg- ...)
      (⇒ : τ-out)
      (⇒ ν (F ...))]])

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
      [(∀+ Ys τ)
       (tyvar-under-union? Xs #'τ)]
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

