#lang turnstile

(provide (rename-out [syndicate:#%module-begin #%module-begin])
         (rename-out [typed-app #%app])
         (rename-out [syndicate:begin-for-declarations declare-types])
         #%top-interaction
         require only-in
         ;; Types
         Int Bool String Tuple Bind Discard → ★/t List
         Observe Inbound Outbound Actor U (type-out U*)
         Event AssertionSet Patch Instruction
         ⊥
         ;; Core Forms
         actor dataspace make-assertion-set project ★ patch
         tuple select lambda observe inbound outbound
         idle quit transition patch-added patch-removed
         for/fold
         ;; extensions
         assert retract sub unsub patch-seq patch-seq*
         ;; core-ish forms
         begin define let let* ann if
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         + - * / and or not > < >= <= = equal? displayln
         list first rest empty? member?
         ;; sets
         Set set set-member? set-add set-count set-union set-subtract set-intersect
         list->set set->list
         ;; making types
         define-type-alias
         define-constructor
         ;; DEBUG and utilities
         print-type
         (rename-out [printf- printf])
         begin-for-syntax
         (for-syntax #%app displayln type-eval current-type? syntax)
         ;; Extensions
         )

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx))
(require (for-syntax turnstile/examples/util/filter-maximal))

(require macrotypes/postfix-in)

(require (rename-in racket/math [exact-truncate exact-truncate-]))
(require (postfix-in - racket/list))
(require (postfix-in - racket/set))
(require (prefix-in syndicate: syndicate/core-lang)
         (prefix-in syndicate: syndicate/trie)
         (prefix-in syndicate: syndicate/comprehensions))

(module+ test
  (require rackunit)
  (require rackunit/turnstile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Needed Forms
;; * dataspace DONE
;; * actor DONE
;; * make-assertion-set DONE
;;   - ★ DONE
;; * patch DONE
;; * project DONE
;;   - bind
;;   - discard
;; * transition DONE
;; * quit DONE
;; * fold DONE
;; * list DONE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(define-base-types Int Bool String Discard ★/t)

(define-type-constructor Bind #:arity = 1)
(define-type-constructor Tuple #:arity >= 0)
(define-type-constructor → #:arity > 0)
(define-type-constructor Observe #:arity = 1)
(define-type-constructor Inbound #:arity = 1)
(define-type-constructor Outbound #:arity = 1)
(define-type-constructor Actor #:arity = 1)
(define-type-constructor AssertionSet #:arity = 1)
(define-type-constructor Patch #:arity = 2)
(define-type-constructor List #:arity = 1)
(define-type-constructor Set #:arity = 1)
;; essentially the sum type of a transition or quit
(define-type-constructor Instruction #:arity = 3)

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

(begin-for-syntax
  (define-syntax ~U/no-order
    (pattern-expander
     (syntax-parser
       [(_ p ...)
        #:fail-when (stx-ormap (lambda [x] (and (identifier? x)
                                                (free-identifier=? x #'(... ...))))
                               #'(p ...))
        "ellipses not allowed"
        #:with ((v ...) ...) (permutations (stx->list #'(p ...)))
        #'(~or* (~U* v ...) ...)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Defined Types, aka Constructors

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
;; Conveniences

(define-type-alias (Action τ-a τ-s)
  (U (Patch τ-a ★/t) (Actor τ-s)))

(begin-for-syntax
  (define-syntax ~Action
    (pattern-expander
     (syntax-parser
       [(_ p1 p2)
        #'(~or (~and (~Patch p1 _)
                     (~parse p2 (type-eval #'(U))))
               (~and (~Actor p2)
                     (~parse p1 (type-eval #'(U))))
               (~U/no-order (~Patch p1 _) (~Actor p2)))]))))

(define-type-alias (Event τ)
  (Patch τ τ))

#;(begin-for-syntax
  (define-syntax ~Event
    (pattern-expander
     (syntax-parser
       [(_ t)
        #`(~and (~Patch τ1:type τ2:type)
                (~parse t #,(type-eval #'(U τ1 τ2))))]))))

(define-type-alias ⊥ (U))

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
    [((~U* τ1 ...) _)
     (stx-andmap (lambda (t) (<: t t2)) #'(τ1 ...))]
    [(_ (~U* τ2:type ...))
     (stx-ormap (lambda (t) (<: t1 t)) #'(τ2 ...))]
    [((~Actor τ1:type) (~Actor τ2:type))
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
    [((~Instruction τs1 τo1 τa1) (~Instruction τs2 τo2 τa2))
     (and (<: #'τs1 #'τs2)
          (<: #'τo1 #'τo2)
          (<: (type-eval #'(Actor τa1)) (type-eval #'(Actor τa2))))]
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
    ;; should probably put this first.
    [_ (type=? t1 t2)]))

(define-for-syntax (bot? t)
  (<: t (type-eval #'(U))))

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
    [((~Instruction τs1 τo1 τa1) (~Instruction τs2 τo2 τa2))
     #:with τs (∩ #'τs1 #'τs2)
     #:fail-when (bot? #'τs) #f
     #:with τa (∩ #'τa1 #'τa2)
     #:fail-when (bot? #'τa) #f
     #:with τo (∩ #'τo1 #'τo2)
     (type-eval #'(Instruction τs τo τa))]
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
;; Core forms

(define-typed-syntax (actor τ-c:type beh st0 as0) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ beh ≫ beh- ⇒ (~→ (~Patch τ-i1 τ-i2)
                       τ-s
                       (~Instruction τ-s2 τ-ta τ-ts))]
  [⊢ st0 ≫ st0- ⇒ τ-st0]
  [⊢ as0 ≫ as0- ⇒ (~AssertionSet τ-as0)]
  #:with τ-out (type-eval #'(U τ-ta τ-as0))
  #:with τ-in (type-eval #'(U τ-i1 τ-i2))
  #:fail-unless (<: #'τ-st0 #'τ-s)
    "bad initial state"
  #:fail-unless (<: #'τ-s2 #'τ-s)
    "bad state update"
  #:fail-unless (<: #'τ-out #'τ-c.norm)
    "output not allowed in dataspace"
  #:fail-unless (<: (type-eval #'(Actor τ-ts))
                    (type-eval #'(Actor τ-c.norm)))
    "spawned actors not valid in dataspace"
  #:fail-unless (<: (∩ (strip-? #'τ-out) #'τ-c.norm) #'τ-in)
    "Not prepared to handle all inputs"
  --------------------------------------------------------------------------------------------
  [⊢ (syndicate:actor (filter-poll-events beh-)
                      st0-
                      (list- (syndicate:patch as0- syndicate:trie-empty)))
     ⇒ (Actor τ-c)])

(define ((filter-poll-events beh) e s)
  (and- e (beh e s)))

(define-typed-syntax (dataspace τ-c:type e) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ e ≫ e- ⇒ (~List τa:type)]
  #:fail-unless (<: #'τa.norm (type-eval #'(Actor τ-c.norm)))
    "Not all actors conform to communication type"
  #:with τ-ds-i (strip-inbound #'τ-c.norm)
  #:with τ-ds-o (strip-outbound #'τ-c.norm)
  #:with τ-relay (relay-interests #'τ-c.norm)
  -----------------------------------------------------------------------------------
  [⊢ (syndicate:dataspace-actor e-) ⇒ (Actor (U τ-ds-i τ-ds-o τ-relay))])

(define-typed-syntax (transition e-s e-as) ≫
  [⊢ e-s ≫ e-s- ⇒ τ-s]
  [⊢ e-as ≫ e-as- ⇒ (~List τ)]
  ;; this parsing of actions is getting realllly hacky
  #:with (~or (~Action τ-o τ-a)
              (~parse (τ-o τ-a) (stx-map type-eval #'(⊥ ⊥)))) #'τ
  -----------------------------------------
  [⊢ (syndicate:transition e-s- e-as-) ⇒ (Instruction τ-s τ-o τ-a)])

(define-typed-syntax quit
  [(quit) ≫
   -------------------------------------
   [⊢ (syndicate:quit) ⇒ (Instruction (U) (U) (U))]]
  [(quit as) ≫
   [⊢ as ≫ as- ⇒ (~List τ)]
   ;; this parsing of actions is getting realllly hacky
   #:with (~or (~Action τ-o τ-a)
              (~parse (τ-o τ-a) (stx-map type-eval #'(⊥ ⊥)))) #'τ
   ----------------------------------------
   [⊢ (syndicate:quit as-) ⇒ (Instruction (U) τ-o τ-a)]])

(define-typed-syntax idle
  [_ ≫
   -------------------------
   [⊢ #f ⇒ (Instruction (U) (U) (U))]])

(define-typed-syntax ★
  [_ ≫
   -------------------------
   [⊢ syndicate:? ⇒ ★/t]])

(define-typed-syntax (make-assertion-set e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  #:fail-unless (stx-andmap flat-type? #'(τ ...))
    "assertions must be first-order"
  -------------------------------------------------
  [⊢ (syndicate:trie-union-all (list- (syndicate:pattern->trie 'typed e-) ...))
     ⇒ (AssertionSet (U τ ...))])

(define-typed-syntax (patch e-add e-sub) ≫
  [⊢ e-add ≫ e-add- ⇒ (~AssertionSet τ-add)]
  [⊢ e-sub ≫ e-sub- ⇒ (~AssertionSet τ-sub)]
  --------------------------------------------
  [⊢ (syndicate:patch e-add- e-sub-) ⇒ (Patch τ-add τ-sub)])

(define-typed-syntax (project [pat e-set] e-body) ≫
  [⊢ e-set ≫ e-set- ⇒ (~AssertionSet τ-s:type)]
  [⊢ pat ≫ _ ⇒ τ-p:type]
  #:with ([x:id τ:type] ...) (pat-bindings #'pat)
  [[x ≫ x- : τ] ... ⊢ e-body ≫ e-body- ⇒ τ-b]
  #:fail-unless (project-safe? #'τ-s.norm #'τ-p.norm)
    "pattern captures infinite set"
  #:with pat- (substs #'(x- ...) #'(x ...) (compile-syndicate-pattern #'pat))
  --------------------------------------------------------
  [⊢ (syndicate:for-trie/list ([pat- e-set-])
       e-body-)
     ⇒ (List τ-b)])

(begin-for-syntax
  (define (compile-pattern pat bind-id-transformer exp-transformer)
    (define (l-e stx) (local-expand stx 'expression '()))
    (let loop ([pat pat])
      (syntax-parse pat
        #:datum-literals (tuple discard bind)
        [(tuple p ...)
         #`(list- 'tuple #,@(stx-map loop #'(p ...)))]
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

  (define (compile-syndicate-pattern pat)
    (compile-pattern pat
                     (lambda (id) #`($ #,id))
                     identity)))

(define-typed-syntax (list e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  -------------------
  [⊢ (list- e- ...) ⇒ (List (U τ ...))])

(define-typed-syntax (for/fold [acc:id e-acc]
                               [x:id e-list]
                       e-body) ≫
  [⊢ e-list ≫ e-list- ⇒ (~List τ-l)]
  [⊢ e-acc ≫ e-acc- ⇒ τ-a:type]
  [[x ≫ x- : τ-l] [acc ≫ acc- : τ-a] ⊢ e-body ≫ e-body- ⇒ τ-b:type]
  #:fail-unless (<: #'τ-b.norm #'τ-a.norm)
    "loop body doesn't match accumulator"
  -------------------------------------------------------
  [⊢ (for/fold- ([acc- e-acc-])
                ([x- (in-list- e-list-)])
       e-body-)
     ⇒ τ-b])

(define-for-syntax (strip-? t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     [(~Observe τ) #'τ]
     [_ #'(U)])))

(define-for-syntax (strip-inbound t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     [(~Inbound τ) #'τ]
     [_ #'(U)])))

(define-for-syntax (strip-outbound t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     [(~Outbound τ) #'τ]
     [_ #'(U)])))

(define-for-syntax (relay-interests t)
  (type-eval
   (syntax-parse t
     ;; TODO: probably need to `normalize` the result
     [(~U* τ ...) #`(U #,@(stx-map strip-? #'(τ ...)))]
     [~★/t #'★/t]
     [(~Observe (~Inbound τ)) #'(Observe τ)]
     [_ #'(U)])))

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

(define-typed-syntax (lambda ([x:id (~optional (~datum :)) τ:type] ...) body ...+) ≫
  [[x ≫ x- : τ] ... ⊢ (begin body ...) ≫ body- ⇒ τ-e]
  ----------------------------------------
  [⊢ (lambda- (x- ...) body-) ⇒ (→ τ ... τ-e)])

(define-typed-syntax (tuple e:expr ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  -----------------------
  [⊢ (list- 'tuple e- ...) (⇒ : (Tuple τ ...))])

(define-typed-syntax (select n:nat e:expr) ≫
  #:do [(define i (syntax->datum #'n))]
  [⊢ e ≫ e- ⇒ (~Tuple τ ...)]
  #:fail-unless (< i (stx-length #'(τ ...))) "index out of range"
  #:with τr (list-ref (stx->list #'(τ ...)) i)
  --------------------------------------------------------------
  [⊢ (tuple-select n e-) ⇒ τr])

(define- (tuple-select n t)
  (list-ref- t (add1 n)))

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

(define-typed-syntax (patch-added e) ≫
  [⊢ e ≫ e- ⇒ (~Patch τ _)]
  --------------------------
  [⊢ (syndicate:patch-added e-) ⇒ (AssertionSet τ)])

(define-typed-syntax (patch-removed e) ≫
  [⊢ e ≫ e- ⇒ (~Patch _ τ)]
  --------------------------
  [⊢ (syndicate:patch-removed e-) ⇒ (AssertionSet τ)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns

(define-typed-syntax (bind x:id τ:type) ≫
  ----------------------------------------
  [⊢ (error- 'bind "escaped") ⇒ (Bind τ)])

(define-typed-syntax discard
  [_ ≫
   --------------------
   ;; TODO: change void to _
   [⊢ (error- 'discard "escaped") ⇒ Discard]])

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
;; Core-ish forms

;; copied from stlc
(define-typed-syntax (ann e (~optional (~datum :)) τ:type) ≫
  [⊢ e ≫ e- ⇐ τ.norm]
  --------
  [⊢ e- ⇒ τ.norm])

;; copied from ext-stlc
(define-typed-syntax define
  [(_ x:id (~datum :) τ:type e:expr) ≫
   ;[⊢ e ≫ e- ⇐ τ.norm]
   #:with x- (generate-temporary #'x)
   --------
   [≻ (begin-
        (define-typed-variable-rename x ≫ x- : τ.norm)
        (define- x- (ann e : τ.norm)))]]
  [(_ x:id e) ≫
   ;This won't work with mutually recursive definitions
   [⊢ e ≫ e- ⇒ τ]
   #:with y (generate-temporary #'x)
   #:with y+props (transfer-props #'e- (assign-type #'y #'τ #:wrap? #f))
   --------
   [≻ (begin-
        (define-syntax x (make-rename-transformer #'y+props))
        (define- y e-))]]
  [(_ (f [x (~optional (~datum :)) ty] ... (~or (~datum →) (~datum ->)) ty_out) e ...+) ≫
   #:with f- (add-orig (generate-temporary #'f) #'f)
   --------
   [≻ (begin-
        (define-typed-variable-rename f ≫ f- : (→ ty ... ty_out))
        (define- f-
          (lambda ([x : ty] ...)
            (ann (begin e ...) : ty_out))))]])

;; copied from ext-stlc
(define-typed-syntax if
  [(_ e_tst e1 e2) ⇐ τ-expected ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   [⊢ e1 ≫ e1- ⇐ τ-expected]
   [⊢ e2 ≫ e2- ⇐ τ-expected]
   --------
   [⊢ (if- e_tst- e1- e2-)]]
  [(_ e_tst e1 e2) ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   [⊢ e1 ≫ e1- ⇒ τ1]
   [⊢ e2 ≫ e2- ⇒ τ2]
   --------
   [⊢ (if- e_tst- e1- e2-) ⇒ (U τ1 τ2)]])

;; copied from ext-stlc
(define-typed-syntax begin
  [(_ e_unit ... e) ⇐ τ_expected ≫
   [⊢ e_unit ≫ e_unit- ⇒ _] ...
   [⊢ e ≫ e- ⇐ τ_expected]
   --------
   [⊢ (begin- e_unit- ... e-)]]
  [(_ e_unit ... e) ≫
   [⊢ e_unit ≫ e_unit- ⇒ _] ...
   [⊢ e ≫ e- ⇒ τ_e]
   --------
   [⊢ (begin- e_unit- ... e-) ⇒ τ_e]])

;; copied from ext-stlc
(define-typed-syntax let
  [(_ ([x e] ...) e_body ...) ⇐ τ_expected ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   [[x ≫ x- : τ_x] ... ⊢ (begin e_body ...) ≫ e_body- ⇐ τ_expected]
   --------
   [⊢ (let- ([x- e-] ...) e_body-)]]
  [(_ ([x e] ...) e_body ...) ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   [[x ≫ x- : τ_x] ... ⊢ (begin e_body ...) ≫ e_body- ⇒ τ_body]
   --------
   [⊢ (let- ([x- e-] ...) e_body-) ⇒ τ_body]])

;; copied from ext-stlc
(define-typed-syntax let*
  [(_ () e_body ...) ≫
   --------
   [≻ (begin e_body ...)]]
  [(_ ([x e] [x_rst e_rst] ...) e_body ...) ≫
   --------
   [≻ (let ([x e]) (let* ([x_rst e_rst] ...) e_body ...))]])

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

(define-typed-syntax (empty? e) ≫
  [⊢ e ≫ e- ⇒ (~List _)]
  -----------------------
  [⊢ (empty?- e-) ⇒ Bool])

(define-typed-syntax (first e) ≫
  [⊢ e ≫ e- ⇒ (~List τ)]
  -----------------------
  [⊢ (first- e-) ⇒ τ])

(define-typed-syntax (rest e) ≫
  [⊢ e ≫ e- ⇒ (~List τ)]
  -----------------------
  [⊢ (rest- e-) ⇒ (List τ)])

(define-typed-syntax (member? e l) ≫
  [⊢ e ≫ e- ⇒ τe:type]
  [⊢ l ≫ l- ⇒ (~List τl:type)]
  #:fail-unless (<: #'τe.norm #'τl.norm) "incompatible list"
  ----------------------------------------
  [⊢ (member?- e- l-) ⇒ Bool])

(define- (member?- v l)
  (and- (member- v l) #t))

(define-typed-syntax (displayln e:expr) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------
  [⊢ (displayln- e-) (⇒ : (U))])

(define-typed-syntax (assert e) ≫
  --------------------------------------------------------
  [≻ (patch (make-assertion-set e) (make-assertion-set))])

(define-typed-syntax (retract e) ≫
  --------------------------------------------------------
  [≻ (patch (make-assertion-set) (make-assertion-set e))])

(define-typed-syntax (sub e) ≫
  -----------------------------
  [≻ (assert (observe e))])

(define-typed-syntax (unsub e) ≫
  -----------------------------
  [≻ (retract (observe e))])

(define-typed-syntax (patch-seq* e) ≫
  [⊢ e ≫ e- ⇒ (~List τ)]
  #:with (~or* (~Patch τa τr)
               (~and (~U* (~Patch τai τri) ...)
                     (~parse (τa τr) #'((U τai ...) (U τri ...)))))
         #'τ
  -------------------------------------
  [⊢ (syndicate:patch-seq* e-) ⇒ (Patch τa τr)])

(define-typed-syntax (patch-seq e ...) ≫
  -----------------------------
  [≻ (patch-seq* (list e ...))])
  

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
;; Sets

(define-typed-syntax (set e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  ---------------
  [⊢ (set- e- ...) ⇒ (Set (U τ ...))])

(define-typed-syntax (set-count e) ≫
  [⊢ e ≫ e- ⇒ (~Set _)]
  ----------------------
  [⊢ (set-count- e-) ⇒ Int])

(define-typed-syntax (set-add st v) ≫
  [⊢ st ≫ st- ⇒ (~Set τs)]
  [⊢ v ≫ v- ⇒ τv]
  -------------------------
  [⊢ (set-add- st- v-) ⇒ (Set (U τs τv))])

(define-typed-syntax (set-member? st v) ≫
  [⊢ st ≫ st- ⇒ (~Set τs:type)]
  [⊢ v ≫ v- ⇒ τv:type]
  #:fail-unless (<: #'τv.norm #'τs.norm)
    "type mismatch"
  -------------------------------------
  [⊢ (set-member?- st- v-) ⇒ Bool])

(define-typed-syntax (set-union st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  [⊢ st ≫ st- ⇒ (~Set τ-st)] ...
  -------------------------------------
  [⊢ (set-union- st0- st- ...) ⇒ (Set (U τ-st0 τ-st ...))])

(define-typed-syntax (set-intersect st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  [⊢ st ≫ st- ⇒ (~Set τ-st)] ...
  #:with τr (∩ #'τ-st0 (type-eval #'(U τ-st ...)))
  -------------------------------------
  [⊢ (set-intersect- st0- st- ...) ⇒ (Set τr)])

(define-typed-syntax (set-subtract st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  [⊢ st ≫ st- ⇒ (~Set _)] ...
  -------------------------------------
  [⊢ (set-subtract- st0- st- ...) ⇒ (Set τ-st0)])

(define-typed-syntax (list->set l) ≫
  [⊢ l ≫ l- ⇒ (~List τ)]
  -----------------------
  [⊢ (list->set- l-) ⇒ (Set τ)])

(define-typed-syntax (set->list s) ≫
  [⊢ s ≫ s- ⇒ (~Set τ)]
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
;; Utilities

(define-typed-syntax (print-type e) ≫
  [⊢ e ≫ e- ⇒ τ:type]
  #:do [(displayln (type->str #'τ.norm))]
  ----------------------------------
  [⊢ e- ⇒ τ])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extensions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

;; project
(module+ test
  

  (check-type (project [(tuple) (make-assertion-set (tuple 1 2))]
                (tuple))
              : (List (Tuple))
              -> (list-))

  (check-type (project [(tuple) (make-assertion-set (tuple))]
                (tuple))
              : (List (Tuple))
              -> (list- (tuple)))

  (check-type (project [(tuple (bind x Int) 2) (make-assertion-set (tuple 1 2))]
                x)
              : (List Int)
              -> (list- 1))

  (check-type (project [(tuple (bind x Int) 2) (make-assertion-set (tuple 1 2) "hello")]
                x)
              : (List Int)
              -> (list- 1))

  (check-type (project [(tuple (bind x (U Int (Tuple Int Int))) 2)
                        (make-assertion-set (tuple 1 2)
                                            "hello"
                                            (tuple (tuple 4 5) 2))]
           
                x)
              : (List (U Int (Tuple Int Int)))
              -> (list- (tuple 4 5) 1))

  ;; nested project to test for ambiguous binding error
  (check-type (project [(tuple (bind x Int) 2) (make-assertion-set (tuple 1 2))]
                (project [(tuple discard x) (make-assertion-set (tuple "bizboz" 1))]
                  x))
              : (List (List Int))
              -> (list- (list- 1))))

;; fold
(module+ test
  (check-type (for/fold (sum 0)
                        (x (list 1 2 3))
                (typed-app + x sum))
              : Int
              -> 6))

;; functions
(module+ test
  (check-type (lambda ([x Int]) x) : (→ Int Int))
  (check-type (typed-app (lambda ([x : Int]) x) 5)
              : Int
              -> 5))

;; patches
(module+ test
  (check-type (patch-added (patch (make-assertion-set 12) (make-assertion-set)))
              : (AssertionSet Int)
              -> (make-assertion-set 12))
  (check-type (patch-removed (patch (make-assertion-set 12) (make-assertion-set)))
              : (AssertionSet (U))
              -> (make-assertion-set))
  ;; patch utilities
  (check-type (patch-seq* (list (assert 1) (assert 2)))
              : (Patch Int ⊥)
              -> (patch (make-assertion-set 1 2) (make-assertion-set)))
  (check-type (patch-seq* (list (assert 1) (retract (tuple "humpty" 42)) (assert "hello")))
              : (Patch (U Int String) (Tuple String Int))
              -> (patch (make-assertion-set 1 "hello") (make-assertion-set (tuple "humpty" 42)))))

;; tuples
(module+ test
  (typecheck-fail (select 0 (tuple)))
  (check-type (select 0 (tuple 18))
              : Int
              -> 18))

;; not reproducing an issue with using ⊥
(module+ test
  (check-type (lambda ([e : (Event ⊥)]
                       [s : ★/t])
                idle)
              : (→ (Event ⊥) ★/t (Instruction ⊥ ⊥ ⊥))))

;; transition
(module+ test
  (check-type (transition #f (list))
              : (Instruction Bool ⊥ ⊥)
              -> (syndicate:transition #f (list-)))
  (check-type (quit) : (Instruction ⊥ ⊥ ⊥))
  (check-type (quit (list)) : (Instruction ⊥ ⊥ ⊥)))
