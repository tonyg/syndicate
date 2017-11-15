#lang turnstile

(provide (rename-out [syndicate:#%module-begin #%module-begin])
         (rename-out [typed-app #%app])
         (rename-out [syndicate:begin-for-declarations declare-types])
         #%top-interaction
         #%top
         require only-in
         ;; Types
         Int Bool String Tuple Bind Discard Case → Behavior FacetName Field ★
         Observe Inbound Outbound Actor U
         ;; Statements
         let-function spawn dataspace facet set! begin stop unsafe-do
         ;; endpoints
         assert on
         ;; expressions
         tuple λ ref observe inbound outbound
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         + - displayln
         ;; DEBUG and utilities
         define-type-alias
         print-type
         (rename-out [printf- printf])
         )

(require (rename-in racket/match [match-lambda match-lambda-]))
(require (prefix-in syndicate: syndicate/actor-lang))

(module+ test
  (require rackunit)
  (require turnstile/rackunit-typechecking))

;(require syndicate/actor-lang)
#;(provide (all-from-out syndicate/actor-lang))

(define-base-types Int Bool String Discard ★ FacetName)
(define-type-constructor Field #:arity = 1)
;; (Behavior τv τi τo τa)
;; τv is the type of thing it evaluates to
;; τi is the type of patterns used to consume incoming assertions
;; τo is the type of assertions made
;; τa is the type of spawned actors
(define-type-constructor Behavior #:arity = 4)
(define-type-constructor Bind #:arity = 1)
(define-type-constructor Tuple #:arity >= 0)
(define-type-constructor U #:arity >= 0)
(define-type-constructor Case #:arity >= 0)
(define-type-constructor → #:arity > 0)
(define-type-constructor Observe #:arity = 1)
(define-type-constructor Inbound #:arity = 1)
(define-type-constructor Outbound #:arity = 1)
(define-type-constructor Actor #:arity = 1)

(begin-for-syntax
  (define-syntax-class exp
    #:datum-literals (tuple λ ref)
    (pattern (~or (o:prim-op es:exp ...)
                  basic-val
                  (k:kons1 e:exp)
                  (tuple es:exp ...)
                  (ref x:id)
                  (λ [p:pat s:stmt] ...))))

  ;; constructors with arity one
  (define-syntax-class kons1
    (pattern (~or (~literal observe)
                  (~literal inbound)
                  (~literal outbound))))

  (define-syntax-class basic-val
    (pattern (~or boolean
                  integer
                  string)))

  (define-syntax-class prim-op
    (pattern (~or (~literal +)
                  (~literal -)
                  (~literal displayln))))

  (define-syntax-class stmt
    #:datum-literals (:
                      begin
                      let-function
                      set!
                      spawn
                      dataspace
                      stop
                      facet
                      unsafe-do
                      fields)
    (pattern (~or (begin seq:stmt ...)
                  (e1:exp e2:exp)
                  (let-function [f:id e:exp] let-fun-body:stmt)
                  (set! x:id e:exp)
                  (spawn τ:type s:stmt)
                  (dataspace τ:type nested:stmt ...)
                  (stop x:id s:stmt)
                  (facet x:id (fields [fn:id τf:type ef:exp] ...) ep:endpoint ...+)
                  ;; note racket expr, not exp
                  (unsafe-do rkt:expr ...))))

  (define-syntax-class endpoint
    #:datum-literals (on start stop)
    (pattern (~or (on ed:event-desc s:stmt)
                  (assert e:exp))))

  (define-syntax-class event-desc
    #:datum-literals (start stop asserted retracted)
    (pattern (~or start
                  stop
                  (asserted p:pat)
                  (retracted p:pat))))
                  
  (define-syntax-class pat
    #:datum-literals (tuple _ discard bind)
    #:attributes (syndicate-pattern match-pattern)
    (pattern (~or (~and (tuple ps:pat ...)
                        (~bind [syndicate-pattern #'(list 'tuple ps.syndicate-pattern ...)]
                               [match-pattern #'(list 'tuple ps.match-pattern ...)]))
                  (~and (k:kons1 p:pat)
                        ;; not sure if this gets the binding of k right
                        (~bind [syndicate-pattern #'(k p.syndicate-pattern)]
                               [match-pattern #'(k p.match-pattern)]))
                  (~and (bind ~! x:id τ:type)
                        (~bind [syndicate-pattern #'($ x)]
                               [match-pattern #'x]))
                  (~and discard
                        (~bind [syndicate-pattern #'_]
                               [match-pattern #'_]))
                  (~and x:id
                        (~bind [syndicate-pattern #'x]
                               [match-pattern #'(== x)]))
                  (~and e:exp
                        (~bind [syndicate-pattern #'e]
                               [match-pattern #'(== e)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

;; TODO: subtyping for facets

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
    [((~Behavior τ-v1 τ-i1 τ-o1 τ-a1) (~Behavior τ-v2 τ-i2 τ-o2 τ-a2))
     (and (<: #'τ-v1 #'τ-v2)
          ;; HMMMMMM. i2 and i1 are types of patterns. TODO
          ;; Want: ∀σ. project-safe(σ, τ-i2) ⇒ project-safe(σ, τ-i1)
          (<: #'τ-i2 #'τ-i1)
          (<: #'τ-o1 #'τ-o2)
          (<: (type-eval #'(Actor τ-a1)) (type-eval #'(Actor τ-a2))))]
    [((~→ τ-in1 ... τ-out1) (~→ τ-in2 ... τ-out2))
     #:when (stx-length=? #'(τ-in1 ...) #'(τ-in2 ...))
     (and (stx-andmap <: #'(τ-in2 ...) #'(τ-in1 ...))
          (<: #'τ-out1 #'τ-out2))]
    [((~Field τ1) (~Field τ2))
     (and (<: #'τ1 #'τ2)
          (<: #'τ2 #'τ1))]
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

;; CONVENTIONS
;; The `:` key is for evaluated expressions
;; The `:i` key is for input patterns
;; The `:o` key is for output assertions
;; The `:a` key is for spawned actors

(define-typed-syntax (set! x:id e:exp) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  [⊢ x ≫ x- (⇒ : (~Field τ-x:type))]
  #:fail-unless (<: #'τ #'τ-x) "Ill-typed field write"
  ----------------------------------------------------
  [⊢ (x- e-) (⇒ : (U)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

(define-typed-syntax (stop facet-name:id cont:stmt) ≫
  [⊢ facet-name ≫ facet-name- (⇐ : FacetName)]
  [⊢ cont ≫ cont- (⇒ :i τ-i) (⇒ :o τ-o) (⇒ :a τ-a)]
  --------------------------------------------------
  [⊢ (syndicate:stop facet-name- cont-) (⇒ : (U)) (⇒ :i τ-i) (⇒ :o τ-o) (⇒ :a τ-a)])

(define-typed-syntax (facet name:id ((~datum fields) [x:id τ:type e:exp] ...) ep:endpoint ...+) ≫
  #:fail-unless (stx-andmap flat-type? #'(τ ...)) "keep your uppity data outa my fields"
  [⊢ e ≫ e- (⇐ : τ)] ...
  [[name ≫ name- : FacetName] [x ≫ x- : (Field τ)] ...
   ⊢ [ep ≫ ep- (⇒ : τ-i) (⇒ :o τ-o) (⇒ :a τ-a)] ...]
  --------------------------------------------------------------
  ;; name NOT name- here because I get an error that way.
  ;; Since name is just an identifier I think it's OK?
  [⊢ (syndicate:react (let- ([name- (syndicate:current-facet-id)])
                            #,(make-fields #'(x- ...) #'(e- ...))
                            #;(syndicate:field [x- e-] ...)
                            ep- ...))
     (⇒ : (U)) (⇒ :i (U τ-i ...)) (⇒ :o (U τ-o ...)) (⇒ :a (U τ-a ...))])

(define-for-syntax (make-fields names inits)
  (syntax-parse #`(#,names #,inits)
    [((x:id ...) (e ...))
     #'(syndicate:field [x e] ...)]))

(define-typed-syntax (dataspace τ-c:type s:stmt ...) ≫
  ;; #:do [(printf "τ-c: ~a\n" (type->str #'τ-c.norm))]
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ :i τ-i:type) (⇒ :o τ-o:type) (⇒ :a τ-s:type)] ...
  ;; #:do [(printf "dataspace types: ~a\n" (stx-map type->str #'(τ-s.norm ...)))
  ;;      (printf "dataspace type: ~a\n" (type->str ((current-type-eval) #'(Actor τ-c.norm))))]
  #:fail-unless (stx-andmap (lambda (t) (<: (type-eval #`(Actor #,t))
                                            (type-eval #'(Actor τ-c.norm))))
                            #'(τ-s.norm ...))
                "Not all actors conform to communication type"
  #:fail-unless (stx-andmap (lambda (t) (<: t (type-eval #'(U))))
                            #'(τ-i.norm ...)) "dataspace init should only be a spawn"
  #:fail-unless (stx-andmap (lambda (t) (<: t (type-eval #'(U))))
                            #'(τ-o.norm ...)) "dataspace init should only be a spawn"
  #:with τ-ds-i (strip-inbound #'τ-c.norm)
  #:with τ-ds-o (strip-outbound #'τ-c.norm)
  #:with τ-relay (relay-interests #'τ-c.norm)
  -----------------------------------------------------------------------------------
  [⊢ (syndicate:dataspace s- ...) (⇒ : (U)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U τ-ds-i τ-ds-o τ-relay))])

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

(define-typed-syntax (spawn τ-c:type s:stmt) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ :i τ-i:type) (⇒ :o τ-o:type) (⇒ :a τ-a:type)]
  ;; TODO: s shouldn't refer to facets or fields!
  #:fail-unless (<: #'τ-o.norm #'τ-c.norm)
                (format "Output ~a not valid in dataspace ~a" (type->str #'τ-o) (type->str #'τ-c))
  #:fail-unless (<: (type-eval #'(Actor τ-a.norm))
                    (type-eval #'(Actor τ-c.norm))) "Spawned actors not valid in dataspace"
  #:fail-unless (project-safe? (∩ (strip-? #'τ-o.norm) #'τ-c.norm)
                               #'τ-i.norm) "Not prepared to handle all inputs"
  --------------------------------------------------------------------------------------------
  [⊢ (syndicate:spawn (syndicate:on-start s-)) (⇒ : (U)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a τ-c)])

(define-typed-syntax (let-function [f:id e:expr] body:expr) ≫
  [⊢ e ≫ e- (⇒ : τ:type)]
  #:fail-unless (procedure-type? #'τ.norm)
                (format "let-function only binds procedures; got ~a" (type->str #'τ.norm))
  [[f ≫ f- : τ] ⊢ body ≫ body- (⇒ : τ-body) (⇒ :i τ-body-i) (⇒ :o τ-body-o) (⇒ :a τ-body-a)]
   ------------------------------------------------------------------------
  [⊢ (let- ([f- e-]) body-) (⇒ : τ-body) (⇒ :i τ-body-i) (⇒ :o τ-body-o) (⇒ :a τ-body-a)])

(define-for-syntax (procedure-type? τ)
  (syntax-parse τ
    [(~→ τ ...+) #t]
    [_ #f]))

(define-typed-syntax (begin s:stmt ...) ≫
  [⊢ s ≫ s- (⇒ :i τ1) (⇒ :o τ2) (⇒ :a τ3)] ...
  ------------------------------------------
  [⊢ (begin- (void-) s- ...) (⇒ : (U)) (⇒ :i (U τ1 ...)) (⇒ :o (U τ2 ...)) (⇒ :a (U τ3 ...))])

(define-for-syntax (flat-type? τ)
  (syntax-parse τ
    [(~→ τ ...) #f]
    [(~Field _) #f]
    [(~Behavior _ _ _ _) #f]
    [_ #t]))

(define-typed-syntax (unsafe-do rkt:expr ...) ≫
  ------------------------
  [⊢ (let- () rkt ...) (⇒ : (U)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Endpoints

(begin-for-syntax
  (define-syntax-class asserted-or-retracted
    #:datum-literals (asserted retracted)
    (pattern (~or (~and asserted
                        (~bind [syndicate-kw #'syndicate:asserted]))
                  (~and retracted
                        (~bind [syndicate-kw #'syndicate:retracted]))))))

(define-typed-syntax on
  [(on (~literal start) s:stmt) ≫
   [⊢ s ≫ s- (⇒ :i τi) (⇒ :o τ-o) (⇒ :a τ-a)]
   -----------------------------------
   [⊢ (syndicate:on-start s-) (⇒ : (U)) (⇒ :i τi) (⇒ :o τ-o) (⇒ :a τ-a)]]
  [(on (~literal stop) s:stmt) ≫
   [⊢ s ≫ s- (⇒ : τi) (⇒ :o τ-o) (⇒ :a τ-a)]
   -----------------------------------
   [⊢ (syndicate:on-stop s-) (⇒ : (U)) (⇒ :i τi) (⇒ :o τ-o) (⇒ :a τ-a)]]
  [(on (a/r:asserted-or-retracted p:pat) s:stmt) ≫
   [⊢ p ≫ _ ⇒ τp]
   #:with ([x:id τ:type] ...) (pat-bindings #'p)
   [[x ≫ x- : τ] ... ⊢ s ≫ s- (⇒ : τi) (⇒ :o τ-o) (⇒ :a τ-a)]
   ;; the type of subscriptions to draw assertions to the pattern
   #:with pat-sub (replace-bind-and-discard-with-★ #'τp)
   -----------------------------------
   [⊢ (syndicate:on (a/r.syndicate-kw p.syndicate-pattern)
                    (let- ([x- x] ...) s-))
      (⇒ : (U))
      (⇒ :i (U τi τp))
      (⇒ :o (U (Observe pat-sub) τ-o))
      (⇒ :a τ-a)]])

;; FlattishType -> FlattishType
(define-for-syntax (replace-bind-and-discard-with-★ t)
  (syntax-parse t
    [(~Bind _)
     (type-eval #'★)]
    [~Discard
     (type-eval #'★)]
    [(~U τ ...)
     (type-eval #`(U #,@(stx-map replace-bind-and-discard-with-★ #'(τ ...))))]
    [(~Tuple τ ...)
     (type-eval #`(Tuple #,@(stx-map replace-bind-and-discard-with-★ #'(τ ...))))]
    [(~Observe τ)
     (type-eval #`(Observe #,(replace-bind-and-discard-with-★ #'τ)))]
    [(~Inbound τ)
     (type-eval #`(Inbound #,(replace-bind-and-discard-with-★ #'τ)))]
    [(~Outbound τ)
     (type-eval #`(Outbound #,(replace-bind-and-discard-with-★ #'τ)))]
    [_ t]))

(define-typed-syntax (assert e:exp) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:with τ-in (strip-? #'τ.norm)
  -------------------------------------
  [⊢ (syndicate:assert e-) (⇒ : (U)) (⇒ :i τ-in) (⇒ :o τ) (⇒ :a (U))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions

(define-typed-syntax (tuple e:exp ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  -----------------------
  [⊢ (list 'tuple e- ...) (⇒ : (Tuple τ ...)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

(define-typed-syntax (ref x:id) ≫
  [⊢ x ≫ x- ⇒ (~Field τ)]
  ------------------------
  [⊢ (x-) (⇒ : τ) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

(define-typed-syntax (λ [p:pat s:stmt] ...) ≫
  #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
  [[x ≫ x- : τ] ... ⊢ s ≫ s- (⇒ : τv) (⇒ :i τ1) (⇒ :o τ2) (⇒ :a τ3)] ...
  ;; REALLY not sure how to handle p/p-/p.match-pattern,
  ;; particularly w.r.t. typed terms that appear in p.match-pattern
  [⊢ p ≫ p- ⇒ τ-p] ...
  #:with (τ-in ...) (stx-map lower-pattern-type #'(τ-p ...))
  --------------------------------------------------------------
  ;; TODO: add a catch-all error clause
  [⊢ (match-lambda- [p.match-pattern (let- ([x- x] ...) s-)] ...)
     (⇒ : (→ (U τ-in ...) (Behavior (U τv ...) (U τ1 ...) (U τ2 ...) (U τ3 ...))))
     (⇒ :i (U))
     (⇒ :o (U))
     (⇒ :a (U))])

;; FlattishType -> FlattishType
;; replaces (Bind τ) with τ and Discard with ★
(define-for-syntax (lower-pattern-type t)
  (syntax-parse t
    [(~Bind τ)
     #'τ]
    [~Discard
     (type-eval #'★)]
    [(~U τ ...)
     (type-eval #`(U #,@(stx-map replace-bind-and-discard-with-★ #'(τ ...))))]
    [(~Tuple τ ...)
     (type-eval #`(Tuple #,@(stx-map replace-bind-and-discard-with-★ #'(τ ...))))]
    [(~Observe τ)
     (type-eval #`(Observe #,(replace-bind-and-discard-with-★ #'τ)))]
    [(~Inbound τ)
     (type-eval #`(Inbound #,(replace-bind-and-discard-with-★ #'τ)))]
    [(~Outbound τ)
     (type-eval #`(Outbound #,(replace-bind-and-discard-with-★ #'τ)))]
    [_ t]))

(define-typed-syntax (typed-app e_fn e_arg ...) ≫
  [⊢ e_fn ≫ e_fn- (⇒ : (~→ τ_in ... (~Behavior τ-v τ-i τ-o τ-a)))]
  #:fail-unless (stx-length=? #'[τ_in ...] #'[e_arg ...])
                (num-args-fail-msg #'e_fn #'[τ_in ...] #'[e_arg ...])
  [⊢ e_arg ≫ e_arg- (⇐ : τ_in)] ...
  ------------------------------------------------------------------------
  [⊢ (#%app- e_fn- e_arg- ...) (⇒ : τ-v) (⇒ :i τ-i) (⇒ :o τ-o) (⇒ :a τ-a)])

;; it would be nice to abstract over these three
(define-typed-syntax (observe e:exp) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:observe e-) (⇒ : (Observe τ)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

(define-typed-syntax (inbound e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:inbound e-) (⇒ : (Inbound τ)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

(define-typed-syntax (outbound e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------------------------------------------------------------------
  [⊢ (syndicate:outbound e-) (⇒ : (Outbound τ)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns

(define-typed-syntax (bind x:id τ:type) ≫
  ----------------------------------------
  ;; TODO: at some point put $ back in
  [⊢ (void-) (⇒ : (Bind τ)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])

(define-typed-syntax discard
  [_ ≫
   --------------------
   ;; TODO: change void to _
   [⊢ (void-) (⇒ : Discard) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))]])

;; pat -> ([Id Type] ...)
(define-for-syntax (pat-bindings stx)
  (syntax-parse stx
    #:datum-literals (bind tuple)
    [(bind x:id τ:type)
     #'([x τ])]
    [(tuple p:pat ...)
     #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
     #'([x τ] ... ...)]
    [_:pat
     #'()]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives

;; hmmm
(define-primop + (→ Int Int (Behavior Int (U) (U) (U))))
(define-primop - (→ Int Int (Behavior Int (U) (U) (U))))

(define-typed-syntax (displayln e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  ---------------
  [⊢ (displayln- e-) (⇒ : (U)) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))])
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Values

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
  ----------------
  [⊢ (#%datum- . n) (⇒ : Int) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))]]
  [(_ . b:boolean) ≫
  ----------------
  [⊢ (#%datum- . b) (⇒ : Bool) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))]]
  [(_ . s:string) ≫
  ----------------
  [⊢ (#%datum- . s) (⇒ : String) (⇒ :i (U)) (⇒ :o (U)) (⇒ :a (U))]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

#;(define-syntax (begin/void-default stx)
  (syntax-parse stx
    [(_)
     (syntax/loc stx (void))]
    [(_ expr0 expr ...)
     (syntax/loc stx (begin- expr0 expr ...))]))

(define-for-syntax (type-eval t)
  ((current-type-eval) t))

(define-typed-syntax (print-type e) ≫
  [⊢ e ≫ e- ⇒ τ]
  #:do [(displayln (type->str #'τ))]
  ----------------------------------
  [⊢ e- ⇒ τ])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

;; WANTED UNIT TESTS
;; (check-true (<: #'(U String) #'String))
;; (check-true (<: #'(U (U)) #'String))
;; (check-true (<: #'(Actor (U (U))) #'(Actor String))
;; (check-true (<: #'(Actor (U (U))) #'(Actor (U (Observe ★) String)))
;; (check-true (<: ((current-type-eval) #'(U (U) (U))) ((current-type-eval) #'(U))))
;; (check-false (<: ((current-type-eval) #'(Actor (U (Observe ★) String Int)))
;;                  ((current-type-eval) #'(Actor (U (Observe ★) String)))))
;; (check-true (<: (Actor (U (Observe ★) String)) (Actor (U (Observe ★) String)))

(module+ test
  (check-type 1 : Int)

  (check-type (tuple 1 2 3) : (Tuple Int Int Int))

  (check-type (tuple discard 1 (bind x Int)) : (Tuple Discard Int (Bind Int)))

  #;(check-type (λ [(bind x Int) (begin)]) : (Case [→ (Bind Int) (Facet (U) (U) (U))]))
  #;(check-true (void? ((λ [(bind x Int) (begin)]) 1))))

(define bank-account
  #'(facet account
           (let-field [balance : Int 0]
                      ;; this bit isn't valid because `assert` is not a statement
                      (begin (assert (tuple "balance" (ref balance)))
                             (on (asserted (tuple "deposit" (bind amount Int)))
                                 (set! balance (+ (ref balance) amount)))))))

(define client
  #'(spawn
     (let-field [deposits : Int 3]
                (facet client
                       (on (asserted (tuple "balance" (bind amount Int)))
                           ;; this bit isn't valid because `assert` is not a statement
                           (begin (assert (tuple "deposit" 100))
                                  (set! deposits (- (ref deposits) 1))
                                  ;; also `if` isn't in the grammar
                                  (if (= (ref deposits) 0)
                                      (stop client (begin))
                                      (begin))))))))

(define-syntax (test-syntax-class stx)
  (syntax-parse stx
    [(_ e class:id)
     #`(let ()
         (define-syntax (test-result stx)
           (syntax-parse e
             [(~var _ class) #'#t]
             [_ #'#f]))
         (test-result))]))

#;(begin-for-syntax
    (displayln (syntax-parse ((current-type-eval) #'(U String))
                 [(~U τ ...)
                  #'(τ ...)]
                 [_ 'boo])))

#;(define-typed-syntax (λ2 ([x:id τ:type] ...) e:expr ...+) ≫
  [[x ≫ x- : τ] ... ⊢ (e ≫ e-  ⇒ τ-e) ...]
  ;;#:do ((printf "~v\n" #'((x- ...) ...)))
  ------------------------------
  [⊢ (lambda- (x- ...) e- ...)
     ⇒ (→ τ ... #,(last (stx->list #'(τ-e ...))))])

#;(define-syntax (#%top stx)
  (printf "my #%top\n")
  #'f)