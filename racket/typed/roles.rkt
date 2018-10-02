#lang turnstile

(provide (rename-out [syndicate:#%module-begin #%module-begin])
         (rename-out [typed-app #%app])
         #%top-interaction
         require only-in
         ;; Types
         (all-from-out "base-types.rkt")
         ;; Statements
         let let* if spawn dataspace start-facet set! begin stop begin/dataflow #;unsafe-do
         when unless send!
         ;; Derived Forms
         during define/query-value define/query-set
         ;; endpoints
         assert on field
         ;; expressions
         tuple select lambda ref observe inbound outbound
         ;; making types
         (all-from-out "user-ctors.rkt")
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         (except-out (all-from-out "define-like-things.rkt") define)
         (rename-out [define/fun define])
         (all-from-out "prim.rkt")
         ;; lists, sets
         (all-from-out "list.rkt")
         (all-from-out "set.rkt")
         ;; DEBUG and utilities
         print-type print-role
         ;; Extensions
         match cond
         ;; require & provides
         require provide
         )

(require (prefix-in syndicate: syndicate/actor-lang))
(require "base-types.rkt"
         "effects.rkt"
         "prim.rkt"
         "judgments.rkt"
         "user-ctors.rkt"
         "list.rkt"
         "set.rkt"
         "define-like-things.rkt")

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx))

(require macrotypes/postfix-in)
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
;; f key aggregates facet effects (starting a facet) as `Role`s and message sends, `Sends`
;; s key aggregates spawned actors as `Actor`s

(define-for-syntax (type-eval t)
  ((current-type-eval) t))

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
                  (~datum outbound)
                  (~datum message))))

  (define (kons1->constructor stx)
    (syntax-parse stx
      #:datum-literals (observe inbound outbound)
      [observe #'syndicate:observe]
      [inbound #'syndicate:inbound]
      [outbound #'syndicate:outbound]
      [message #'syndicate:message])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities Over Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (bot? t)
  (<: t (type-eval #'(U*))))

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
     (define t (replace-bind-and-discard-with-★ #'τ))
     (type-eval #`(Observe #,t))]))

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
    [(~Message τ)
     (type-eval #`(Message #,(replace-bind-and-discard-with-★ #'τ)))]
    [(~constructor-type _ τ ...)
     (make-cons-type t (stx-map replace-bind-and-discard-with-★ #'(τ ...)))]
    [_ t]))


;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; MODIFYING GLOBAL TYPECHECKING STATE!!!!!
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(begin-for-syntax
  (current-typecheck-relation <:))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
               ;; untyped syndicate might allow this - TODO
               #;(~and τ-m (~Sends _))
               (~and τ-r (~Reacts _ ...))
               ~MakesField)
          ...)
         ep-effects
  #:with τ (type-eval #`(Role (#,name--)
                          τ-a ...
                          ;; τ-m ...
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

(define-typed-syntax (assert e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τs (type-eval #'(Shares τ))
  -------------------------------------
  [⊢ (syndicate:assert e-) (⇒ : ★/t)
                           (⇒ ep (τs))])

(define-typed-syntax (send! e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τm (type-eval #'(Sends τ))
  --------------------------------------
  [⊢ (syndicate:send! e-) (⇒ : ★/t)
                          (⇒ f (τm))])

(define-typed-syntax (stop facet-name:id cont ...) ≫
  [⊢ facet-name ≫ facet-name- (⇐ : FacetName)]
  [⊢ (begin #f cont ...) ≫ cont- (⇒ ep (~effs)) (⇒ s (~effs)) (⇒ f (~effs τ-f ...))]
  #:with τ (mk-Stop- #`(facet-name- τ-f ...))
  ---------------------------------------------------------------------------------
  [⊢ (syndicate:stop-facet facet-name- cont-) (⇒ : ★/t)
                                              (⇒ f (τ))])

(begin-for-syntax
  (define-syntax-class asserted/retracted/message
    #:datum-literals (asserted retracted message)
    (pattern (~or (~and asserted
                        (~bind [syndicate-kw #'syndicate:asserted]
                               [react-con #'Know]))
                  (~and retracted
                        (~bind [syndicate-kw #'syndicate:retracted]
                               [react-con #'¬Know]))
                  (~and message
                        (~bind [syndicate-kw #'syndicate:message]
                               [react-con #'Message]))))))

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
  [(on (a/r/m:asserted/retracted/message p) s ...) ≫
   [⊢ p ≫ p-- (⇒ : τp)]
   #:fail-unless (pure? #'p--) "pattern not allowed to have effects"
   #:with ([x:id τ:type] ...) (pat-bindings #'p)
   [[x ≫ x- : τ] ... ⊢ (begin s ...) ≫ s-
                 (⇒ ep (~effs))
                 (⇒ f (~effs τ-f ...))
                 (⇒ s (~effs τ-s ...))]
   #:with p- (substs #'(x- ...) #'(x ...) (compile-syndicate-pattern #'p))
   #:with τ-r (type-eval #'(Reacts (a/r/m.react-con τp) τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on (a/r/m.syndicate-kw p-)
                    s-)
      (⇒ : ★/t)
      (⇒ ep (τ-r))]])

(define-typed-syntax (begin/dataflow s ...+) ≫
  [⊢ (begin s ...) ≫ s-
     (⇒ : _)
     (⇒ ep (~effs))
     (⇒ f (~effs τ-f ...))
     (⇒ s (~effs τ-s ...))]
  #:with τ-r (type-eval #'(Reacts OnDataflow τ-f ... τ-s ...))
  --------------------------------------------------
  [⊢ (syndicate:begin/dataflow s-)
     (⇒ : ★/t)
     (⇒ ep (τ-r))])

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
      ;; not sure about this
      [discard
       #'discard]
      [(~constructor-exp ctor p ...)
       (define/with-syntax uctor (untyped-ctor #'ctor))
       #`(ctor #,@(stx-map loop #'(p ...)))]
      [_
       pat])))

(define-typed-syntax (define/query-value x:id e0 p e) ≫
  [⊢ e0 ≫ e0- (⇒ : τ)]
  #:fail-unless (pure? #'e0-) "expression must be pure"
  ----------------------------------------
  [≻ (begin (field [x τ e0-])
            (on (asserted p)
                (set! x e))
            (on (retracted p)
                (set! x e0-)))])

;; TODO: #:on-add
(define-typed-syntax (define/query-set x:id p e) ≫
  #:with ([y τ] ...) (pat-bindings #'p)
  ;; e will be re-expanded :/
  [[y ≫ y- : τ] ... ⊢ e ≫ e- ⇒ τ-e]
  ----------------------------------------
  [≻ (begin (field [x (Set τ-e) (set)])
            (on (asserted p)
                (set! x (set-add (ref x) e)))
            (on (retracted p)
                (set! x (set-remove (ref x) e))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (ref x:id) ≫
  [⊢ x ≫ x- ⇒ (~Field τ)]
  ------------------------
  [⊢ (x-) (⇒ : τ)])

(define-typed-syntax (lambda ([x:id (~optional (~datum :)) τ:type] ...) body ...+) ≫
  [[x ≫ x- : τ] ... ⊢ (begin body ...) ≫ body- (⇒ : τ-e)
                (⇒ ep (~effs τ-ep ...))
                (⇒ s (~effs τ-s ...))
                (⇒ f (~effs τ-f ...))]
  ----------------------------------------
  [⊢ (lambda- (x- ...) body-) (⇒ : (→ τ ... (Computation (Value τ-e)
                                                         (Endpoints τ-ep ...)
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

(define-typed-syntax (message e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  ------------------------------
  [⊢ (syndicate:message e-) (⇒ : (Message τ))])

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

;; add support for function definitions to define, otherwise falling back to `define` from
;; "define-like-things.rkt"
(define-typed-syntax define/fun
  [(_ (f [x (~optional (~datum :)) ty:type] ...
         (~or (~datum →) (~datum ->)) ty_out:type)
         e ...+) ≫
   [⊢ (lambda ([x : ty] ...) (begin e ...)) ≫ e- (⇒ : (~and fun-ty
                                                            (~→ (~not (~Computation _ ...)) ...
                                                                (~Computation (~Value τ-v)
                                                                              _ ...))))]
   #:fail-unless (<: #'τ-v #'ty_out.norm)
     (format "expected different return type\n got ~a\n expected ~a\n"
       (type->str #'τ-v)
       (type->str #'ty_out))
   #:with f- (add-orig (generate-temporary #'f) #'f)
   --------
   [⊢ (define/intermediate f f- fun-ty e-) (⇒ : ★/t)]]
  [(_ (f [x (~optional (~datum :)) ty] ...)
         e ...+) ≫
   ----------------------------
   [≻ (define/fun (f [x ty] ... -> ★/t) e ...)]]
  [(_ e ...+) ≫
   ---------------------------------------------
   [≻ (define e ...)]])

;; copied from stlc
(define-typed-syntax (ann e (~optional (~datum :)) τ:type) ≫
  [⊢ e ≫ e- (⇐ : τ.norm)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  ------------------------------------------------
  [⊢ e- (⇒ : τ.norm) ])

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

(define-typed-syntax (when e s ...+) ≫
  ------------------------------------
  [≻ (if e (begin s ...) #f)])

(define-typed-syntax (unless e s ...+) ≫
  ------------------------------------
  [≻ (if e #f (begin s ...))])

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
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-type (spawn (U (Message (Tuple String Int))
                        (Observe (Tuple String ★/t)))
                     (start-facet echo
                                  (on (message (tuple "ping" (bind x Int)))
                                      (send! (tuple "pong" x)))))
              : ★/t)
  (typecheck-fail (spawn (U (Message (Tuple String Int))
                            (Message (Tuple String String))
                            (Observe (Tuple String ★/t)))
                         (start-facet echo
                                      (on (message (tuple "ping" (bind x Int)))
                                          (send! (tuple "pong" x)))))))

;; local definitions
#;(module+ test
  ;; these cause an error in rackunit-typechecking, don't know why :/
  #;(check-type (let ()
                (begin
                  (define id : Int 1234)
                  id))
              : Int
              -> 1234)
  #;(check-type (let ()
                (define (spawn-cell [initial-value : Int])
                  (define id 1234)
                  id)
                (typed-app spawn-cell 42))
              : Int
              -> 1234)
  (check-equal? (let ()
                  (define id : Int 1234)
                  id)
                1234)
  #;(check-equal? (let ()
                  (define (spawn-cell [initial-value : Int])
                    (define id 1234)
                    id)
                  (typed-app spawn-cell 42))
                1234))