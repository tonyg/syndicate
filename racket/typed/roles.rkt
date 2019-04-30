#lang turnstile

(provide #%module-begin
         (rename-out [typed-app #%app])
         (rename-out [typed-quote quote])
         #%top-interaction
         require only-in
         ;; Start dataspace programs
         run-ground-dataspace
         ;; Types
         Int Bool String Tuple Bind Discard → ∀ ByteString Symbol
         Role Reacts Shares Know ¬Know Message OnDataflow Stop OnStart OnStop
         FacetName Field ★/t
         Observe Inbound Outbound Actor U
         Computation Value Endpoints Roles Spawns
         →fn
         ;; Statements
         let let* if spawn dataspace start-facet set! begin stop begin/dataflow #;unsafe-do
         when unless send! define
         ;; Derived Forms
         during define/query-value define/query-set
         ;; endpoints
         assert on field
         ;; expressions
         tuple select lambda ref observe inbound outbound
         Λ inst
         ;; making types
         define-type-alias
         define-constructor define-constructor*
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         (all-from-out "prim.rkt")
         ;; lists
         (all-from-out "list.rkt")
         ;; sets
         (all-from-out "set.rkt")
         ;; sequences
         (all-from-out "sequence.rkt")
         ;; hash tables
         (all-from-out "hash.rkt")
         ;; DEBUG and utilities
         print-type print-role
         ;; Extensions
         match cond
         ;; require & provides
         require provide
         submod for-syntax for-meta only-in except-in
         require/typed
         require-struct
         )
(require "core-types.rkt")
(require "list.rkt")
(require "set.rkt")
(require "prim.rkt")
(require "sequence.rkt")
(require "hash.rkt")

(require (prefix-in syndicate: syndicate/actor-lang))

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx syntax/parse racket/base))
(require (for-syntax turnstile/examples/util/filter-maximal))
(require (for-syntax racket/struct-info))
(require macrotypes/postfix-in)
(require (postfix-in - racket/list))
(require (postfix-in - racket/set))
(require (postfix-in - racket/match))

(module+ test
  (require rackunit)
  (require rackunit/turnstile))

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
     (⇒ ν-f (τ))])

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
     (⇒ ν-ep (MF))])

(define-typed-syntax (assert e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τs (type-eval #'(Shares τ))
  -------------------------------------
  [⊢ (syndicate:assert e-) (⇒ : ★/t)
                           (⇒ ν-ep (τs))])

(define-typed-syntax (send! e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τm (type-eval #'(Sends τ))
  --------------------------------------
  [⊢ (syndicate:send! e-) (⇒ : ★/t)
                          (⇒ ν-f (τm))])

(define-typed-syntax (stop facet-name:id cont ...) ≫
  [⊢ facet-name ≫ facet-name- (⇐ : FacetName)]
  [⊢ (begin #f cont ...) ≫ cont- (⇒ ν-ep (~effs)) (⇒ ν-s (~effs)) (⇒ ν-f (~effs τ-f ...))]
  #:with τ (mk-Stop- #`(facet-name- τ-f ...))
  ---------------------------------------------------------------------------------
  [⊢ (syndicate:stop-facet facet-name- cont-) (⇒ : ★/t)
                                              (⇒ ν-f (τ))])

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
   [⊢ (begin s ...) ≫ s- (⇒ ν-ep (~effs))
                          (⇒ ν-f (~effs τ-f ...))
                          (⇒ ν-s (~effs τ-s ...))]
   #:with τ-r (type-eval #'(Reacts OnStart τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on-start s-) (⇒ : ★/t)
      (⇒ ν-ep (τ-r))]]
  [(on (~literal stop) s ...) ≫
   [⊢ (begin s ...) ≫ s- (⇒ ν-ep (~effs))
                          (⇒ ν-f (~effs τ-f ...))
                          (⇒ ν-s (~effs τ-s ...))]
   #:with τ-r (type-eval #'(Reacts OnStop τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on-stop s-) (⇒ : ★/t)
      (⇒ ν-ep (τ-r))]]
  [(on (a/r/m:asserted/retracted/message p) s ...) ≫
   [⊢ p ≫ p-- (⇒ : τp)]
   #:fail-unless (pure? #'p--) "pattern not allowed to have effects"
   #:with ([x:id τ:type] ...) (pat-bindings #'p)
   [[x ≫ x- : τ] ... ⊢ (begin s ...) ≫ s-
                 (⇒ ν-ep (~effs))
                 (⇒ ν-f (~effs τ-f ...))
                 (⇒ ν-s (~effs τ-s ...))]
   #:with p- (substs #'(x- ...) #'(x ...) (compile-syndicate-pattern #'p))
   #:with τ-r (type-eval #'(Reacts (a/r/m.react-con τp) τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on (a/r/m.syndicate-kw p-)
                    s-)
      (⇒ : ★/t)
      (⇒ ν-ep (τ-r))]])

(define-typed-syntax (begin/dataflow s ...+) ≫
  [⊢ (begin s ...) ≫ s-
     (⇒ : _)
     (⇒ ν-ep (~effs))
     (⇒ ν-f (~effs τ-f ...))
     (⇒ ν-s (~effs τ-s ...))]
  #:with τ-r (type-eval #'(Reacts OnDataflow τ-f ... τ-s ...))
  --------------------------------------------------
  [⊢ (syndicate:begin/dataflow s-)
     (⇒ : ★/t)
     (⇒ ν-ep (τ-r))])

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
  [⊢ s ≫ s- (⇒ ν-ep (~effs)) (⇒ ν-s (~effs)) (⇒ ν-f (~effs τ-f ...))]
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
                                               (⇒ ν-s (τ-final))])

(define-typed-syntax (dataspace τ-c:type s ...) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ ν-ep (~effs)) (⇒ ν-s (~effs τ-s ...)) (⇒ ν-f (~effs))] ...
  #:with τ-actor (type-eval #'(Actor τ-c.norm))
  #:fail-unless (stx-andmap (lambda (t) (<: t #'τ-actor)) #'(τ-s ... ...))
                "Not all actors conform to communication type"
  #:with τ-ds-i (strip-inbound #'τ-c.norm)
  #:with τ-ds-o (strip-outbound #'τ-c.norm)
  #:with τ-relay (relay-interests #'τ-c.norm)
  -----------------------------------------------------------------------------------
  [⊢ (syndicate:dataspace s- ...) (⇒ : ★/t)
                                  (⇒ ν-s ((Actor (U τ-ds-i τ-ds-o τ-relay))))])

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
     (⇒ ν-ep (τ-ep ...))
     (⇒ ν-s (τ-s ...))
     (⇒ ν-f (τ-f ...))])

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
      (⇒ ν-ep (~effs eps1 ...)) (⇒ ν-f (~effs fs1 ...)) (⇒ ν-s (~effs ss1 ...))]
   [⊢ e2 ≫ e2- (⇐ : τ-expected)
      (⇒ ν-ep (~effs eps2 ...)) (⇒ ν-f (~effs fs2 ...)) (⇒ ν-s (~effs ss2 ...))]
   --------
   [⊢ (if- e_tst- e1- e2-)
      (⇒ ν-ep (eps1 ... eps2 ...))
      (⇒ ν-f #,(make-Branch #'((fs1 ...) (fs2 ...))))
      (⇒ ν-s (ss1 ... ss2 ...))]]
  [(_ e_tst e1 e2) ≫
   [⊢ e_tst ≫ e_tst- ⇒ _] ; Any non-false value is truthy.
   #:fail-unless (pure? #'e_tst-) "expression must be pure"
   [⊢ e1 ≫ e1- (⇒ : τ1)
      (⇒ ν-ep (~effs eps1 ...)) (⇒ ν-f (~effs fs1 ...)) (⇒ ν-s (~effs ss1 ...))]
   [⊢ e2 ≫ e2- (⇒ : τ2)
      (⇒ ν-ep (~effs eps2 ...)) (⇒ ν-f (~effs fs2 ...)) (⇒ ν-s (~effs ss2 ...))]
   #:with τ (type-eval #'(U τ1 τ2))
   --------
   [⊢ (if- e_tst- e1- e2-) (⇒ : τ)
      (⇒ ν-ep (eps1 ... eps2 ...))
      (⇒ ν-f #,(make-Branch #'((fs1 ...) (fs2 ...))))
      (⇒ ν-s (ss1 ... ss2 ...))]])

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
                   (⇒ ν-ep (~effs eps ...))
                   (⇒ ν-f (~effs fs ...))
                   (⇒ ν-s (~effs ss ...))]
   ----------------------------------------------------------
   [⊢ (let- ([x- e-] ...) e_body-)
      (⇒ ν-ep (eps ...))
      (⇒ ν-f (fs ...))
      (⇒ ν-s (ss ...))]]
  [(_ ([x e] ...) e_body ...) ≫
   [⊢ e ≫ e- ⇒ : τ_x] ...
   #:fail-unless (stx-andmap pure? #'(e- ...)) "expressions must be pure"
   [[x ≫ x- : τ_x] ... ⊢ (begin e_body ...) ≫ e_body- (⇒ : τ_body)
                   (⇒ ν-ep (~effs eps ...))
                   (⇒ ν-f (~effs fs ...))
                   (⇒ ν-s (~effs ss ...))]
   ----------------------------------------------------------
   [⊢ (let- ([x- e-] ...) e_body-) (⇒ : τ_body)
      (⇒ ν-ep (eps ...))
      (⇒ ν-f (fs ...))
      (⇒ ν-s (ss ...))]])

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
                   (⇒ ν-ep (~effs eps ...))
                   (⇒ ν-f (~effs fs ...))
                   (⇒ ν-s (~effs ss ...))] ...
  ------------------------------------------------
  [⊢ (cond- [pred- s-] ...) (⇒ : (U τ-s ...))
     (⇒ ν-ep (eps ... ...))
     (⇒ ν-f #,(make-Branch #'((fs ...) ...)))
     (⇒ ν-s (ss ... ...))])

(define-typed-syntax (match e [p s ...+] ...+) ≫
  [⊢ e ≫ e- (⇒ : τ-e)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  #:with (([x τ] ...) ...) (stx-map pat-bindings #'(p ...))
  [[x ≫ x- : τ] ... ⊢ (begin s ...) ≫ s- (⇒ : τ-s)
                (⇒ ν-ep (~effs eps ...))
                (⇒ ν-f (~effs fs ...))
                (⇒ ν-s (~effs ss ...))] ...
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
     (⇒ ν-ep (eps ... ...))
     (⇒ ν-f #,(make-Branch #'((fs ...) ...)))
     (⇒ ν-s (ss ... ...))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Dataspace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; n.b. this is a blocking operation, so an actor that uses this internally
;; won't necessarily terminate.
(define-typed-syntax (run-ground-dataspace τ-c:type s ...) ≫
  [⊢ (dataspace τ-c s ...) ≫ ((~literal erased) ((~literal syndicate:dataspace) s- ...)) (⇒ : t)]
  -----------------------------------------------------------------------------------
  [⊢ (syndicate:run-ground s- ...) (⇒ : (AssertionSet τ-c))])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (print-type e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ν-ep (~effs eps ...)) (⇒ ν-f (~effs fs ...)) (⇒ ν-s (~effs ss ...))]
  #:do [(pretty-display (type->str #'τ))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ ν-ep (eps ...)) (⇒ ν-f (fs ...)) (⇒ ν-s (ss ...))])

(define-typed-syntax (print-role e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ν-ep (~effs eps ...)) (⇒ ν-f (~effs fs ...)) (⇒ ν-s (~effs ss ...))]
  #:do [(for ([r (in-syntax #'(fs ...))])
          (pretty-display (type->str r)))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ ν-ep (eps ...)) (⇒ ν-f (fs ...)) (⇒ ν-s (ss ...))])

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
