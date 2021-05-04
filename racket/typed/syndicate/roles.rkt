#lang turnstile

(provide #%module-begin
         #%app
         (rename-out [typed-quote quote])
         #%top-interaction
         module+ module*
         ;; require & provides
         require only-in prefix-in except-in rename-in
         provide all-defined-out all-from-out rename-out except-out
         for-syntax for-template for-label for-meta struct-out
         ;; Start dataspace programs
         run-ground-dataspace
         ;; Types
         Tuple Bind Discard → ∀ AssertionSet
         Role Reacts Shares Asserted Retracted Message OnDataflow Stop OnStart OnStop
         Know Forget Realize
         Branch Effs
         FacetName Field ★/t
         Observe Inbound Outbound Actor U ⊥
         Computation Value Endpoints Roles Spawns Sends
         →fn proc
         ;; Statements
         let let* if spawn dataspace start-facet set! begin block stop begin/dataflow #;unsafe-do
         when unless send! realize! define during/spawn
         ;; Derived Forms
         during During
         define/query-value
         define/query-set
         define/query-hash
         define/dataflow
         on-start on-stop
         ;; endpoints
         assert know on field
         ;; expressions
         tuple select lambda λ ref (struct-out observe) (struct-out message) (struct-out inbound) (struct-out outbound)
         Λ inst call/inst
         ;; making types
         define-type-alias
         assertion-struct
         message-struct
         define-constructor define-constructor*
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         (all-from-out "prim.rkt")
         ;; expressions
         (except-out (all-from-out "core-expressions.rkt") mk-tuple tuple-select)
         ;; lists
         (all-from-out "list.rkt")
         ;; sets
         (all-from-out "set.rkt")
         ;; sequences
         (all-from-out "sequence.rkt")
         ;; hash tables
         (all-from-out "hash.rkt")
         ;; for loops
         (all-from-out "for-loops.rkt")
         ;; utility datatypes
         (all-from-out "maybe.rkt")
         (all-from-out "either.rkt")
         ;; DEBUG and utilities
         print-type print-role role-strings
         ;; Behavioral Roles
         export-roles export-type check-simulates check-has-simulating-subgraph lift+define-role
         verify-actors
         ;; LTL Syntax
         True False Always Eventually Until WeakUntil Implies And Or Not A M
         ;; Extensions
         match cond
         submod for-syntax for-meta only-in except-in
         require/typed
         require-struct
         )
(require "core-types.rkt")
(require "core-expressions.rkt")
(require "list.rkt")
(require "set.rkt")
(require "prim.rkt")
(require "sequence.rkt")
(require "hash.rkt")
(require "for-loops.rkt")
(require "maybe.rkt")
(require "either.rkt")

(require (prefix-in syndicate: syndicate/actor-lang))
(require (submod syndicate/actor priorities))
(require (prefix-in syndicate: (submod syndicate/actor for-module-begin)))

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx syntax/parse racket/base))
(require macrotypes/postfix-in)
(require (for-syntax turnstile/mode))
(require turnstile/typedefs)
(require (postfix-in - racket/list))
(require (postfix-in - racket/set))

(require (for-syntax (prefix-in proto: "proto.rkt")
                     (prefix-in proto: "ltl.rkt")
                     syntax/id-table)
         (prefix-in proto: "proto.rkt")
         (prefix-in proto: "compile-spin.rkt"))

(module+ test
  (require rackunit)
  (require rackunit/turnstile))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating Communication Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (assertion-struct name:id (~datum :) Name:id (slot:id ...))
  (define-constructor* (name : Name slot ...)))

(define-simple-macro (message-struct name:id (~datum :) Name:id (slot:id ...))
  (define-constructor* (name : Name slot ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile-time State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define current-communication-type (make-parameter #f))
  ;; Type -> Mode
  (define (communication-type-mode ty)
    (make-param-mode current-communication-type ty))

  (define (elaborate-pattern/with-com-ty pat)
    (define τ? (current-communication-type))
    (if τ?
        (elaborate-pattern/with-type pat τ?)
        (elaborate-pattern pat))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax start-facet
  [(_ name:id #:implements ~! spec:type ep ...+) ≫
   [⊢ (start-facet name ep ...) ≫ e- (⇒ ν-f (~effs impl-ty))]
   #:fail-unless (simulating-types? #'impl-ty #'spec.norm)
                 "facet does not implement specification"
   ------------------------------------------------------------
   [≻ e-]]
  [(_ name:id #:includes-behavior ~! spec:type ep ...+) ≫
   [⊢ (start-facet name ep ...) ≫ e- (⇒ ν-f (~effs impl-ty))]
   #:fail-unless (type-has-simulating-subgraphs? #'impl-ty #'spec.norm)
                 "no subset implements specified behavior"
   ------------------------------------------------------------
   [≻ e-]]
  [(_ name:id ep ...+) ≫
  #:with name- (syntax-local-identifier-as-binding (syntax-local-introduce (generate-temporary #'name)))
  #:with name+ (syntax-local-identifier-as-binding #'name)
  #:with facet-name-ty (type-eval #'FacetName)
  #:do [(define ctx (syntax-local-make-definition-context))
        (define unique (gensym 'start-facet))
        (define name-- (add-orig (internal-definition-context-introduce ctx #'name- 'add)
                                 #'name))
        (int-def-ctx-bind-type-rename #'name+ #'name- #'facet-name-ty ctx)
        (define-values (ep-... τ... ep-effects facet-effects spawn-effects)
          (walk/bind #'(ep ...) ctx unique))
        (unless (and (stx-null? facet-effects) (stx-null? spawn-effects))
          (type-error #:src #'(ep ...) #:msg "only endpoint effects allowed"))]
  #:with ((~or (~and τ-a (~Shares _))
               (~and τ-k (~Know _))
               ;; untyped syndicate might allow this - TODO
               #;(~and τ-m (~Sends _))
               (~and τ-r (~Reacts _ _ ...))
               ~MakesField)
          ...)
         ep-effects
  #:with τ (type-eval #`(Role (#,name--)
                          τ-a ...
                          τ-k ...
                          ;; τ-m ...
                          τ-r ...))
  --------------------------------------------------------------
  [⊢ (syndicate:react (let- ([#,name-- (#%app- syndicate:current-facet-id)])
                        #,@ep-...))
     (⇒ : ★/t)
     (⇒ ν-f (τ))]])

(define-typed-syntax (during/spawn pat bdy ...+) ≫
  #:with pat+ (elaborate-pattern/with-com-ty #'pat)
  [⊢ pat+ ≫ pat-- (⇒ : τp)]
  #:fail-unless (pure? #'pat--) "pattern not allowed to have effects"
  #:fail-unless (allowed-interest? (pattern-sub-type #'τp)) "overly broad interest, ?̱̱★ and ??★ not allowed"
  #:with ([x:id τ:type] ...) (pat-bindings #'pat+)
  [[x ≫ x- : τ] ... ⊢ (block bdy ...) ≫ bdy-
                (⇒ ν-ep (~effs τ-ep ...))
                (⇒ ν-f (~effs))
                (⇒ ν-s (~effs))]
  #:with pat- (substs #'(x- ...) #'(x ...) (compile-syndicate-pattern #'pat+))
  #:with τc:type (current-communication-type)
  #:with τ-facet (type-eval #'(Role (_) τ-ep ...))
  #:with τ-spawn (mk-ActorWithRole- #'(τc.norm τ-facet))
  #:with τ-endpoint (type-eval #'(Reacts (Asserted τp) τ-spawn))
  ------------------------------
  [⊢ (syndicate:during/spawn pat- bdy-)
     (⇒ : ★/t)
     (⇒ ν-ep (τ-endpoint))])

(define-typed-syntax field
  [(_ [x:id (~optional (~datum :)) τ-f:type e:expr] ...) ≫
   #:cut
   #:fail-unless (stx-andmap flat-type? #'(τ-f ...)) "keep your uppity data outta my fields"
   [⊢ e ≫ e- (⇐ : τ-f)] ...
   #:fail-unless (stx-andmap pure? #'(e- ...)) "field initializers not allowed to have effects"
   #:with (x- ...) (generate-temporaries #'(x ...))
   #:with (τ ...) (stx-map type-eval #'((Field τ-f.norm) ...))
   #:with MF (type-eval #'MakesField)
   ----------------------------------------------------------------------
   [⊢ (erased (field/intermediate [x x- τ e-] ...))
      (⇒ : ★/t)
      (⇒ ν-ep (MF))]]
  [(_ flds ... [x:id e:expr] more-flds ...) ≫
   #:cut
   [⊢ e ≫ e- (⇒ : τ)]
   --------------------
   [≻ (field flds ... [x τ e-] more-flds ...)]])

(define-typed-syntax (assert e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:fail-unless (allowed-interest? #'τ) "overly broad interest, ?̱̱★ and ??★ not allowed"
  #:with τs (mk-Shares- #'(τ))
  -------------------------------------
  [⊢ (syndicate:assert e-) (⇒ : ★/t)
                           (⇒ ν-ep (τs))])

(define-typed-syntax (know e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τs (mk-Know- #'(τ))
  -------------------------------------
  [⊢ (syndicate:know e-) (⇒ : ★/t)
     (⇒ ν-ep (τs))])

(define-typed-syntax (send! e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τm (mk-Sends- #'(τ))
  --------------------------------------
  [⊢ (#%app- syndicate:send! e-) (⇒ : ★/t)
                                 (⇒ ν-f (τm))])

(define-typed-syntax (realize! e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τm (mk-Realizes- #'(τ))
  --------------------------------------
  [⊢ (#%app- syndicate:realize! e-) (⇒ : ★/t)
     (⇒ ν-f (τm))])

(define-typed-syntax (stop facet-name:id cont ...) ≫
  [⊢ facet-name ≫ facet-name- (⇐ : FacetName)]
  [⊢ (block #f cont ...) ≫ cont-
                         (⇒ ν-ep (~effs))
                         (⇒ ν-s (~effs))
                         (⇒ ν-f (~effs τ-f ...))]
  #:with τ #'(Stop facet-name- τ-f ...)
  ---------------------------------------------------------------------------------
  [⊢ (syndicate:stop-facet facet-name- cont-) (⇒ : ★/t)
                                              (⇒ ν-f (τ))])

(begin-for-syntax
  (define-syntax-class event-cons
    #:attributes (syndicate-kw ty-cons)
    #:datum-literals (asserted retracted message know forget realize)
    (pattern (~or (~and asserted
                        (~bind [syndicate-kw #'syndicate:asserted]
                               [ty-cons #'Asserted]))
                  (~and retracted
                        (~bind [syndicate-kw #'syndicate:retracted]
                               [ty-cons #'Retracted]))
                  (~and message
                        (~bind [syndicate-kw #'syndicate:message]
                               [ty-cons #'Message]))
                  (~and know
                        (~bind [syndicate-kw #'syndicate:know]
                               [ty-cons #'Know]))
                  (~and forget
                        (~bind [syndicate-kw #'syndicate:forget]
                               [ty-cons #'Forget]))
                  (~and realize
                        (~bind [syndicate-kw #'syndicate:realize]
                               [ty-cons #'Realize])))))
  (define-syntax-class priority-level
    #:literals (*query-priority-high*
                *query-priority*
                *query-handler-priority*
                *normal-priority*
                *gc-priority*
                *idle-priority*)
    (pattern (~and level
                   (~or *query-priority-high*
                        *query-priority*
                        *query-handler-priority*
                        *normal-priority*
                        *gc-priority*
                        *idle-priority*))))
  (define-splicing-syntax-class priority
    #:attributes (level)
    (pattern (~seq #:priority l:priority-level)
             #:attr level #'l.level)
    (pattern (~seq)
             #:attr level #'*normal-priority*))
  )

(define-typed-syntax on
  #:datum-literals (start stop)
  [(on start s ...) ≫
   [⊢ (block s ...) ≫ s- (⇒ ν-ep (~effs))
                          (⇒ ν-f (~effs τ-f ...))
                          (⇒ ν-s (~effs τ-s ...))]
   #:with τ-r (type-eval #'(Reacts OnStart τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on-start s-) (⇒ : ★/t)
      (⇒ ν-ep (τ-r))]]
  [(on stop s ...) ≫
   [⊢ (block s ...) ≫ s- (⇒ ν-ep (~effs))
                          (⇒ ν-f (~effs τ-f ...))
                          (⇒ ν-s (~effs τ-s ...))]
   #:with τ-r (type-eval #'(Reacts OnStop τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on-stop s-) (⇒ : ★/t)
      (⇒ ν-ep (τ-r))]]
  [(on (evt:event-cons p)
       priority:priority
       s ...) ≫
   #:do [(define msg? (free-identifier=? #'syndicate:message (attribute evt.syndicate-kw)))
         (define elab
           (elaborate-pattern/with-com-ty (if msg? #'(message p) #'p)))]
   #:with p/e (if msg? (stx-cadr elab) elab)
   [⊢ p/e ≫ p-- (⇒ : τp)]
   #:fail-unless (pure? #'p--) "pattern not allowed to have effects"
   #:fail-unless (allowed-interest? (pattern-sub-type #'τp)) "overly broad interest, ?̱̱★ and ??★ not allowed"
   #:with ([x:id τ:type] ...) (pat-bindings #'p/e)
   [[x ≫ x- : τ] ... ⊢ (block s ...) ≫ s-
                 (⇒ ν-ep (~effs))
                 (⇒ ν-f (~effs τ-f ...))
                 (⇒ ν-s (~effs τ-s ...))]
   #:with p- (substs #'(x- ...) #'(x ...) (compile-syndicate-pattern #'p/e))
   #:with τ-r (type-eval #'(Reacts (evt.ty-cons τp) τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on (evt.syndicate-kw p-)
                    #:priority priority.level
                    s-)
      (⇒ : ★/t)
      (⇒ ν-ep (τ-r))]])

(define-typed-syntax (begin/dataflow s ...+) ≫
  [⊢ (block s ...) ≫ s-
     (⇒ : _)
     (⇒ ν-ep (~effs))
     (⇒ ν-f (~effs τ-f ...))
     (⇒ ν-s (~effs τ-s ...))]
  #:with τ-r (type-eval #'(Reacts OnDataflow τ-f ... τ-s ...))
  --------------------------------------------------
  [⊢ (syndicate:begin/dataflow s-)
     (⇒ : ★/t)
     (⇒ ν-ep (τ-r))])

(define-for-syntax (compile-syndicate-pattern pat)
  (compile-pattern pat
                   #'list-
                   (lambda (id) #`($ #,id))
                   identity))

(define-typed-syntax spawn
  ;; TODO - do the lack of #:cut-s cause bad error messages here?
  [(spawn τ-c:type s) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  ;; TODO: check that each τ-f is a Role
  #:mode (communication-type-mode #'τ-c.norm)
    [
     [⊢ (block s) ≫ s- (⇒ ν-ep (~effs)) (⇒ ν-s (~effs)) (⇒ ν-f (~effs τ-f ...))]
    ]
  ;; TODO: s shouldn't refer to facets or fields!
  #:fail-unless (and (stx-andmap Role? #'(τ-f ...))
                     (= 1 (length (syntax->list #'(τ-f ...)))))
                 "expected exactly one Role for body"
  #:with (τ-i τ-o τ-i/i τ-o/i τ-a) (analyze-roles #'(τ-f ...))
  #:fail-unless (<: #'τ-o #'τ-c.norm)
                (format "Output ~a not valid in dataspace ~a" (type->str #'τ-o) (type->str #'τ-c.norm))
  #:with τ-final #;(mk-Actor- #'(τ-c.norm)) (mk-ActorWithRole- #'(τ-c.norm τ-f ...))
  #:fail-unless (<: #'τ-a #'τ-final)
                "Spawned actors not valid in dataspace"
  #:fail-unless (project-safe? (∩ (strip-? #'τ-o) #'τ-c.norm)
                               #'τ-i)
                (string-append "Not prepared to handle inputs:\n" (make-actor-error-message #'τ-i #'τ-o #'τ-c.norm))
  #:fail-unless (project-safe? (∩ (strip-? #'τ-o/i) #'τ-o/i) #'τ-i/i)
                (string-append "Not prepared to handle internal events:\n" (make-actor-error-message #'τ-i/i #'τ-o/i #'τ-o/i))
  --------------------------------------------------------------------------------------------
  [⊢ (syndicate:spawn (syndicate:on-start s-)) (⇒ : ★/t)
                                               (⇒ ν-s (τ-final))]]
  [(spawn s) ≫
   #:do [(define τc (current-communication-type))]
   #:when τc
   ----------------------------------------
   [≻ (spawn #,τc s)]])

;; Type Type Type -> String
(define-for-syntax (make-actor-error-message τ-i τ-o τ-c)
  (define mismatches (find-surprising-inputs τ-i τ-o τ-c))
  (string-join (map type->str mismatches) ",\n"))

;; Type Type Type -> (Listof Type)
(define-for-syntax (find-surprising-inputs τ-i τ-o τ-c)
  (define incoming (∩ (strip-? τ-o) τ-c))
  ;; Type -> (Listof Type)
  (let loop ([ty incoming])
    (syntax-parse ty
      [(~U* τ ...)
       (apply append (map loop (syntax->list #'(τ ...))))]
      [_
       (cond
         [(project-safe? ty τ-i)
          '()]
         [else
          (list ty)])])))

(define-typed-syntax (dataspace τ-c:type s ...) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  #:mode (communication-type-mode #'τ-c.norm)
    [
     [⊢ s ≫ s- (⇒ ν-ep (~effs)) (⇒ ν-s (~effs τ-s ...)) (⇒ ν-f (~effs))] ...
    ]
  #:with τ-actor (mk-Actor- #'(τ-c.norm))
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
  [⊢ (#%app- x- e-) (⇒ : ★/t)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derived Forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax during
  #:literals (know)
  [(_ (~or (~and k (know p)) p) s ...) ≫
  #:with p+ (elaborate-pattern/with-com-ty #'p)
  #:with inst-p (instantiate-pattern #'p+)
  #:with start-e (if (attribute k) #'know #'asserted)
  #:with stop-e (if (attribute k) #'forget #'retracted)
  ----------------------------------------
  [≻ (on (start-e p+)
         (start-facet during-inner
           (on (stop-e inst-p)
               (stop during-inner))
           s ...))]])

(define-simple-macro (During (~or (~and K ((~literal Know) τ:type)) τ:type)
                       EP ...)
  #:with τ/inst (instantiate-pattern-type #'τ.norm)
  #:with start-e (if (attribute K) #'Know #'Asserted)
  #:with stop-e (if (attribute K) #'Forget #'Retracted)
  (Reacts (start-e τ)
          (Role (during-inner)
                (Reacts (stop-e τ/inst)
                        (Stop during-inner))
                EP ...)))

;; TODO - reconcile this with `compile-pattern`
(define-for-syntax (instantiate-pattern pat)
  (let loop ([pat pat])
    (syntax-parse pat
      #:datum-literals (tuple discard bind)
      [(tuple p ...)
       #`(tuple #,@(stx-map loop #'(p ...)))]
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

;; Type -> Type
;; replace occurrences of (Bind τ) with τ in a type, in much the same way
;; instantiate-pattern does for patterns
;; TODO - this is almost exactly the same as replace-bind-and-discard-with-★
(define-for-syntax (instantiate-pattern-type ty)
  (syntax-parse ty
    [(~Bind τ)
     #'τ]
    [(~U* τ ...)
     (mk-U- (stx-map instantiate-pattern-type #'(τ ...)))]
    [(~Any/new τ-cons τ ...)
     #:when (reassemblable? #'τ-cons)
     (define subitems (for/list ([t (in-syntax #'(τ ...))])
                        (instantiate-pattern-type t)))
     (reassemble-type #'τ-cons subitems)]
    [_ ty]))

(begin-for-syntax
  (define-splicing-syntax-class on-add
    #:attributes (expr)
    (pattern (~seq #:on-add add-e)
             #:attr expr #'add-e)
    (pattern (~seq)
             #:attr expr #'#f))

  (define-splicing-syntax-class on-remove
    #:attributes (expr)
    (pattern (~seq #:on-remove remove-e)
             #:attr expr #'remove-e)
    (pattern (~seq)
             #:attr expr #'#f)))


(define-typed-syntax (define/query-value x:id e0 p e
                       (~optional add:on-add)
                       (~optional remove:on-remove)) ≫
  [⊢ e0 ≫ e0- (⇒ : τ)]
  #:fail-unless (pure? #'e0-) "expression must be pure"
  ----------------------------------------
  [≻ (begin (field [x τ e0-])
            (on (asserted p)
                #:priority *query-priority*
                (set! x e)
                add.expr)
            (on (retracted p)
                #:priority *query-priority-high*
                (set! x e0-)
                remove.expr))])

(define-typed-syntax (define/query-set x:id p e
                       (~optional add:on-add)
                       (~optional remove:on-remove)) ≫
  #:with p+ (elaborate-pattern/with-com-ty #'p)
  #:with ([y τ] ...) (pat-bindings #'p+)
  ;; e will be re-expanded :/
  [[y ≫ y- : τ] ... ⊢ e ≫ e- ⇒ τ-e]
  ----------------------------------------
  [≻ (begin (field [x (Set τ-e) (set)])
            (on (asserted p+)
                #:priority *query-priority*
                (set! x (set-add (ref x) e))
                add.expr)
            (on (retracted p+)
                #:priority *query-priority-high*
                (set! x (set-remove (ref x) e))
                remove.expr))])

(define-typed-syntax (define/query-hash x:id p e-key e-value
                       (~optional add:on-add)
                       (~optional remove:on-remove)) ≫
  #:with p+ (elaborate-pattern/with-com-ty #'p)
  #:with ([y τ] ...) (pat-bindings #'p+)
  ;; e-key and e-value will be re-expanded :/
  ;; but it's the most straightforward way to keep bindings in sync with
  ;; pattern
  [[y ≫ y- : τ] ... ⊢ e-key ≫ e-key- ⇒ τ-key]
  [[y ≫ y-- : τ] ... ⊢ e-value ≫ e-value- ⇒ τ-value]
  ;; TODO - this is gross, is there a better way to do this?
  ;; #:with e-value-- (substs #'(y- ...) #'(y-- ...) #'e-value- free-identifier=?)
  ;; I thought I could put e-key- and e-value-(-) in the output below, but that
  ;; gets their references to pattern variables out of sync with `p`
  ----------------------------------------
  [≻ (begin (field [x (Hash τ-key τ-value) (hash)])
            (on (asserted p+)
                #:priority *query-priority*
                (set! x (hash-set (ref x) e-key e-value))
                add.expr)
            (on (retracted p+)
                #:priority *query-priority-high*
                (set! x (hash-remove (ref x) e-key))
                remove.expr))])

(define-simple-macro (on-start e ...)
  (on start e ...))

(define-simple-macro (on-stop e ...)
  (on stop e ...))

(define-typed-syntax define/dataflow
  [(define/dataflow x:id τ:type e) ≫
  [⊢ e ≫ e- (⇐ : τ)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  ;; because the begin/dataflow body is scheduled to run at some later point,
  ;; the initial value is visible e.g. immediately after the define/dataflow
;; #:with place-holder (attach #'(#%datum- #f) ': #'τ.norm)
  ----------------------------------------
  [≻ (begin (field [x τ e-])
            (begin/dataflow (set! x e-)))]]
  [(define/dataflow x:id e) ≫
   [⊢ e ≫ e- (⇒ : τ)]
   #:fail-unless (pure? #'e-) "expression must be pure"
   ----------------------------------------
   [≻ (define/dataflow x τ e-)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (ref x:id) ≫
  [⊢ x ≫ x- ⇒ (~Field τ)]
  ------------------------
  [⊢ (#%app- x-) (⇒ : τ)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Dataspace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; n.b. this is a blocking operation, so an actor that uses this internally
;; won't necessarily terminate.
(define-typed-syntax (run-ground-dataspace τ-c:type s ...) ≫
  ;;#:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  #:mode (communication-type-mode #'τ-c.norm)
  [
   [⊢ s ≫ s- (⇒ : t1)] ...
   [⊢ (dataspace τ-c.norm s- ...) ≫ _ (⇒ : t2)]
  ]
  #:with τ-out (strip-outbound #'τ-c.norm)
  -----------------------------------------------------------------------------------
  [⊢ (#%app- syndicate:run-ground (#%app- syndicate:capture-actor-actions (lambda- () (#%app- list- s- ...))))
     (⇒ : (AssertionSet τ-out))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typed-syntax (print-type e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ν-ep (~effs eps ...)) (⇒ ν-f (~effs fs ...)) (⇒ ν-s (~effs ss ...))]
  #:do [(pretty-display (type->strX #'τ))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ ν-ep (eps ...)) (⇒ ν-f (fs ...)) (⇒ ν-s (ss ...))])

(define-typed-syntax (print-role e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ν-ep (~effs eps ...)) (⇒ ν-f (~effs fs ...)) (⇒ ν-s (~effs ss ...))]
  #:do [(for ([r (in-syntax #'(fs ...))])
          (pretty-display (type->strX r)))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ ν-ep (eps ...)) (⇒ ν-f (fs ...)) (⇒ ν-s (ss ...))])

;; this is mainly for testing
(define-typed-syntax (role-strings e) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ν-f (~effs fs ...))]
  #:with (s ...) (for/list ([r (in-syntax #'(fs ...))])
                   (type->strX r))
  ----------------------------------------
  [⊢ (#%app- list- (#%datum- . s) ...) (⇒ : (List String))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LTL Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type LTL : LTL)

(define-type True : LTL)
(define-type False : LTL)
(define-type Always : LTL -> LTL)
(define-type Eventually : LTL -> LTL)
(define-type Until : LTL LTL -> LTL)
(define-type WeakUntil : LTL LTL -> LTL)
(define-type Implies : LTL LTL -> LTL)
(define-type And : LTL * -> LTL)
(define-type Or : LTL * -> LTL)
(define-type Not : LTL -> LTL)
(define-type A : Type -> LTL) ;; Assertions
(define-type M : Type -> LTL) ;; Messages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavioral Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax

  (define ID-PHASE 0)

  (define-syntax (build-id-table stx)
    (syntax-parse stx
      [(_ (~seq key val) ...)
       #'(make-free-id-table (hash (~@ #'key val) ...) #:phase ID-PHASE)]))

  (define (mk-proto:U . args)
    (proto:U args))
  (define (mk-proto:Branch . args)
    (proto:Branch args))

  (define TRANSLATION#
    (build-id-table Spawns proto:Spawn
                    Sends proto:Sends
                    Realizes proto:Realizes
                    Shares proto:Shares
                    Know proto:Know
                    Branch mk-proto:Branch
                    Effs list
                    Asserted proto:Asserted
                    Retracted proto:Retracted
                    Message proto:Message
                    Forget proto:Forget
                    Realize proto:Realize
                    U* mk-proto:U
                    Observe proto:Observe
                    List proto:List
                    Set proto:Set
                    Hash proto:Hash
                    OnStart proto:StartEvt
                    OnStop proto:StopEvt
                    OnDataflow proto:DataflowEvt
                    ;; LTL
                    True #t
                    False #f
                    Always proto:always
                    Eventually proto:eventually
                    Until proto:strong-until
                    WeakUntil proto:weak-until
                    Implies proto:ltl-implies
                    And proto:&&
                    Or proto:||
                    Not proto:ltl-not
                    A proto:atomic
                    M (compose proto:atomic proto:Message)))

  (define (double-check)
    (for/first ([id (in-dict-keys TRANSLATION#)]
                #:when (false? (identifier-binding id)))
      (pretty-print id)
      (pretty-print (syntax-debug-info id))))

  (define (synd->proto ty)
    (let convert ([ty (resugar-type ty)])
      (syntax-parse ty
        #:literals (★/t Bind Discard ∀/internal →/internal Role/internal Stop Reacts Actor ActorWithRole)
        [(ctor:id t ...)
         #:when (dict-has-key? TRANSLATION# #'ctor)
         (apply (dict-ref TRANSLATION# #'ctor) (stx-map convert #'(t ...)))]
        [nm:id
         #:when (dict-has-key? TRANSLATION# #'nm)
         (dict-ref TRANSLATION# #'nm)]
        [(Actor _)
         (error "only able to convert actors with roles")]
        [(ActorWithRole _ r)
         (proto:Spawn (convert #'r))]
        [★/t proto:⋆]
        [(Bind t)
         ;; TODO - this is debatable handling
         (convert #'t)]
        [Discard
         ;; TODO - should prob have a Discard type in proto
         proto:⋆]
        [(∀/internal (X ...) body)
         ;; TODO
         (error "unimplemented")]
        [(→/internal ty-in ... ty-out)
         ;; TODO
         (error "unimplemented")]
        [(Role/internal (nm) body ...)
         (proto:Role (syntax-e #'nm) (stx-map convert #'(body ...)))]
        [(Stop nm body ...)
         (proto:Stop (syntax-e #'nm) (stx-map convert #'(body ...)))]
        [(Reacts evt body ...)
         (define converted-body (stx-map convert #'(body ...)))
         (define body+
           (if (= 1 (length converted-body))
               (first converted-body)
               converted-body))
         (proto:Reacts (convert #'evt) body+)]
        [t:id
         (proto:Base (syntax-e #'t))]
        [(ctor:id args ...)
         ;; assume it's a struct
         (proto:Struct (syntax-e #'ctor) (stx-map convert #'(args ...)))]
        [unrecognized (error (format "unrecognized type: ~a" #'unrecognized))]))))

(define-typed-syntax (export-roles dest:string e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ν-ep (~effs eps ...)) (⇒ ν-f (~effs fs ...)) (⇒ ν-s (~effs ss ...))]
  #:do [(with-output-to-file (syntax-e #'dest)
          (thunk (for ([f (in-syntax #'(fs ...))])
                   (pretty-write (synd->proto f))))
          #:exists 'replace)]
  ----------------------------------------
  [⊢ e- (⇒ : τ) (⇒ ν-ep (eps ...)) (⇒ ν-f (fs ...)) (⇒ ν-s (ss ...))])

(define-typed-syntax (export-type dest:string τ:type) ≫
  #:do [(with-output-to-file (syntax-e #'dest)
          (thunk (pretty-write (synd->proto #'τ.norm)))
          #:exists 'replace)]
  ----------------------------------------
  [⊢ (#%app- void-) (⇒ : ★/t)])

(define-typed-syntax (lift+define-role x:id e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ) (⇒ ν-ep (~effs)) (⇒ ν-f ((~and r  (~Role (_) _ ...)))) (⇒ ν-s (~effs))]
  ;; because turnstile introduces a lot of intdef scopes; ideally, we'd be able to synthesize somethign
  ;; with the right module scopes
  #:with x+ (syntax-local-introduce (datum->syntax #f (syntax-e #'x)))
  #:do [(define r- (synd->proto #'r))
        (syntax-local-lift-module-end-declaration #`(define- x+ '#,r-))]
  ----------------------------------------
  [⊢ e- (⇒ : τ) (⇒ ν-ep ()) (⇒ ν-f (r)) (⇒ ν-s ())])


;; Type Type -> Bool
;; (normalized Types)
(define-for-syntax (simulating-types? ty-impl ty-spec)
  (define ty-impl- (synd->proto ty-impl))
  (define ty-spec- (synd->proto ty-spec))
  (proto:simulates?/report-error ty-impl- ty-spec-))

;; Type Type -> Bool
;; (normalized Types)
(define-for-syntax (type-has-simulating-subgraphs? ty-impl ty-spec)
  (define ty-impl- (synd->proto ty-impl))
  (define ty-spec- (synd->proto ty-spec))
  (define ans (proto:find-simulating-subgraph/report-error ty-impl- ty-spec-))
  (unless ans
    (pretty-print ty-impl-)
    (pretty-print ty-spec-))
  ans)

(define- (ensure-Role! r)
  (unless- (#%app- proto:Role? r)
    (#%app- error- 'check-simulates "expected a Role type, got " r))
  r)

(begin-for-syntax
  (define-syntax-class type-or-proto
    #:attributes (role)
    (pattern t:type #:attr role #`(quote- #,(synd->proto #'t.norm)))
    (pattern x:id #:attr role #'(#%app- ensure-Role! x))
    #;(pattern ((~literal quote-) r)
             #:do [(define r- (syntax-e ))]
             #:when (proto:Role? r-)
             #:attr role r-)))

(require rackunit)

(define-syntax-parser check-simulates
  [(_ τ-impl:type-or-proto τ-spec:type-or-proto)
   (syntax/loc this-syntax
     (check-true (#%app- proto:simulates?/report-error τ-impl.role τ-spec.role)))])

(define-syntax-parser check-has-simulating-subgraph
  [(_ τ-impl:type-or-proto τ-spec:type-or-proto)
   (syntax/loc this-syntax
     (check-not-false (#%app- proto:find-simulating-subgraph/report-error τ-impl.role τ-spec.role)))])

(define-syntax-parser verify-actors
  [(_ spec actor-ty:type-or-proto ...)
   #:with spec- #`(quote- #,(synd->proto (type-eval #'spec)))
   (syntax/loc this-syntax
     (check-true (#%app- proto:compile+verify spec- (#%app- list- actor-ty.role ...))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (check-type
    (spawn (U (Observe (Tuple Int ★/t)))
           (start-facet echo
                        (on (message (tuple 1 $x:Int))
                            #f)))
    : ★/t)
  (check-type (spawn (U (Message (Tuple String Int))
                        (Observe (Tuple String ★/t)))
                     (start-facet echo
                                  (on (message (tuple "ping" $x))
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
