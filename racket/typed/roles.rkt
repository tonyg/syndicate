#lang turnstile

(provide #%module-begin
         #%app
         (rename-out [typed-quote quote])
         #%top-interaction
         require only-in
         ;; Start dataspace programs
         run-ground-dataspace
         ;; Types
         Tuple Bind Discard → ∀
         Role Reacts Shares Know ¬Know Message OnDataflow Stop OnStart OnStop
         FacetName Field ★/t
         Observe Inbound Outbound Actor U ⊥
         Computation Value Endpoints Roles Spawns
         →fn proc
         ;; Statements
         let let* if spawn dataspace start-facet set! begin stop begin/dataflow #;unsafe-do
         when unless send! define
         ;; Derived Forms
         during
         define/query-value
         define/query-set
         define/query-hash
         ;; endpoints
         assert on field
         ;; expressions
         tuple select lambda ref observe inbound outbound
         Λ inst call/inst
         ;; making types
         define-type-alias
         assertion-struct
         define-constructor define-constructor*
         ;; values
         #%datum
         ;; patterns
         bind discard
         ;; primitives
         (all-from-out "prim.rkt")
         ;; expressions
         (all-from-out "core-expressions.rkt")
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

(require (for-meta 2 macrotypes/stx-utils racket/list syntax/stx syntax/parse racket/base))
(require macrotypes/postfix-in)
(require (for-syntax turnstile/mode))
(require (postfix-in - racket/list))
(require (postfix-in - racket/set))

(module+ test
  (require rackunit)
  (require rackunit/turnstile))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating Communication Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (assertion-struct name:id (~datum :) Name:id (slot:id ...))
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

(define-typed-syntax (start-facet name:id ep ...+) ≫
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
  [⊢ (syndicate:react (let- ([#,name-- (#%app- syndicate:current-facet-id)])
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
  #:with τs (mk-Shares- #'(τ))
  -------------------------------------
  [⊢ (syndicate:assert e-) (⇒ : ★/t)
                           (⇒ ν-ep (τs))])

(define-typed-syntax (send! e:expr) ≫
  [⊢ e ≫ e- (⇒ : τ)]
  #:fail-unless (pure? #'e-) "expression not allowed to have effects"
  #:with τm (mk-Sends- #'(τ))
  --------------------------------------
  [⊢ (#%app- syndicate:send! e-) (⇒ : ★/t)
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
                               [react-con #'Message])))))
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
  [(on (a/r/m:asserted/retracted/message p)
       priority:priority
       s ...) ≫
   #:do [(define msg? (free-identifier=? #'syndicate:message (attribute a/r/m.syndicate-kw)))
         (define elab
           (elaborate-pattern/with-com-ty (if msg? #'(message p) #'p)))]
   #:with p/e (if msg? (stx-cadr elab) elab)
   [⊢ p/e ≫ p-- (⇒ : τp)]
   #:fail-unless (pure? #'p--) "pattern not allowed to have effects"
   #:with ([x:id τ:type] ...) (pat-bindings #'p/e)
   [[x ≫ x- : τ] ... ⊢ (begin s ...) ≫ s-
                 (⇒ ν-ep (~effs))
                 (⇒ ν-f (~effs τ-f ...))
                 (⇒ ν-s (~effs τ-s ...))]
   #:with p- (substs #'(x- ...) #'(x ...) (compile-syndicate-pattern #'p/e))
   #:with τ-r (type-eval #'(Reacts (a/r/m.react-con τp) τ-f ... τ-s ...))
   -----------------------------------
   [⊢ (syndicate:on (a/r/m.syndicate-kw p-)
                    #:priority priority.level
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

(define-for-syntax (compile-syndicate-pattern pat)
  (compile-pattern pat
                   #'list-
                   (lambda (id) #`($ #,id))
                   identity))

(define-typed-syntax spawn
  [(spawn τ-c:type s) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  ;; TODO: check that each τ-f is a Role
  #:mode (communication-type-mode #'τ-c.norm)
    [
     [⊢ s ≫ s- (⇒ ν-ep (~effs)) (⇒ ν-s (~effs)) (⇒ ν-f (~effs τ-f ...))]
    ]
  ;; TODO: s shouldn't refer to facets or fields!
  #:with (τ-i τ-o τ-a) (analyze-roles #'(τ-f ...))
  #:fail-unless (<: #'τ-o #'τ-c.norm)
                (format "Output ~a not valid in dataspace ~a" (type->str #'τ-o) (type->str #'τ-c.norm))
  #:with τ-final (mk-Actor- #'(τ-c.norm))
  #:fail-unless (<: #'τ-a #'τ-final)
                "Spawned actors not valid in dataspace"
  #:fail-unless (project-safe? (∩ (strip-? #'τ-o) #'τ-c.norm)
                               #'τ-i)
                "Not prepared to handle all inputs"
  --------------------------------------------------------------------------------------------
  [⊢ (syndicate:spawn (syndicate:on-start s-)) (⇒ : ★/t)
                                               (⇒ ν-s (τ-final))]]
  [(spawn s) ≫
   #:do [(define τc (current-communication-type))]
   #:when τc
   ----------------------------------------
   [≻ (spawn #,τc s)]])

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

(define-typed-syntax (during p s ...) ≫
  #:with p+ (elaborate-pattern/with-com-ty #'p)
  #:with inst-p (instantiate-pattern #'p+)
  ----------------------------------------
  [≻ (on (asserted p+)
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
  [⊢ (dataspace τ-c s ...) ≫ ((~literal erased) ((~literal syndicate:dataspace) s- ...)) (⇒ : t)]
  -----------------------------------------------------------------------------------
  [⊢ (#%app- syndicate:run-ground s- ...) (⇒ : (AssertionSet τ-c))])


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
          (pretty-display (type->strX r)))]
  ----------------------------------
  [⊢ e- (⇒ : τ) (⇒ ν-ep (eps ...)) (⇒ ν-f (fs ...)) (⇒ ν-s (ss ...))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
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
