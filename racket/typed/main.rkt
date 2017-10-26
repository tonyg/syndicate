#lang turnstile

(provide #%module-begin
         #%app
         #%top-interaction
         require
         Int
         let-field
         #%datum
         Field
         ⊥
         nil
         tuple Tuple
         bind Bind
         discard Discard
         + -
         λ Case →
         Facet)

(require (rename-in racket/match [match-lambda match-lambda-]))
(require (prefix-in syndicate: syndicate/actor))

(module+ test
  (require rackunit)
  (require turnstile/rackunit-typechecking))

;(require syndicate/actor-lang)
#;(provide (all-from-out syndicate/actor-lang))

(define-base-types Int Bool String ⊥ Discard)
(define-type-constructor Field #:arity = 1)
(define-type-constructor Facet #:arity = 3)
(define-type-constructor Bind #:arity = 1)
(define-type-constructor Tuple #:arity >= 0)
(define-type-constructor U #:arity >= 0)
(define-type-constructor Case #:arity >= 0)
(define-type-constructor → #:arity > 0)

(begin-for-syntax
  (define-syntax-class exp
    #:datum-literals (tuple λ ref)
    (pattern (~or (o:prim-op e:exp ...)
                  basic-val
                  (tuple e:exp ...)
                  (ref x:id)
                  (λ [p:pat s:stmt] ...))))

  (define-syntax-class basic-val
    (pattern (~or boolean
                  integer
                  string)))

  (define-syntax-class prim-op
    (pattern (~or (~literal +)
                  (~literal -))))

  (define-syntax-class stmt
    #:datum-literals (:
                      begin
                      let-field
                      let-function
                      set!
                      spawn
                      dataspace
                      stop
                      facet)
    (pattern (~or (begin seq:stmt ...)
                  (e1:exp e2:exp)
                  nil ;; TODO - remove, only for making some initial examples
                  (let-field [x:id : τ:type e:exp] lf-body:stmt)
                  (let-function [f:id : τ:type e:exp] let-fun-body:stmt)
                  (set! x:id e:exp)
                  (spawn τ:type s:stmt)
                  (dataspace τ:type nested:stmt ...)
                  (stop x:id s:stmt)
                  (facet x:id ep:endpoint ...))))

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
    (pattern (~or (~and (tuple p:pat ...)
                        (~bind [syndicate-pattern #'(list 'tuple p.syndicate-pattern ...)]
                               [match-pattern #'(list 'tuple p.match-pattern ...)]))
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
;; Statements

#;(define-syntax-class stmt
    #:datum-literals (spawn
                      dataspace
                      stop
                      facet)
    (pattern (~or (begin seq:stmt ...)
                  (e1:exp e2:exp)
                  nil ;; TODO - remove, only for making some initial examples
                  (let-field [x:id : τ:type e:exp] lf-body:stmt)
                  (let-function [f:id : τ:type e:exp] let-fun-body:stmt)
                  (set! x:id e:exp)
                  (spawn τ:type s:stmt)
                  (dataspace τ:type nested:stmt ...)
                  (stop x:id s:stmt)
                  (facet x:id ep:endpoint ...))))

(define-typed-syntax (set! x:id e:exp) ≫
  [⊢ e ≫ e- ⇒ τ]
  [⊢ x ≫ x- ⇒ (Field τ-x)]
  ;; TODO: (~ τ τ-x)
  -------------------------
  [⊢ (x- e-) (⇒ : ⊥) (⇒ :2 ⊥) (⇒ :3 ⊥)])

(define-typed-syntax (spawn τ-c:type s:stmt) ≫
  #:fail-unless (flat-type? #'τ-c.norm) "Communication type must be first-order"
  [⊢ s ≫ s- (⇒ : τ-i) (⇒ :2 τ-o) (⇒ :3 τ-a)]
  ;; TODO: s shouldn't refer to facets or fields!
  ;; TODO: τ-o <: τ-c
  ;; TODO: (Actor τ-a) <: (Actor τ-c)
  ;; TODO: project-safe(strip-?(τ-o) ∩ τ-c, τ-i)
  ----------------------------------------------
  [⊢ (syndicate:spawn s-) (⇒ : ⊥) (⇒ :2 ⊥) (⇒ τ-c)])

(define-typed-syntax (let-field x:id (~datum :) τ:type e:expr body:expr) ≫
  [⊢ e ≫ e- (⇐ : τ.norm)]
  [[x ≫ x- : (Field τ)] ⊢ body ≫ body- (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)]
  #:fail-unless (flat-type? #'τ.norm) "Keep your functions out of fields"
  ------------------------------------------------------------------------
  [⊢ (begin- (syndicate:field [x- e-]) body-) (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)])

(define-typed-syntax (let-function f:id (~datum :) τ:type e:expr body:expr) ≫
  [⊢ e ≫ e- (⇐ : τ.norm)]
  [[x ≫ x- : τ] ⊢ body ≫ body- (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)]
  #:fail-unless (procedure-type? #'τ.norm) "let-function only binds procedures"
   ------------------------------------------------------------------------
  [⊢ (let- ([x- e-]) body-) (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)])

(define-for-syntax (procedure-type? τ)
  (syntax-parse τ
    [(~Case (~→ τ1:type τ2:type) ...) #t]
    [_ #f]))

(define-typed-syntax (begin s:stmt ...) ≫
  [⊢ s ≫ s- (⇒ : τ1) (⇒ : τ2) (⇒ : τ3)] ...
  ------------------------------------------
  [⊢ (begin- s- ...) (⇒ : (U τ1 ...)) (⇒ : (U τ2 ...)) (⇒ : (U τ3 ...))])

(define-typed-syntax nil
  [_:id ≫
   ---------------
   [⊢ (void-) (⇒ : ⊥) (⇒ :2 ⊥) (⇒ :3 ⊥)]])

(define-for-syntax (flat-type? τ)
  (syntax-parse τ
    [(~→ _ ...) #f]
    [(~Field _) #f]
    [_ #t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions

(define-typed-syntax (tuple e:expr ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  -----------------------
  [⊢ (list 'tuple e- ...) (⇒ : (Tuple τ ...))])

(define-typed-syntax (ref x:id) ≫
  [⊢ x ≫ x- ⇒ (Field τ)]
  ------------------------
  [⊢ (x-) ⇒ τ])

(define-typed-syntax (λ [p:pat s:stmt] ...) ≫
  #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
  [[x ≫ x- : τ] ... ⊢ s ≫ s- (⇒ : τ1) (⇒ :2 τ2) (⇒ :3 τ3)] ...
  ;; REALLY not sure how to handle p/p-/p.match-pattern,
  ;; particularly w.r.t. typed terms that appear in p.match-pattern
  [⊢ p ≫ p- ⇒ τ-p] ...
  --------------------------------------------------------------
  ;; TODO: add a catch-all error clause
  [⊢ (match-lambda- [p.match-pattern s-] ...) ⇒ (Case [→ τ-p (Facet τ1 τ2 τ3)] ...)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patterns

(define-typed-syntax (bind x:id τ:type) ≫
  --------------------
  ;; TODO: at some point put $ back in
  [⊢ (void-) ⇒ (Bind τ)])

(define-typed-syntax discard
  [_ ≫
   --------------------
   ;; TODO: change void to _
   [⊢ (void-) ⇒ Discard]])

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
(define-primop + (→ Int Int Int))
(define-primop - (→ Int Int Int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Values

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
  ----------------
  [⊢ (#%datum- . n) ⇒ Int]]
  [(_ . b:boolean) ≫
  ----------------
  [⊢ (#%datum- . b) ⇒ Bool]]
  [(_ . s:string) ≫
  ----------------
  [⊢ (#%datum- . s) ⇒ String]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (check-true (void? ((λ [(bind x Int) nil]) 1))))

(define bank-account
  #'(facet account
           (let-field [balance : Int 0]
                      (begin (assert (list "balance" (ref balance)))
                             (on (asserted (list "deposit" (bind amount Int)))
                                 (set! balance (+ (ref balance) amount)))))))

(define client
  #'(facet client
           (let-field [deposits : Int 3]
                      (on (asserted (list "balance" (bind amount Int)))
                          (assert (list "deposit" 100))
                          (set! deposits (- (ref deposits) 1))
                          (if (= (ref deposits) 0)
                              (stop client (begin)))))))