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
    #:datum-literals (tuple λ)
    (pattern (~or (o:prim-op e:exp ...)
                  basic-val
                  (tuple e:exp ...)
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
    #:datum-literals (tuple _ discard)
    (pattern (~or (tuple p:pat ...)
                  ($ x:id : τ:type)
                  discard
                  x:id
                  e:exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statements

(define-typed-syntax (let-field x:id (~datum :) τ:type e:expr body:expr) ≫
  [⊢ e ≫ e- (⇐ : τ.norm)]
  [[x ≫ x- : (Field τ)] ⊢ body ≫ body- (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)]
  #:fail-unless (flat-type? #'τ.norm) "Keep your functions out of fields"
  ---------
  [⊢ (let- ([x- e-]) body-) (⇒ : τ-body) (⇒ :2 τ-body2) (⇒ :3 τ-body3)])

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

#;(define-syntax-class exp
    #:datum-literals (tuple λ)
    (pattern (~or (o:prim-op e:exp ...)
                  basic-val
                  (tuple e:exp ...)
                  (λ [p:pat s:stmt] ...))))

(define-typed-syntax (tuple e:expr ...) ≫
  [⊢ e ≫ e- (⇒ : τ)] ...
  -----------------------
  [⊢ (list 'tuple e- ...) (⇒ : (Tuple τ ...))])

(define-typed-syntax (λ [p:pat s:stmt] ...) ≫
  #:with (([x:id τ:type] ...) ...) (stx-map pat-bindings #'(p ...))
  [[x ≫ x- : τ] ... ⊢ s ≫ s- (⇒ : τ1) (⇒ :2 τ2) (⇒ :3 τ3)] ...
  [⊢ p ≫ p- ⇒ τ-p] ...
  --------------------------------------------------------------
  [⊢ (match-lambda- [p- s-] ...) ⇒ (Case [→ τ-p (Facet τ1 τ2 τ3)] ...)])

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



;; x[A (D S) ...]

#;(react x
       (assert A)
       (on D S)
       ...)