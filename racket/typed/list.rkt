#lang turnstile

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require "base-types.rkt")
(require "prim.rkt")
(require "effects.rkt")
(require (only-in "define-like-things.rkt" begin))

(require macrotypes/postfix-in)
(require (postfix-in - racket/list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type-constructor List #:arity = 1
  #:arg-variances (all-args-are covariant))

(define-typed-syntax (list e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  #:fail-unless (all-pure? #'(e- ...)) "expressions must be pure"
  -------------------
  [⊢ (list- e- ...) ⇒ (List (U τ ...))])

(define-typed-syntax (cons e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ τ1]
  #:fail-unless (pure? #'e1-) "expression must be pure"
  [⊢ e2 ≫ e2- ⇒ (~List τ2)]
  #:fail-unless (pure? #'e2-) "expression must be pure"
  #:with τ-l ((current-type-eval) #'(List (U τ1 τ2)))
  ----------------------------------------
  [⊢ (cons- e1- e2-) ⇒ τ-l])

(define-typed-syntax (for/fold [acc:id e-acc]
                               [x:id e-list]
                       e-body ...+) ≫
  [⊢ e-list ≫ e-list- ⇒ (~List τ-l)]
  #:fail-unless (pure? #'e-list-) "expression must be pure"
  [⊢ e-acc ≫ e-acc- ⇒ τ-a]
  #:fail-unless (pure? #'e-acc-) "expression must be pure"
  [[x ≫ x- : τ-l] [acc ≫ acc- : τ-a] ⊢ (begin e-body ...) ≫ e-body- ⇐ τ-a]
  #:fail-unless (pure? #'e-body-) "body must be pure"
  -------------------------------------------------------
  [⊢ (for/fold- ([acc- e-acc-])
                ([x- (in-list- e-list-)])
       e-body-)
     ⇒ τ-a])

(define-typed-syntax (for ([x:id e-list] ...)
                       e-body ...+) ≫
  [⊢ e-list ≫ e-list- ⇒ (~List τ-l)] ...
  #:fail-unless (all-pure? #'(e-list- ...)) "expressions must be pure"
  [[x ≫ x- : τ-l] ... ⊢ (begin e-body ...) ≫ e-body- (⇒ : τ-b)
                  (⇒ ep (~effs eps ...))
                  (⇒ f (~effs fs ...))
                  (⇒ s (~effs ss ...))]
  -------------------------------------------------------
  [⊢ (for- ([x- (in-list- e-list-)] ...)
       e-body-) (⇒ : ★/t)
                (⇒ ep (eps ...))
                (⇒ f (fs ...))
                (⇒ s (ss ...))])

(define-typed-syntax (empty? e) ≫
  [⊢ e ≫ e- ⇒ (~List _)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  -----------------------
  [⊢ (empty?- e-) ⇒ Bool])

(define-typed-syntax (first e) ≫
  [⊢ e ≫ e- ⇒ (~List τ)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  -----------------------
  [⊢ (first- e-) ⇒ τ])

(define-typed-syntax (rest e) ≫
  [⊢ e ≫ e- ⇒ (~List τ)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  -----------------------
  [⊢ (rest- e-) ⇒ (List τ)])

(define-typed-syntax (member? e l) ≫
  [⊢ e ≫ e- ⇒ τe]
  #:fail-unless (pure? #'e-) "expression must be pure"
  [⊢ l ≫ l- ⇒ (~List τl)]
  #:fail-unless (pure? #'l-) "expression must be pure"
  #:fail-unless ((current-typecheck-relation) #'τe #'τl) "incompatible list"
  ----------------------------------------
  [⊢ (member?- e- l-) ⇒ Bool])

(define- (member?- v l)
  (and- (member- v l) #t))
