#lang turnstile

(provide List
         (for-syntax ~List)
         list
         cons
         first
         rest
         member?
         empty?
         reverse)

(require "core-types.rkt")
(require (postfix-in - racket/list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-container-type List #:arity = 1)

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
  #:with τ-l (type-eval #'(List (U τ1 τ2)))
  ----------------------------------------
  [⊢ (cons- e1- e2-) ⇒ τ-l])

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
  #:fail-unless (<: #'τe #'τl) "incompatible list"
  ----------------------------------------
  [⊢ (member?- e- l-) ⇒ Bool])

(define- (member?- v l)
  (and- (member- v l) #t))

(require/typed racket/base
  [reverse : (∀ (X) (→fn (List X) (List X)))])
