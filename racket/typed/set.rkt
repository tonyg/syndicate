#lang turnstile

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require "effects.rkt")
(require "base-types.rkt")
(require "prim.rkt")
(require "list.rkt")
(require (only-in "judgments.rkt" ∩))

(require macrotypes/postfix-in)
(require (postfix-in - racket/set))

(module+ test
  (require turnstile/rackunit-typechecking))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type-constructor Set #:arity = 1
  #:arg-variances (all-args-are covariant))

(define-typed-syntax (set e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  #:fail-unless (all-pure? #'(e- ...)) "expressions must be pure"
  ---------------
  [⊢ (set- e- ...) ⇒ (Set (U τ ...))])

(define-typed-syntax (set-count e) ≫
  [⊢ e ≫ e- ⇒ (~Set _)]
  #:fail-unless (pure? #'e-) "expression must be pure"
  ----------------------
  [⊢ (set-count- e-) ⇒ Int])

(define-typed-syntax (set-add st v) ≫
  [⊢ st ≫ st- ⇒ (~Set τs)]
  #:fail-unless (pure? #'st-) "expression must be pure"
  [⊢ v ≫ v- ⇒ τv]
  #:fail-unless (pure? #'v-) "expression must be pure"
  -------------------------
  [⊢ (set-add- st- v-) ⇒ (Set (U τs τv))])

(define-typed-syntax (set-remove st v) ≫
  [⊢ st ≫ st- ⇒ (~Set τs)]
  #:fail-unless (pure? #'st-) "expression must be pure"
  [⊢ v ≫ v- ⇐ τs]
  #:fail-unless (pure? #'v-) "expression must be pure"
  -------------------------
  [⊢ (set-remove- st- v-) ⇒ (Set τs)])

(define-typed-syntax (set-member? st v) ≫
  [⊢ st ≫ st- ⇒ (~Set τs)]
  #:fail-unless (pure? #'st-) "expression must be pure"
  [⊢ v ≫ v- ⇒ τv]
  #:fail-unless (pure? #'v-) "expression must be pure"
  #:fail-unless ((current-typecheck-relation) #'τv #'τs)
    "type mismatch"
  -------------------------------------
  [⊢ (set-member?- st- v-) ⇒ Bool])

(define-typed-syntax (set-union st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set τ-st)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  -------------------------------------
  [⊢ (set-union- st0- st- ...) ⇒ (Set (U τ-st0 τ-st ...))])

(define-typed-syntax (set-intersect st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set τ-st)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  #:with τr (∩ #'τ-st0 ((current-type-eval) #'(U τ-st ...)))
  -------------------------------------
  [⊢ (set-intersect- st0- st- ...) ⇒ (Set τr)])

(define-typed-syntax (set-subtract st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set _)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  -------------------------------------
  [⊢ (set-subtract- st0- st- ...) ⇒ (Set τ-st0)])

(define-typed-syntax (list->set l) ≫
  [⊢ l ≫ l- ⇒ (~List τ)]
  #:fail-unless (pure? #'l-) "expression must be pure"
  -----------------------
  [⊢ (list->set- l-) ⇒ (Set τ)])

(define-typed-syntax (set->list s) ≫
  [⊢ s ≫ s- ⇒ (~Set τ)]
  #:fail-unless (pure? #'s-) "expression must be pure"
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