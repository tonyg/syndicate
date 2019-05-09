#lang turnstile

(provide Set
         (for-syntax ~Set)
         set
         set-member?
         set-add
         set-remove
         set-count
         set-union
         set-subtract
         set-intersect
         list->set
         set->list)

(require "core-types.rkt")
(require (only-in "prim.rkt" Int))
(require (only-in "list.rkt" ~List))

(require (postfix-in - racket/set))

(module+ test
  (require rackunit)
  (require rackunit/turnstile)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-container-type Set #:arity = 1)

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
  #:fail-unless (<: #'τv #'τs)
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
  #:with τr (∩ #'τ-st0 (type-eval #'(U τ-st ...)))
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
  (require "prim.rkt")
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
