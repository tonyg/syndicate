#lang turnstile

(provide Set
         (for-syntax ~Set)
         set
         ;; set-member?
         ;; set-add
         ;; set-remove
         ;; set-count
         set-union
         set-subtract
         set-intersect
         ;; list->set
         ;; set->list
         (typed-out [[set-first- : (∀ (X) (→fn (Set X) X))] set-first]
                    [[set-empty?- : (∀ (X) (→fn (Set X) Bool))] set-empty?]
                    [[set-count- : (∀ (X) (→fn (Set X) Int))] set-count]
                    [[set-add- : (∀ (X) (→fn (Set X) X (Set X)))] set-add]
                    [[set-remove- : (∀ (X) (→fn (Set X) X (Set X)))] set-remove]
                    [[set-member?- : (∀ (X) (→fn (Set X) X Bool))] set-member?]
                    [[list->set- : (∀ (X) (→fn (List X) (Set X)))] list->set]
                    [[set->list- : (∀ (X) (→fn (Set X) (List X)))] set->list]
                    ))

(require "core-types.rkt")
(require (only-in "prim.rkt" Int Bool))
(require (only-in "list.rkt" ~List List))

(require (postfix-in - racket/set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-container-type Set #:arity = 1)

(define-typed-syntax (set e ...) ≫
  [⊢ e ≫ e- ⇒ τ] ...
  #:fail-unless (all-pure? #'(e- ...)) "expressions must be pure"
  ---------------
  [⊢ (#%app- set- e- ...) ⇒ (Set (U τ ...))])

(define-typed-syntax (set-union st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set τ-st)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  -------------------------------------
  [⊢ (#%app- set-union- st0- st- ...) ⇒ (Set (U τ-st0 τ-st ...))])

(define-typed-syntax (set-intersect st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set τ-st)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  #:with τr (∩ #'τ-st0 (type-eval #'(U τ-st ...)))
  -------------------------------------
  [⊢ (#%app- set-intersect- st0- st- ...) ⇒ (Set τr)])

(define-typed-syntax (set-subtract st0 st ...) ≫
  [⊢ st0 ≫ st0- ⇒ (~Set τ-st0)]
  #:fail-unless (pure? #'st0-) "expression must be pure"
  [⊢ st ≫ st- ⇒ (~Set _)] ...
  #:fail-unless (all-pure? #'(st- ...)) "expressions must be pure"
  -------------------------------------
  [⊢ (#%app- set-subtract- st0- st- ...) ⇒ (Set τ-st0)])
