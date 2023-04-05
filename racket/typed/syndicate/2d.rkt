#lang turnstile

(provide run-2d-dataspace)

(require "core-types.rkt"
         "roles.rkt")


(module shim racket
  (provide run-2d run-2d*)
  (require (only-in syndicate-gl/2d 2d-dataspace))
  (require (only-in (submod syndicate/actor for-module-begin) capture-actor-actions)
           (only-in syndicate/actor-lang current-ground-dataspace run-ground))
  (require syntax/parse/define)

  (define (run-2d boot)
    (parameterize ([current-ground-dataspace (2d-dataspace)])
      (run-ground (capture-actor-actions (boot)))))

  (define-simple-macro (run-2d* s ...)
    (run-2d (lambda () (list s ...))))

  )

(require (submod "." shim))

(define-typed-syntax (run-2d-dataspace s ...) ≫
  [⊢ s ≫ s-] ...
  [⊢ (dataspace s- ...) ≫ _ (⇒ ν (~effs (~AnyActor τ-ds)))]
  -----------------------------------------------------------------------------------
  [⊢ (run-2d* s- ...)
     (⇒ : (AssertionSet τ-ds))]

  )
