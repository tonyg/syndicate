#lang turnstile

(provide supervise)

(require "core-types.rkt")
(require (submod "roles.rkt" implementation-details))
(require (prefix-in syndicate: syndicate/supervise))

;; TODO - need to activate timestate driver for supervise to actually work

(define-typed-syntax (supervise on:opt-name s ...+) ≫
  [⊢ s ≫ s- (⇒ ν (~effs F ...))] ...
  #:do [(ensure-all! AnyActor? #'(F ... ...) "only spawn effects allowed" #:src this-syntax)]
  ------------------------------
  [⊢ (syndicate:supervise (~? (~@ #:name on.name-)) s- ...)
     (⇒ : ★/t)
     (⇒ ν (F ... ...))]
  )
