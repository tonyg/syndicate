#lang turnstile

(provide (for-syntax flat-type?)) #;(flat-type? Type)

(require "../base-types.rkt")

(define-for-syntax (flat-type? τ)
  (syntax-parse τ
    [(~→ τ ...) #f]
    [(~Actor τ) #f]
    [_ #t]))