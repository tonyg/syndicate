#lang turnstile

(provide Observe★)

(require "core-types.rkt")
(require turnstile/typedefs)

(define-syntax (Observe★ stx)
  (define star (type-eval #'★/t))
  (syntax-parse stx
    [(_ TyCons:id)
     #:do [(define arity? (get-type-arity #'TyCons))]
     #:when arity?
     (mk-Observe- (list (reassemble-type #'TyCons (make-list (arity-min arity?) star))))]
    [(_ (~Any/new TyCons τ ...))
     #:when (reassemblable? #'TyCons)
     (mk-Observe- (list (reassemble-type #'TyCons (stx-map (lambda (_) star) #'(τ ...)))))]
    [_
     (raise-syntax-error #f "Not a type that can automatically be subscribed to" stx)]))

(begin-for-syntax
  ;; Arity -> Nat
  (define (arity-min a)
    (match a
      [(arity-eq n) n]
      [(arity-ge n) n])))
