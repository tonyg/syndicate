#lang racket/base

(provide hash-order)

(require data/order)
(require racket/contract)

(define hash-order
  (order 'hash-order
         any/c
         eq?
         (lambda (a b) (< (eq-hash-code a) (eq-hash-code b)))))
