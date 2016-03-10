#lang racket/base

(provide hash-order)

(require data/order)
(require racket/contract)

(define hash-order
  (order 'hash-order
         any/c
         eq?
         (lambda (a b)
           (define a-code (eq-hash-code a))
           (define b-code (eq-hash-code b))
           (or (< a-code b-code)
               (and (= a-code b-code)
                    (eq? (datum-order a b) '<))))))
