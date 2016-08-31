#lang racket/base

(require "treap.rkt")

(provide tset?
         datum-tset
         datum-tset-empty
         make-tset
         tset-count
         tset-empty
         tset-empty?
         tset-add
         tset-remove
         tset-union
         tset-intersect
         tset-subtract
         tset->list
         tset-member?
         tset/set-union
         )

(require "hash-order.rkt")
(require (only-in racket/set set-union list->set))

(define (tset? t)
  (treap? t))

(define (datum-tset . elts)
  (make-tset hash-order elts))

(define (make-tset o elts)
  (for/fold [(t (tset-empty o))] [(e elts)] (tset-add t e)))

(define (tset-count t)
  (treap-size t))

(define (tset-empty o)
  (treap-empty o))

(define (tset-empty? t)
  (treap-empty? t))

(define (tset-add t v)
  (treap-insert t v #t))

(define (tset-remove t v)
  (treap-delete t v))

(define (tset-union t1 t2)
  (if (< (treap-size t1) (treap-size t2))
      (treap-fold t1 treap-insert t2)
      (treap-fold t2 treap-insert t1)))

(define (tset-intersect t1 t2)
  (if (< (treap-size t1) (treap-size t2))
      (treap-fold t1
                  (lambda (t k v) (if (treap-has-key? t2 k) (treap-insert t k v) t))
                  (treap->empty t1))
      (treap-fold t2
                  (lambda (t k v) (if (treap-has-key? t1 k) (treap-insert t k v) t))
                  (treap->empty t2))))

(define (tset-subtract t1 t2)
  (if (< (treap-size t1) (treap-size t2))
      (treap-fold t1
                  (lambda (t k v) (if (treap-has-key? t2 k) t (treap-insert t k v)))
                  (treap->empty t1))
      (treap-fold t2
                  (lambda (t k v) (treap-delete t k))
                  t1)))

(define (tset->list t)
  (treap-fold t (lambda (acc k v) (cons k acc)) '()))

(define (tset-member? t k)
  (treap-has-key? t k))

(define (tset/set-union t s)
  (set-union (list->set (tset->list t)) s))

(define datum-tset-empty (datum-tset))

(module+ test
  (require rackunit)
  (require data/order)
  (define (tset . elts) (make-tset hash-order elts))
  (check-equal? (tset->list (tset 1 2 3 4 5)) '(1 2 3 4 5))
  (check-equal? (tset->list (tset 5 4 3 2 1)) '(1 2 3 4 5))
  (check-equal? (tset->list (tset-union (tset 1 2 3) (tset 2 3 4))) '(1 2 3 4))
  (check-equal? (tset->list (tset-intersect (tset 1 2 3) (tset 2 3 4))) '(2 3))
  (check-equal? (tset->list (tset-subtract (tset 1 2 3) (tset 2 3 4))) '(1))
  (check-true (tset-member? (tset 1 2 3) 2))
  (check-false (tset-member? (tset 1 2 3) 4))
  (check-true (tset-empty? (tset)))
  (check-false (tset-empty? (tset 1)))
  (check-equal? (tset-count (tset 1 2 3)) 3)
  (check-equal? (tset-count (tset)) 0)
  (check-equal? (tset-count (tset-union (tset 1 2 3) (tset 2 3 4))) 4)
  (check-true (tset? (tset-empty hash-order)))
  (check-true (tset? (tset)))
  (check-false (tset? 123))
  (check-false (tset? (list 1 2 3)))
  (check-false (tset? 'a))
  )
