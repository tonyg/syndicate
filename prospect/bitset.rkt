#lang racket/base
;; Set of small integers stored in bytes.

(provide bitset
         bitset?
         list->bitset
         bitset-count
         bitset-empty
         bitset-empty?
         bitset-add
         bitset-remove
         bitset-union
         bitset-intersect
         bitset-subtract
         bitset->list
         bitset-member?
         )

(require racket/performance-hint)

(define (bitset . vs)
  (list->bitset vs))

(define (bitset? x)
  (bytes? x))

(define (length-to-include v)
  (define-values (y i) (quotient/remainder v 8))
  (+ y 1))

(define (list->bitset vs)
  (define limit (apply max 0 vs))
  (foldr bitset-add!* (make-bytes (length-to-include limit)) vs))

;; Cribbed from data/bit-vector.rkt
(require (for-syntax racket/base
                     (only-in data/private/count-bits-in-fixnum fxpopcount)))
(define popcount-table
  (let ()
    (define-syntax (make-table stx)
      (with-syntax ([(elt ...)
                     (for/list ([i (in-range 256)])
                       (fxpopcount i))])
        ;; Literal immutable vector allocated once (?)
        #'(quote #(elt ...))))
    (make-table)))

(require (only-in racket/unsafe/ops unsafe-vector-ref))
(define (bitset-count bs)
  (for/sum [(b (in-bytes bs))]
    (unsafe-vector-ref popcount-table b)))

(define (bitset-empty)
  (bytes))

(define (bitset-empty? bs)
  (for/and ((b (in-bytes bs))) (zero? b)))

(define (bitset-copy/extend bs v)
  (define minsize (length-to-include v))
  (if (>= (bytes-length bs) minsize)
      (bytes-copy bs)
      (let ((result (make-bytes minsize)))
        (bytes-copy! result 0 bs)
        result)))

(define (bitset-add bs v)
  (bitset-add!* v (bitset-copy/extend bs v)))

(define (bitset-trim bs)
  (define len (bytes-length bs))
  (let loop ((y (- len 1)))
    (cond
      [(negative? y) (bytes)]
      [(zero? (bytes-ref bs y)) (loop (- y 1))]
      [(= y (- len 1)) bs]
      [else (subbytes bs 0 (+ y 1))])))

(define (bitset-remove bs v)
  (bitset-trim (bitset-remove! (bytes-copy bs) v)))

(define (bitset-for-merge bs1 bs2)
  (make-bytes (max (bytes-length bs1) (bytes-length bs2))))

(define-inline (bitset-merge* bs bs-short bs-long combiner)
  (for ((y (in-range (bytes-length bs-short))))
    (bytes-set! bs y (combiner (bytes-ref bs-short y) (bytes-ref bs-long y))))
  (for ((y (in-range (bytes-length bs-short) (bytes-length bs-long))))
    (bytes-set! bs y (combiner 0 (bytes-ref bs-long y))))
  bs)

(define-inline (bitset-merge bs1 bs2 combiner)
  (define bs (bitset-for-merge bs1 bs2))
  (if (< (bytes-length bs1) (bytes-length bs2))
      (bitset-merge* bs bs1 bs2 combiner)
      (bitset-merge* bs bs2 bs1 (lambda (b a) (combiner a b)))))

(define (bitset-union bs1 bs2) (bitset-merge bs1 bs2 bitwise-ior))
(define (bitset-intersect bs1 bs2) (bitset-trim (bitset-merge bs1 bs2 bitwise-and)))
(define (bitset-subtract bs1 bs2)
  (bitset-trim (bitset-merge bs1 bs2 (lambda (a b) (bitwise-and a (bitwise-not b))))))

(define (bitset->list bs)
  (for/fold [(acc '())]
            [(b (in-bytes bs)) (byte-index (in-naturals))]
    (if (zero? b)
        acc
        (for/fold [(acc acc)] [(bit-index (in-range 8))]
          (if (bitwise-bit-set? b bit-index)
              (cons (+ (* byte-index 8) bit-index) acc)
              acc)))))

(define (bitset-member? bs v)
  (define-values (y i) (quotient/remainder v 8))
  (and (> (bytes-length bs) y)
       (bitwise-bit-set? (bytes-ref bs y) i)))

(define (bitset-add!* v bs)
  (define-values (y i) (quotient/remainder v 8))
  (bytes-set! bs y (bitwise-ior (bytes-ref bs y) (arithmetic-shift 1 i)))
  bs)

(define (bitset-remove! bs v)
  (define-values (y i) (quotient/remainder v 8))
  (bytes-set! bs y (bitwise-and (bytes-ref bs y) (bitwise-not (arithmetic-shift 1 i))))
  bs)

(module+ test
  (require rackunit)
  (require racket/set)
  (define-syntax-rule (check-set-equal? actual expected)
    (check-equal? (list->set actual) (list->set expected)))
  (check-set-equal? (bitset->list (bitset 1 2 3 4 5)) '(1 2 3 4 5))
  (check-set-equal? (bitset->list (bitset 10 20 30 40 50)) '(10 20 30 40 50))
  (check-set-equal? (bitset->list (bitset 5 4 3 2 1)) '(1 2 3 4 5))
  (check-set-equal? (bitset->list (bitset-union (bitset 1 2 3) (bitset 2 3 4))) '(1 2 3 4))
  (check-set-equal? (bitset->list (bitset-intersect (bitset 1 2 3) (bitset 2 3 4))) '(2 3))
  (check-set-equal? (bitset->list (bitset-subtract (bitset 1 2 3) (bitset 2 3 4))) '(1))
  (check-true (bitset-member? (bitset 1 2 3) 2))
  (check-false (bitset-member? (bitset 1 2 3) 4))
  (check-true (bitset-empty? (bitset)))
  (check-false (bitset-empty? (bitset 1)))
  (check-equal? (bitset-count (bitset 1 2 3)) 3)
  (check-equal? (bitset-count (bitset)) 0)
  (check-equal? (bitset-count (bitset-union (bitset 1 2 3) (bitset 2 3 4))) 4)
  (check-true (bitset? (bitset-empty)))
  (check-true (bitset? (bitset)))
  (check-false (bitset? 123))
  (check-false (bitset? (list 1 2 3)))
  (check-false (bitset? 'a))
  )
