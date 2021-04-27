#lang racket

(provide ones
         vec+)

(struct vec (x y z) #:transparent)

(define ones (vec 1 1 1))

(define (vec+ v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))
       (+ (vec-z v1) (vec-z v2))))
