#lang racket/base

(provide bytes-index)

;; This should probably be in the standard library.
(define (bytes-index bs b)
  (define len (bytes-length bs))
  (let loop ((i 0))
    (cond [(= i len) #f]
          [(eqv? (bytes-ref bs i) b) i]
          [else (loop (+ i 1))])))
