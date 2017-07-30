#lang racket/base
;; Hash-table utilities that are not (yet) part of Racket

(provide hash-set/remove
         hashset-member?
         hashset-add
         hashset-remove)

(require racket/set)

(define (hash-set/remove ht key val [default-val #f] #:compare [compare equal?])
  (if (compare val default-val)
      (hash-remove ht key)
      (hash-set ht key val)))

(define (hashset-member? ht key val)
  (define s (hash-ref ht key #f))
  (and s (set-member? s val)))

(define (hashset-add ht key val #:set [set set])
  (hash-set ht key (set-add (hash-ref ht key set) val)))

(define (hashset-remove ht k v)
  (define old (hash-ref ht k #f))
  (if old
      (let ((new (set-remove old v)))
        (if (set-empty? new)
            (hash-remove ht k)
            (hash-set ht k new)))
      ht))
