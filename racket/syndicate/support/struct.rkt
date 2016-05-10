#lang racket/base
;; Misc support routines not (yet) provided by racket itself

(provide non-object-struct?
         struct-type-name
         struct->struct-type)

(require (only-in racket/class object?))

;; Any -> Boolean
;; Racket objects are structures, so we reject them explicitly for
;; now, leaving them opaque to unification.
(define (non-object-struct? x)
  (and (struct? x)
       (not (object? x))))

;; struct-type -> Symbol
;; Extract just the name of the given struct-type.
(define (struct-type-name st)
  (define-values (name x2 x3 x4 x5 x6 x7 x8) (struct-type-info st))
  name)

;; Structure -> StructType
;; Errors when given any struct that isn't completely transparent/prefab.
(define (struct->struct-type p)
  (define-values (t skipped?) (struct-info p))
  (when skipped? (error 'struct->struct-type "Cannot reflect on struct instance ~v" p))
  t)
