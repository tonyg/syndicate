#lang syndicate
;; After the Fahrenheit-to-Celsius converter example in "Fabrik - A
;; Visual Programming Environment", Ingalls, Wallace, Chow, Ludolph
;; and Doyle, OOPSLA 1988.

;;---------------------------------------------------------------------------

(struct versioned-value (value version write-id) #:prefab)

(define (random-id)
  (local-require racket/random sha)
  (bytes->hex-string (crypto-random-bytes 16)))

(define versioned-field-sentinel (cons #f #f))

(define (make-versioned-field name initial-value)
  (local-require (only-in (submod syndicate/actor implementation-details) make-field))
  (define f (make-field name (versioned-value initial-value 0 "")))
  (match-lambda*
    [(list) (versioned-value-value (f))]
    [(list (== versioned-field-sentinel eq?) 'get-field) f]
    [(list new-value)
     (f (versioned-value new-value (current-inexact-milliseconds) (random-id)))]
    [(list new-value cause)
     (define cause-f (versioned-field->field cause))
     (match-define (versioned-value _ my-version my-write-id) (f))
     (match-define (versioned-value _ cause-version cause-write-id) (cause-f))
     (when (or (> cause-version my-version)
               (and (= cause-version my-version) (string>? cause-write-id my-write-id)))
       (f (versioned-value new-value cause-version cause-write-id)))]))

(define (versioned-field->field vf) (vf versioned-field-sentinel 'get-field))

(define-syntax-rule (versioned-field [id init] ...)
  (begin (define id (make-versioned-field 'id init)) ...))

;;---------------------------------------------------------------------------

(struct temperature (unit value) #:prefab)
(struct set-temperature (unit value) #:prefab)

(spawn (versioned-field [C 0] [F 32])
       (begin/dataflow
         (F (+ (* (C) 9/5) 32) C)
         (C (* (- (F) 32) 5/9) F))
       (assert (temperature 'C (C)))
       (assert (temperature 'F (F)))
       (on (message (set-temperature 'C $v)) (C v))
       (on (message (set-temperature 'F $v)) (F v)))

(spawn (on (asserted (temperature $unit $value))
           (printf "Temperature in ~a is ~a\n" unit (exact->inexact value))))

(spawn (on (asserted (observe (set-temperature _ _)))
           (send! (set-temperature 'C 20))
           (send! (set-temperature 'F 90))))
