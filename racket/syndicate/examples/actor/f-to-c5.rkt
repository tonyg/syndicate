#lang syndicate
;; After the Fahrenheit-to-Celsius converter example in "Fabrik - A
;; Visual Programming Environment", Ingalls, Wallace, Chow, Ludolph
;; and Doyle, OOPSLA 1988.

;;---------------------------------------------------------------------------

(define (make-idempotent-field name initial-value [=? equal?])
  (local-require (submod syndicate/actor implementation-details))
  (define f (make-field name initial-value))
  (match-lambda*
    [(list) (f)]
    [(list new-value)
     (unless (=? (field-ref (field-handle-desc f)) new-value)
       (f new-value))]))

(define-syntax-rule (idempotent-field [id init] ...)
  (begin (define id (make-idempotent-field 'id init)) ...))

;;---------------------------------------------------------------------------

(struct temperature (unit value) #:prefab)
(struct set-temperature (unit value) #:prefab)

(spawn (idempotent-field [C 0] [F 32])
       (begin/dataflow (F (+ (* (C) 9/5) 32)))
       (begin/dataflow (C (* (- (F) 32) 5/9)))
       (assert (temperature 'C (C)))
       (assert (temperature 'F (F)))
       (on (message (set-temperature 'C $v)) (C v))
       (on (message (set-temperature 'F $v)) (F v)))

(spawn (on (asserted (temperature $unit $value))
           (printf "Temperature in ~a is ~a\n" unit (exact->inexact value))))

(spawn (on (asserted (observe (set-temperature _ _)))
           (send! (set-temperature 'C 20))
           (send! (set-temperature 'F 90))))
