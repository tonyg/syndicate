#lang syndicate

(require "../../widgets.rkt")
(require (only-in racket/format ~a ~r))

;; a bi-directional temperature converter (Fahrenheit vs Celsius)

(define ((callback setter) field val)
  (define-values (field:num last) (string->number* val))
  (cond
    [(and field:num (rational? field:num))
     (define inexact-n (* #i1.0 field:num))
     (setter inexact-n)
     (render field inexact-n last)]
    [else (send! (set-text-field-background field "red"))]))

(define (string->number* str)
  (define n (string->number str))
  (values n (and n (string-ref str (- (string-length str) 1)))))

(define (flow *from --> *to to-field)
  (λ (x)
    (*from x)
    (*to (--> x))
    (render to-field (*to) "")))

(define (render to-field *to last)
  (send! (set-text-field-background to-field "white"))
  (send! (set-text-field to-field (~a (~r *to #:precision 4) (if (eq? #\. last) "." "")))))

(define frame   (spawn-frame #:label "temperature converter"))
(define pane    (spawn-horizontal-pane #:parent frame))

(define (make-field v0 lbl)
  (spawn-text-field #:parent pane
                    #:min-width 199
                    #:label lbl
                    #:init-value v0))

(define C0 0)
(define F0 32)

(define C-field (make-field  (~a C0) "celsius:"))
(define F-field (make-field  (~a F0) " = fahrenheit:"))

(spawn

 (field [*C C0]
        [*F F0])

 (define celsius->fahrenheit (callback (flow *C (λ (c) (+  (* c 9/5) 32)) *F F-field)))
 (define fahrenheit->celsius (callback (flow *F (λ (f) (* (- f 32) 5/9))  *C C-field)))

 (on (message (text-field-update C-field $val))
     (celsius->fahrenheit C-field val))
 (on (message (text-field-update F-field $val))
     (fahrenheit->celsius F-field val))
 (on-start
  (send! (show frame #t))))
