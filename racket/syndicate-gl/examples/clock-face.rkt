#lang syndicate/actor
;; Compare to "ezd" clock-face example from: J. F. Bartlett, “Don’t
;; Fidget with Widgets, Draw!,” Palo Alto, California, DEC WRL
;; Research Report 91/6, May 1991.

(require lang/posn)
(require 2htdp/image)
(require (only-in racket/math pi))
(require racket/date)
(require "../2d.rkt")

(define hand
  (polygon (list (make-posn 0 0)
                 (make-posn 30 10)
                 (make-posn 100 0)
                 (make-posn 30 -10))
           "solid"
           "black"))

(define (fmod a b)
  (- a (* b (truncate (/ a b)))))

(define (hand-sprite id layer angle-field length)
  (sprite #:id id layer `((translate 100 100)
                          (rotate ,(fmod (- 90 (angle-field)) 360))
                          (scale ,length ,(/ length 5))
                          (translate 0 -0.5)
                          (touchable ,id ,in-unit-square?)
                          (texture ,hand))))

(define (text-sprite layer x y content)
  (define i (text content 24 "green"))
  (simple-sprite layer x y (image-width i) (image-height i) i))

(spawn (field [minute-angle 0]
              [hour-angle 0]
              [start-time (current-inexact-milliseconds)]
              [elapsed-seconds 0]
              [displacement (let ((now (current-date)))
                              (* 6 (+ (* 60 (date-hour now))
                                      (date-minute now))))])

       (assert (outbound (simple-sprite 10 0 0 200 200 (circle 100 "solid" "blue"))))
       (assert (outbound (hand-sprite 'minute 9 minute-angle 95)))
       (assert (outbound (text-sprite 8 40 40 "time")))
       (assert (outbound (text-sprite 8 110 80 "drifts")))
       (assert (outbound (text-sprite 8 40 120 "by")))
       (assert (outbound (hand-sprite 'hour 7 hour-angle 65)))
       (assert (outbound (simple-sprite 6 95 95 10 10 (circle 5 "solid" "black"))))

       (define (respond-to-drags id scale)
         (define/query-value touching? #f (inbound (touching id)) #t)
         (on #:when (touching?) (message
                                 (inbound (mouse-event 'left-down (mouse-state $mx $my _ _ _))))
             (start-time #f)
             (elapsed-seconds 0)
             (update-displacement! mx my scale)
             (react (stop-when (message (inbound (mouse-event 'left-up _))))
                    (stop-when (message (inbound (mouse-event 'leave _))))
                    (on-stop (start-time (current-inexact-milliseconds)))
                    (on (message (inbound (mouse-event 'motion (mouse-state $mx $my _ _ _))))
                        (update-displacement! mx my scale)))))

       (define (update-displacement! mx my scale)
         (define angle (- 90 (* (/ 180 pi) (atan (- 100 my) (- mx 100)))))
         (define delta0 (fmod (- (* scale angle) (displacement)) 360))
         (define delta (if (<= delta0 -180) (+ delta0 360) delta0))
         (displacement (+ (displacement) delta)))

       (respond-to-drags 'minute 1)
       (respond-to-drags 'hour 12)

       (begin/dataflow
         (define angle (+ (/ (elapsed-seconds) 1000 10) (displacement)))
         (minute-angle angle)
         (hour-angle (/ angle 12)))

       (on (message (inbound (frame-event _ _ _ _)))
           (when (start-time)
             (elapsed-seconds (- (current-inexact-milliseconds) (start-time)))))

       (on (message (inbound (key-event #\q #t _)))
           (assert! (outbound 'stop))))

(module+ main (current-ground-dataspace (2d-dataspace #:label "Syndicate Clock"
                                                      #:width 200
                                                      #:height 200)))
