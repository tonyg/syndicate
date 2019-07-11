#lang syndicate

(require "../../widgets.rkt")
(require/activate syndicate/drivers/timestate)

;; notes on MF impl:
;;  - reset button doesn't do anything if duration is at 0
;;  - duration is meant to update as slider is moved, not just when released

;; a timer that permits the continuous setting of a new interval, plusanything if duration is at 0
;;  - duration is meant to update as slider is moved, not just when released
;; a gauge and a text field that display the fraction of the elapsed time
;; a reset button that sends the elapsed time back to 0

(define INTERVAL 100)

(define (next-time) (+ (current-milliseconds) INTERVAL))

(spawn

(define frame   (spawn-frame #:label "timer"))
(define elapsed (spawn-gauge #:label "elapsed" #:parent frame #:enabled #f #:range 100))
(define text    (spawn-text-field #:parent frame #:init-value "0" #:label ""))
(define slider  (spawn-slider #:label "duration" #:parent frame #:min-value 0 #:max-value 100))
(define button  (spawn-button #:label "reset" #:parent frame))

(spawn
 (field [*elapsed 0]  ;; INTERVAL/1000 ms accumulated elapsed time
        [*duration 0] ;; INTERVAL/1000 ms set duration interval
        [t (next-time)])

 (define (timer-cb)
   (unless (>= (*elapsed) (*duration))
     (*elapsed (+ (*elapsed) 1))
     (t (next-time))
     (elapsed-cb)))

 (define (elapsed-cb)
   (send! (set-text-field text (format "elapsed ~a" (*elapsed))))
   (unless (zero? (*duration))
     (define r (quotient (* 100 (*elapsed)) (*duration)))
     (send! (set-gauge-value elapsed r))))

 (define (reset-cb)
   (*elapsed 0)
   (timer-cb))

 (define (duration-cb new-duration)
   (unless (= new-duration (*duration))
     (*duration new-duration)
     (timer-cb)))

 (on (asserted (later-than (t)))
     (timer-cb))
 (on (message (button-press button))
     (reset-cb))
 (on (message (slider-update slider $val))
     (duration-cb val))
 (on-start (elapsed-cb)
           (send! (show-frame frame #t))))

)
