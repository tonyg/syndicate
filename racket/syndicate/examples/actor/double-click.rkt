#lang syndicate/actor

(require syndicate/big-bang)
(require/activate syndicate/big-bang/timestate)

(struct click-event (description) #:prefab)

(spawn #:name 'button
       (define name 'clickable)

       (define (wait-for-initial-press)
         (react (stop-when (message (inbound (mouse-event _ _ name "button-down")))
                  (send! (click-event 'first-click))
                  (react (stop-when (message (inbound (mouse-event _ _ name "button-down")))
                           (send! (click-event 'double-click))
                           (wait-for-initial-press))
                         (stop-when-timeout 500
                           (send! (click-event 'single-click))
                           (wait-for-initial-press))))))

       (on-start (wait-for-initial-press))

       (assert (outbound (window name 10 10 0
                                 (seal (overlay (text "Click me" 22 "white")
                                                (rectangle 140 50 "solid" "red")))))))

(spawn #:name 'status
       (field [status 'waiting] [clear-time (current-inexact-milliseconds)])
       (on (message (click-event $description))
           (status description)
           (clear-time (+ (current-inexact-milliseconds) 1000)))
       (on (asserted (later-than (clear-time)))
           (status 'waiting))
       (assert (outbound (window 'status 10 60 0
                                 (seal (text (format "~a" (status)) 22 "black"))))))

(module+ main
  (current-ground-dataspace
   (big-bang-dataspace #:width 160
                       #:height 120)))
