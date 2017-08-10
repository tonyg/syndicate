#lang syndicate
;; Demonstrates debouncer-like functionality.

(require syndicate/big-bang)
(require/activate syndicate/big-bang/timestate)

(struct key-down (key) #:prefab)
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
                                 (seal (overlay (text "Click me" 20 "white")
                                                (rectangle 140 50 "solid" "red")))))))

(spawn #:name 'status
       (field [status 'waiting] [clear-time (current-inexact-milliseconds)])
       (on (message (click-event $description))
           (status description)
           (clear-time (+ (current-inexact-milliseconds) 1000)))
       (on (asserted (later-than (clear-time)))
           (status 'waiting))
       (assert (outbound (window 'status 10 60 0
                                 (seal (text (format "~a" (status)) 20 "black"))))))

(spawn #:name 'key-down-listener
       (on (message (inbound (key-event $k _)))
           (react (assert (key-down k))
                  (stop-when (message (inbound (release-event k _)))))))

(spawn #:name 'key-debouncer
       (field [status 'no-key])

       (on (asserted (key-down $k))
           (react (stop-when (retracted (key-down k)))
                  (stop-when-timeout 500 (status `(down ,k)))))
       (on (retracted (key-down $k))
           (react (stop-when (asserted (key-down k)))
                  (stop-when-timeout 500 (status `(up ,k)))))

       (assert (outbound (window 'key-debouncer 10 90 0
                                 (seal (text (format "~a" (status)) 20 "black"))))))

(module+ main
  (current-ground-dataspace
   (big-bang-dataspace #:width 160
                       #:height 120)))
