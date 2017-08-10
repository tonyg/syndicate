#lang syndicate
;; Demonstrate let-event.
;; Should print "Complete.".

(spawn* (let-event [(message 'one)
                    (message 'two)
                    (message 'three)]
                   (send! 'complete)))

(spawn (on-start (send! 'one)
                 (flush!) ;; needed to give the other actor time to
                          ;; become responsive to the next message (!)
                 (send! 'two)
                 (flush!)
                 (send! 'three))
       (stop-when (message 'complete)
         (printf "Complete.\n")))
