#lang syndicate

(require/activate syndicate/drivers/timestate)

(assertion-struct active ())
(message-struct toggle ())

(spawn* (define (active-state)
          (react (assert (active))
                 (stop-when (message (toggle))
                    (inactive-state))))
        (define (inactive-state)
          (react (stop-when (message (toggle))
                    (active-state))))
        (inactive-state))

(spawn (on (asserted (active)) (printf "Flip-flop is active\n"))
       (on (retracted (active)) (printf "Flip-flop is inactive\n"))

       (field [next-toggle-time (current-inexact-milliseconds)])
       (on (asserted (later-than (next-toggle-time)))
           (send! (toggle))
           (next-toggle-time (+ (next-toggle-time) 1000))))
