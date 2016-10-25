#lang syndicate/actor

(require/activate syndicate/drivers/timer)

(actor #:name 'main
       (on (message 'trigger)
           (actor* #:name 'asserter
                   (assert! 'up)
                   (send! 'done)))
       (during/actor 'up
                     #:name 'up
                     (on-start (printf "starting\n"))
                     (on-stop (printf "stopping\n"))))

(actor* #:name 'triggerer
        (until (asserted (observe 'trigger)))
        (send! 'trigger)
        (until (message 'done))
        (printf "got done\n")
        (send! (set-timer 'timer 500 'relative))
        (until (message (timer-expired 'timer _))))
