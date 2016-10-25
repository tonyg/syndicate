#lang syndicate/actor
;;
;; Test case for a problem written up on 25 Oct 2016 in my research
;; journal.
;;
;; When the problem exists, we see only "starting", and not
;; "stopping", because by the time the spawn action producing the 'up
;; actor is processed, 'up has already been retracted (by the
;; termination of 'asserter).
;;
;; This is an expectation/operation mismatch.
;;
;; The approach I've chosen is to *label* each instance of a
;; `during/actor` using the new syndicate/protocol/instance, and to
;; change the lifetime-control protocol between the spawner and
;; spawnee to match.
;;
;; PROBLEM OUTPUT:
;;
;; got done
;; starting
;;
;; EXPECTED OUTPUT:
;;
;; got done
;; starting
;; stopping
;;

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
