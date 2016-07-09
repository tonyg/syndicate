#lang syndicate

(require syndicate/actor)
(require syndicate/drivers/timer)

(spawn-timer-driver)

(define (sleep sec)
  (define timer-id (gensym 'sleep))
  (until (message (timer-expired timer-id _))
         (on-start (send! (set-timer timer-id (* sec 1000.0) 'relative)))))

(define (chain-step n)
  (printf "chain-step ~v\n" n)
  (actor (sleep 1)
         (if (< n 5)
             (chain-step (+ n 1))
             (printf "done.\n"))))

(chain-step 0)
