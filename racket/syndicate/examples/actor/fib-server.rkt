#lang syndicate

(require syndicate/threaded)
(require/activate syndicate/drivers/timer)

(spawn
 (during/spawn (observe `(fib ,$n ,_))
               #:spawn spawn/thread
               (on-start (log-info "Computing fib ~a..." n))
               (on-stop (log-info "Leaving fib ~a" n))
               (assert `(up ,n))
               (on-start
                (flush!)
                (react
                 (assert `(fib ,n
                               ,(let ((answer
                                       (let f ((n n))
                                         (if (< n 2)
                                             n
                                             (+ (f (- n 1))
                                                (f (- n 2)))))))
                                  (if (= n 36)
                                      (error 'fib "Deliberate, hardcoded failure for n=36")
                                      answer))))))))

(dataspace/thread
 (spawn
  (field [tick-count 0])
  (define (arm!)
    (log-info "Tick ~v!" (tick-count))
    (send! (outbound (set-timer 'tick 1000 'relative))))
  (on (message (inbound (timer-expired 'tick _)))
      (tick-count (+ (tick-count) 1))
      (arm!))
  (on-start (arm!)))

 (field [counter 0])
 (react
  (during (inbound `(up ,$n))
          (on-start (log-info "up: ~v" n))
          (on-stop (log-info "down: ~v" n)
                   (counter (+ (counter) 1)))))

 (react (stop-when (asserted (inbound `(fib 36 ,$v)))
                   (log-info "fib 36 is ~a" v)))

 (react (stop-when (asserted (inbound `(fib 38 ,$v)))
                   (log-info "fib 38 is ~a" v)))

 (until (rising-edge (= (counter) 2)))
 (log-info "Quitting main")
 (until (message (inbound (timer-expired 'wait _)))
        (on-start (send! (outbound (set-timer 'wait 100 'relative)))))
 (quit-dataspace!))
