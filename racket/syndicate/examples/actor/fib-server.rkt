#lang syndicate/actor

(require syndicate/threaded)
(require/activate syndicate/drivers/timer)

(actor
 (react
  (during/actor (observe `(fib ,$n ,_))
                #:actor actor/thread
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
                                       answer)))))))))

(dataspace/thread
 (actor
  (field [tick-count 0])
  (define (arm!)
    (log-info "Tick ~v!" (tick-count))
    (send! (set-timer 'tick 1000 'relative) #:meta-level 1))
  (react (on (message (timer-expired 'tick _) #:meta-level 1)
             (tick-count (+ (tick-count) 1))
             (arm!))
         (on-start (arm!))))

 (field [counter 0])
 (react
  (during `(up ,$n) #:meta-level 1
          (on-start (log-info "up: ~v" n))
          (on-stop (log-info "down: ~v" n)
                   (counter (+ (counter) 1)))))

 (react (stop-when (asserted `(fib 36 ,$v) #:meta-level 1)
                   (log-info "fib 36 is ~a" v)))

 (react (stop-when (asserted `(fib 38 ,$v) #:meta-level 1)
                   (log-info "fib 38 is ~a" v)))

 (until (rising-edge (= (counter) 2)))
 (log-info "Quitting main")
 (until (message (timer-expired 'wait _) #:meta-level 1)
        (on-start (send! (set-timer 'wait 100 'relative) #:meta-level 1))))
