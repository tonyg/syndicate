#lang syndicate/actor

(require syndicate/threaded)
(require/activate syndicate/drivers/timer)

(actor
 (react
  (during/actor (observe `(fib ,$n ,_))
                #:actor actor/thread
                (assert `(fib ,n
                              ,(begin
                                 (log-info "Computing fib ~a..." n)
                                 (let f ((n n))
                                   (if (< n 2)
                                       n
                                       (+ (f (- n 1))
                                          (f (- n 2)))))))))))

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

 (react/suspend (done)
  (on (asserted `(fib 36 ,$v) #:meta-level 1)
      (log-info "fib 36 is ~a" v))
  (on (asserted `(fib 38 ,$v) #:meta-level 1)
      (log-info "fib 38 is ~a" v)
      (done))
  (on-stop (log-info "Quitting main"))))
