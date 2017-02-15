#lang syndicate/actor
;; Shows the difference between a supervised exit and a supervised crash.

(require/activate syndicate/supervise)
(require/activate syndicate/drivers/timestate)

(supervise
 #:name 'ward-supervisor
 (spawn #:name 'ward
        (on-start (log-info "Starting ward"))
        (on-stop (log-info "Stopping ward"))
        (on (message 'crash)
            (log-info "Crashing")
            (error 'ward "Eep!"))
        (stop-when (message 'quit)
                   (log-info "Bye!"))))

(define (monitor-interest-in thing)
  (spawn #:name (list 'monitor-interest-in thing)
         (during (observe thing)
           (on-start (log-info "Interest in ~v appeared" thing))
           (on-stop (log-info "Interest in ~v disappeared" thing)))))

(monitor-interest-in 'crash)
(monitor-interest-in 'quit)

(spawn* #:name 'main
        (sleep 1)
        (send! 'crash)
        (sleep 1)
        (send! 'quit)
        (sleep 1))
