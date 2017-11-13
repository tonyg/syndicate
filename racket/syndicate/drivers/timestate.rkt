#lang syndicate

(provide (struct-out later-than)
         on-timeout
         stop-when-timeout
         sleep)

(require/activate syndicate/drivers/timer)

(struct later-than (msecs) #:prefab)

(spawn #:name 'drivers/timestate
       (during (observe (later-than $msecs))
         (define timer-id (gensym 'timestate))
         (on-start (send! (set-timer timer-id msecs 'absolute)))
         (on (message (timer-expired timer-id _))
             (react (assert (later-than msecs))))))

(define-syntax-rule (on-timeout relative-msecs body ...)
  (let ((timer-id (gensym 'timeout)))
    (on-start (send! (set-timer timer-id relative-msecs 'relative)))
    (on (message (timer-expired timer-id _)) body ...)))

(define-syntax-rule (stop-when-timeout relative-msecs body ...)
  (on-timeout relative-msecs (stop-current-facet body ...)))

(define (sleep sec)
  (define timer-id (gensym 'sleep))
  (until (message (timer-expired timer-id _))
    (on-start (send! (set-timer timer-id (* sec 1000.0) 'relative)))))
