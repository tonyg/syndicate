#lang syndicate/actor

(provide (struct-out later-than)
         stop-when-timeout)

(require/activate syndicate/drivers/timer)

(struct later-than (msecs) #:prefab)

(actor #:name 'drivers/timestate
       (during (observe (later-than $msecs))
         (define timer-id (gensym 'timestate))
         (field [expired? #f])
         (on-start (send! (set-timer timer-id msecs 'absolute)))
         (on (message (timer-expired timer-id _)) (expired? #t))
         (assert #:when (expired?) (later-than msecs))))

(define-syntax-rule (stop-when-timeout relative-msecs body ...)
  (let ((timer-id (gensym 'timeout)))
    (on-start (send! (set-timer timer-id relative-msecs 'relative)))
    (stop-when (message (timer-expired timer-id _)) body ...)))
