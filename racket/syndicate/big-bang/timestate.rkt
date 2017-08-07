#lang syndicate/actor
;; Big-bang specific timestate implementation with compatible protocol.

(provide (struct-out later-than)
         stop-when-timeout
         sleep)

(require syndicate/big-bang)
(require (only-in syndicate/drivers/timestate
                  later-than later-than? struct:later-than later-than-msecs))

(define-syntax-rule (stop-when-timeout relative-msecs body ...)
  (let ((deadline (+ (current-inexact-milliseconds) relative-msecs)))
    (stop-when (asserted (later-than deadline)) body ...)))

(define (sleep sec)
  (let ((deadline (+ (current-inexact-milliseconds) (* sec 1000.0))))
    (until (asserted (later-than deadline)))))

(spawn #:name 'big-bang-timestate-driver
       (during (observe (later-than $deadline))
         (field [ready? #f])
         (assert #:when (ready?) (later-than deadline))
         (on (message (inbound (tick-event)))
             (ready? (>= (current-inexact-milliseconds) deadline)))))
