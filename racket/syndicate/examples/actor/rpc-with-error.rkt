#lang syndicate

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/timestate)

(assertion-struct function (argument result))

(spawn #:name 'division-server
       (during/spawn (observe (function `(divide ,$n ,$d) _))
         (printf "S: Preparing to compute ~a/~a\n" n d)
         (assert (advertise (function `(divide ,n ,d) _)))
         (on-stop (printf "S: Request of ~a/~a is finished.\n" n d))
         (on-start (flush!)
                   (printf "S: Computing ~a/~a\n" n d)
                   (react (assert (function `(divide ,n ,d) (/ n d)))
                          (on-start (printf "S: Computed ~a/~a\n" n d))))))

(define (invert d #:on-answer [ks void] #:on-error [kf void])
  (define req `(divide 1 ,d))
  (printf "C: Requesting ~a\n" req)
  (on (asserted (advertise (function req _)))
      (printf "C: Answer in progress!\n"))
  (stop-when (retracted (advertise (function req _)))
    (printf "C: No answer was supplied!\n")
    (kf))
  (stop-when (asserted (function req $answer))
    (printf "C: The answer is: ~a\n" answer)
    (ks answer)))

(define (pause)
  (sleep 0.5)
  (printf "C: Continuing after pause.\n"))

(spawn (invert 2
               #:on-answer
               (lambda (_)
                 (pause)
                 (react (invert 2
                                #:on-answer
                                (lambda (_)
                                  (pause)
                                  (react (invert 0
                                                 #:on-error
                                                 (lambda ()
                                                   (printf "C: Done!\n"))))))))))

(spawn (on (asserted (observe (function $req _)))
           (printf "X: Noticed request ~a\n" req)
           (react (assert (observe (function req _)))
                  (stop-when (retracted (advertise (function req _)))
                    (printf "X: Removing cache entry for ~a because of error\n" req))
                  (stop-when-timeout 750
                    (printf "X: Timed out cache entry for ~a\n" req)))))

;; This one takes responsibility itself. It could also signal to the
;; server that it has taken responsibility.
;;
;; (spawn (on (asserted (observe (function $req _)))
;;            (printf "X: Noticed request ~a\n" req)
;;            (react (on (asserted (function req $rep))
;;                       (printf "X: Got reply ~a to request ~a\n" rep req)
;;                       (react (assert (function req rep))))
;;                   (stop-when-timeout 750
;;                     (printf "X: Timed out cache entry for ~a\n" req)))))
