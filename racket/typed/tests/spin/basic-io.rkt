#lang typed/syndicate

(define-constructor* (go))
(define-constructor* (ack))

(define (spawn-reactor)
  (spawn
    (start-facet r
      (on (message (go))
          (react
            (assert (ack)))))))

(module+ test
  (verify-actors (Or (Always (Not (M Go)))
                     (Eventually (A Ack)))
    #:IO (Message Go)
    r)
  (verify-actors/fail (Always (Not (M Go)))
    #:IO (Message Go)
    r))
