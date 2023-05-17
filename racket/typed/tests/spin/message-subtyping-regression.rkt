#lang typed/syndicate

(define (spawn-sender)
  (spawn
    (on (asserted (observe (tuple ★)))
        (send! (tuple #t)))))

(define (spawn-receiver)
  (spawn
    (on (message (tuple $b:Bool))
        (react
          (assert (tuple 0))))))

(module+ test
  (verify-actors (Eventually (Tuple Zero))
    (spawn-sender)
    (spawn-receiver)))
