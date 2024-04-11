#lang typed/syndicate

(define (spawnA)
  (spawn
    (react
      (assert (tuple))
      (on (asserted (tuple))
          (send! (tuple 0))))))

(module+ test
  (verify-actors (Eventually (Tuple Int))
    (spawnA)))
