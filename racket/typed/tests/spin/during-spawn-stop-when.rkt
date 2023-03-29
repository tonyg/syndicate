#lang typed/syndicate

(define (A1)
  (spawn
    (start-facet a1
      (during/spawn (tuple)
        (stop-when (message (tuple 5))
                   (react (assert (tuple "hi"))))))))

(define (A2)
  (spawn
    (start-facet a2
      (assert (tuple))
      (on (asserted (observe (tuple 5)))
          (send! (tuple 5))))))

(module+ test
  (verify-actors (Eventually (A (Tuple String)))
    a1
    a2))
