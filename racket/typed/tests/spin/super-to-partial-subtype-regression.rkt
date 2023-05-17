#lang typed/syndicate

(define (booler)
  (spawn
    (assert (tuple (ann #t Bool)))))

(define (onT)
  (spawn
    (on (asserted (tuple #t))
        (react
          (assert (tuple "hello"))))))

(module+ test
  (verify-actors/fail (Eventually (Tuple String))
    (booler)
    (onT)))
