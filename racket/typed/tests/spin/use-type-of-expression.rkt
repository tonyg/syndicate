#lang typed/syndicate

(define-constructor* (ping [v : Int]))
(define-constructor* (pong))

(define (spawn-asserter)
  (spawn
    (start-facet a
      (assert (ping 0)))))

(define (spawn-responder)
  (spawn
    (start-facet r
      (on (asserted (ping $x))
          (start-facet go
            (assert (pong)))))))

(module+ test
  (verify-actors (Eventually (A Pong))
    (spawn-asserter)
    (spawn-responder))

  (verify-actors/fail (Eventually (A Pong))
    (start-facet r
      (on (asserted (ping $x))
          (start-facet go
            (assert (pong)))))))
