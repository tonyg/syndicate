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
          (send! (pong))))))

(module+ test
  (verify-actors (Eventually (Message Pong))
    a
    r)

  (verify-actors (Eventually Ping)
    a
    r)

  (verify-actors/fail (Eventually (Message Pong))
    r))
