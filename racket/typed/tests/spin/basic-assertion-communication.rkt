#lang typed/syndicate

(define-constructor* (ping [v : Int]))
(define-constructor* (pong))

(define (spawn-asserter)
  (spawn
   (lift+define-role ar
   (start-facet a
     (assert (ping 0))))))

(define (spawn-responder)
  (spawn
   (lift+define-role rr
   (start-facet r
     (on (asserted (ping $x))
         (start-facet go
           (assert (pong))))))))

(module+ test
  (verify-actors (Eventually (A Pong))
                 ar
                 rr)

  (verify-actors/fail (Eventually (A Pong))
                      rr))
