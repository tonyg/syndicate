#lang typed/syndicate

(define (spawn-asserter)
  (spawn
   (lift+define-role ar
   (start-facet a
     (assert (tuple 0))))))

(module+ test
  (verify-actors (Eventually (A (Tuple Int)))
                 ar)

  (verify-actors/fail (Not (Eventually (A (Tuple Int))))
                      ar)

  (verify-actors/fail (Always (A (Tuple Int)))
                      ar)

  (verify-actors (Eventually (Always (A (Tuple Int))))
                 ar))
