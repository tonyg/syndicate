#lang typed/syndicate

(define-constructor* (time [t : Int]))
(define-constructor* (place [p : String]))

(define (friend1)
  (spawn
    (lift+define-role F1
    (start-facet f1
      (printf "friend1: waiting for a time\n")
      (on (asserted (time $t))
          (printf "friend1: suggesting a place\n")
          (start-facet p
            (assert (if #f
                        (place "yours")
                        (place "mine")))))))))

(define (friend2)
  (spawn
    (lift+define-role F2
    (start-facet f2
      (printf "friend2: waiting for a place\n")
      (on (asserted (place $p))
          (printf "friend2: suggesting a time\n")
          (start-facet t
            (assert (if #f
                        (time 0)
                        (time 1)))))))))

#;(run-ground-dataspace (friend1) (friend2))
(module+ test
  ;; these two should fail
  (check-not-deadlock-free F1 F2)
  (check-not-deadlock-free* F1 F2))

(module+ test
  (define-type-alias (DeadlockLTL T)
    (Always (Implies (A (Observe★ T))
                     (Eventually (Or (A T)
                                     (Not (A (Observe★ T))))))))

  #;(verify-actors/fail (And (DeadlockLTL Time)
                           (Eventually (A (Observe★ Time))))
    F1
    F2))

(define-constructor* (house [price : Int]))
(define-constructor* (boat [price : Int]))

(define (richie-rich)
  (spawn
    (lift+define-role RR
    (start-facet wait
      (on (asserted (house $p))
          (stop wait))
      (on (asserted (boat $p))
          (stop wait))))))

(define (house-seller)
  (spawn
    (lift+define-role HS
    (start-facet hs
      (during (observe (house _))
        (assert (house 500000)))))))

(define (boat-seller)
  (spawn
    (lift+define-role BS
    (start-facet bs
      (during (observe (boat _))
        (assert (boat 5000)))))))

(module+ test
  ;; this one should pass
  (check-deadlock-free RR HS)
  (check-deadlock-free* RR HS)
  ;; sanity check
  (check-not-deadlock-free RR)
  (check-not-deadlock-free* RR))
