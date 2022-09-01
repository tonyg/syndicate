#lang typed/syndicate

(define (fut1)
(lift+define-role va1
(start-facet X
  (field [state Bool #t])
  (assert (tuple (! state))))))

(module+ test
  (verify-actors (Eventually (A (Tuple Bool)))
                 va1)
  )

(define-constructor* (go-true))
(define-constructor* (go-false))

(define (fut2)
  (lift+define-role va2
  (export-roles "va2.rktd"
  (print-role
  (start-facet X
    (field [state Bool #f])
    (assert (tuple (! state)))
    (on (message (go-true))
        (:= state #t))))))

  (lift+define-role gt
  (start-facet Y
    (on (asserted (tuple _))
        (send! (go-true))))))

(module+ test
  (verify-actors (Eventually (A (Tuple True)))
                 va2
                 gt)
  (verify-actors/fail (Always (Not (A (Tuple False))))
    va2
    gt))
