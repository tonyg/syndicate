#lang typed/syndicate

(define-type-alias ds-type
  (U (Tuple String Int)
     (Observe (Tuple String ★))
     (Observe (Observe (Tuple String ★)))))

(dataspace ds-type

           (spawn ds-type
                  (facet _
                         (fields [balance Int 0])
                         (assert (tuple "balance" (ref balance)))
                         (on (asserted (tuple "deposit" (bind amount Int)))
                             (set! balance (+ (ref balance) amount)))))

           (spawn ds-type
                  (facet _
                         (fields)
                         (on (asserted (tuple "balance" (bind amount Int)))
                             (displayln amount))))

           (spawn ds-type
                  (facet _
                         (fields)
                         (on (asserted (observe (tuple "deposit" discard)))
                               (facet _
                                      (fields)
                                      (assert (tuple "deposit" 100))
                                      (assert (tuple "deposit" -30)))))))

#|
;; Hello-worldish "bank account" example.

(struct account (balance) #:prefab)
(struct deposit (amount) #:prefab)

(spawn (field [balance 0])
       (assert (account (balance)))
       (on (message (deposit $amount))
           (balance (+ (balance) amount))))

(spawn (on (asserted (account $balance))
           (printf "Balance changed to ~a\n" balance)))

(spawn* (until (asserted (observe (deposit _))))
        (send! (deposit +100))
        (send! (deposit -30)))
|#