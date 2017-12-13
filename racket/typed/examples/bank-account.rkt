#lang typed/syndicate

(define-constructor (account balance)
  #:type-constructor AccountT
  #:with Account (AccountT Int)
  #:with AccountRequest (AccountT ★))

(define-constructor (deposit amount)
  #:type-constructor DepositT
  #:with Deposit (DepositT Int)
  #:with DepositRequest (DepositT ★))

(define-type-alias ds-type
  (U Account
     (Observe AccountRequest)
     (Observe (Observe AccountRequest))
     Deposit
     (Observe DepositRequest)
     (Observe (Observe DepositRequest))))

(dataspace ds-type

           (spawn ds-type
                  (facet _
                         (fields [balance Int 0])
                         (assert (account (ref balance)))
                         (on (asserted (deposit (bind amount Int)))
                             (set! balance (+ (ref balance) amount)))))

           (spawn ds-type
                  (facet _
                         (fields)
                         (on (asserted (account (bind amount Int)))
                             (displayln amount))))

           (spawn ds-type
                  (facet _
                         (fields)
                         (on (asserted (observe (deposit discard)))
                               (facet _
                                      (fields)
                                      (assert (deposit 100))
                                      (assert (deposit -30)))))))

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