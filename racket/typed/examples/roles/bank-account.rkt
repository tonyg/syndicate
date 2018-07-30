#lang typed/syndicate/roles

(define-constructor (account balance)
  #:type-constructor AccountT
  #:with Account (AccountT Int)
  #:with AccountRequest (AccountT ★/t))

(define-constructor (deposit amount)
  #:type-constructor DepositT
  #:with Deposit (DepositT Int)
  #:with DepositRequest (DepositT ★/t))

(define-type-alias ds-type
  (U Account
     (Observe AccountRequest)
     (Observe (Observe AccountRequest))
     Deposit
     (Observe DepositRequest)
     (Observe (Observe DepositRequest))))

(dataspace ds-type

  (spawn ds-type
    (print-role
    (start-facet account-manager
      (fields [balance Int 0])
      (assert (account (ref balance)))
      (on (asserted (deposit (bind amount Int)))
          (set! balance (+ (ref balance) amount))))))

  (spawn ds-type
    (print-role
    (start-facet observer
      (fields)
      (on (asserted (account (bind amount Int)))
          (displayln amount)))))

  (spawn ds-type
    (print-role
    (start-facet buyer
      (fields)
      (on (asserted (observe (deposit discard)))
          (start-facet deposits
            (fields)
            (assert (deposit 100))
            (assert (deposit -30))))))))