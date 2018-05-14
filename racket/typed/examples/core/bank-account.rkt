#lang typed/syndicate/core

(define-constructor (account balance)
  #:type-constructor AccountT
  #:with Account (AccountT Int))

(define-constructor (transaction id amount)
  #:type-constructor TransactionT
  #:with Transaction (TransactionT Int Int))

(define-type-alias τc
  (U Account
     Transaction
     (Observe (AccountT ★/t))
     (Observe (TransactionT ★/t ★/t))))

(define account-manager
  (actor τc
    (lambda ([e : (Event τc)]
             [b : Int])
      (let ([new-balance
             (for/fold [balance b]
                       [txn (project [(transaction discard (bind v Int)) (patch-added e)] v)]
               (+ balance txn))])
        (transition new-balance
                    (list (patch (make-assertion-set (account new-balance))
                                 (make-assertion-set (account ★)))))))
    0
    (make-assertion-set (account 0)
                        (observe (transaction ★ ★)))))

(define (make-transaction [id : Int] [amount : Int] → (Actor Transaction))
  (actor Transaction
   (lambda ([e : (Event (U))]
            [s : ★/t])
     idle)
   #f
   (make-assertion-set (transaction id amount))))

(define client
  (actor τc
    (lambda ([e : (Event τc)]
             [s : ★/t])
      (quit (list (make-transaction 0 100)
                  (make-transaction 1 -70))))
    #f
    (make-assertion-set (observe (account ★)))))

(define observer
  (actor τc
    (lambda ([e : (Event τc)]
             [s : ★/t])
      (project [(account (bind value Int)) (patch-added e)]
        (displayln value))
      idle)
    #f
    (make-assertion-set (observe (account ★)))))

(dataspace τc
  (list
   account-manager
   observer
   client))