#lang syndicate/actor
;; Hello-worldish "bank account" example.

(struct account (balance) #:prefab)
(struct deposit (amount) #:prefab)

(actor (field [balance 0])
       (assert (account (balance)))
       (on (message (deposit $amount))
           (balance (+ (balance) amount))))

(actor (on (asserted (account $balance))
           (printf "Balance changed to ~a\n" balance)))

(actor* (until (asserted (observe (deposit _))))
        (send! (deposit +100))
        (send! (deposit -30)))
