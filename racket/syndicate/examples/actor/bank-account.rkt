#lang syndicate
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
