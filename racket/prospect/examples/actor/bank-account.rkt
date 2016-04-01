#lang prospect
;; Hello-worldish "bank account" example.

(require prospect/actor)

(struct account (balance) #:prefab)
(struct deposit (amount) #:prefab)

(actor (forever #:collect [(balance 0)]
                (assert (account balance))
                (on (message (deposit $amount))
                    (+ balance amount))))

(actor (forever (on (asserted (account $balance))
                    (printf "Balance changed to ~a\n" balance))))

(actor (until (asserted (observe (deposit _))))
       (send! (deposit +100))
       (send! (deposit -30)))
