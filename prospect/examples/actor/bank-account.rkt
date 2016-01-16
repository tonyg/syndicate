#lang prospect
;; Hello-worldish "bank account" example.

(require prospect/actor)

(struct account (balance) #:prefab)
(struct alter-balance-by (amount) #:prefab)

(actor-body->spawn-action
 (lambda ()
   (actor (forever #:collect [(balance 0)]
                   (assert (account balance))
                   (on (message (alter-balance-by $amount))
                       (+ balance amount))))

   (actor (forever (on (asserted (account $balance))
                       (printf "Balance changed to ~a\n" balance))))

   (until (asserted (observe (alter-balance-by _))))
   (send! (alter-balance-by +100))
   (send! (alter-balance-by -30))
   ))
