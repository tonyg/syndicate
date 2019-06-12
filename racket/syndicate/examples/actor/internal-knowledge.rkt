#lang syndicate

;; Expected Output:
#|
balance = 0
balance = 5
balance = 0
JEEPERS
know overdraft!
balance = -1
balance = -2
no longer in overdraft
balance = 8
|#

(assertion-struct balance (v))
(message-struct deposit (v))

(spawn
 ;; Internal Events
 (message-struct new-transaction (old new))
 (assertion-struct overdraft ())

 (field [account 0])

 (assert (balance (account)))

 (on (message (deposit $v))
     (define prev (account))
     (account (+ v (account)))
     (realize! (new-transaction prev (account))))

 (on (realize (new-transaction $old $new))
     (when (and (negative? new)
                (not (negative? old)))
       (react
        ;; (this print is to make sure only one of these facets is created)
        (printf "JEEPERS\n")
        (know (overdraft))
        (on (realize (new-transaction $old $new))
            (when (not (negative? new))
              (stop-current-facet))))))

 (on (know (overdraft))
     (printf "know overdraft!\n"))
 (on (forget (overdraft))
     (printf "no longer in overdraft\n")))

(spawn
 (on (asserted (balance $v))
     (printf "balance = ~a\n" v)))

(spawn*
 (send! (deposit 5))
 (flush!)
 (send! (deposit -5))
 (flush!)
 (send! (deposit -1))
 (flush!)
 (send! (deposit -1))
 (flush!)
 (send! (deposit 10)))
