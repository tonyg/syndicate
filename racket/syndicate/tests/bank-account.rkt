#lang syndicate/test

(spawn (field [balance 0])
       (assert (list "account" (balance)))
       (on (message (list "deposit" $amount))
           (balance (+ (balance) amount))))

(spawn (on (asserted (list "account" $balance))
           (printf "Balance changed to ~a\n" balance))
       (stop-when (asserted (list "account" 70))
                  (printf "bye\n"))
       (on-stop (printf "good.\n")))

(spawn (stop-when (asserted (observe (list "deposit" _)))
                  (send! (list "deposit" +100))
                  (send! (list "deposit" -30))))

(trace (assertion-added '("account" 0))
       (and (assertion-added '("account" 100))
            (assertion-removed '("account" 0)))
       (and (assertion-added '("account" 70))
            (assertion-removed '("account" 100))))