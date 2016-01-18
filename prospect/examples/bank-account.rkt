#lang prospect
;; Hello-worldish "bank account" example.

(struct account (balance) #:prefab)
(struct alter-balance-by (amount) #:prefab)

(spawn (lambda (e balance)
         (match e
           [(message (alter-balance-by amount))
            (define new-balance (+ balance amount))
            (transition new-balance
                        (patch-seq (retract (account balance))
                                   (assert (account new-balance))))]
           [_ #f]))
       0
       (patch-seq (assert (observe (alter-balance-by ?)))
                  (assert (account 0))))

(spawn (lambda (e s)
         (match e
           [(patch added removed)
            (for [(balance (project-assertions added (account (?!))))]
              (printf "Balance changed to ~a\n" balance))
            #f]
           [_ #f]))
       (void)
       (assert (observe (account ?))))

(spawn (lambda (e s)
         (if (and (patch? e) (matcher-non-empty? (patch-added e)))
             (quit (list (message (alter-balance-by +100))
                         (message (alter-balance-by -30))))
             #f))
       (void)
       (assert (observe (observe (alter-balance-by ?)))))
