#lang prospect
;; Hello-worldish "bank account" example.

(struct account (balance) #:prefab)
(struct deposit (amount) #:prefab)

(define (manager e balance)
  (match-event e
    [(message (deposit amount))
     (transition (+ balance amount) (patch-seq (retract (account balance))
                                               (assert (account (+ balance amount)))))]))

(define (observer e _)
  (when (patch? e) (for [(balance (project-assertions (patch-added e) (account (?!))))]
                     (printf "Balance changed to ~a\n" balance))))

(define (updater e _)
  (when (and (patch? e) (trie-non-empty? (patch-added e)))
    (quit (list (message (deposit +100))
                (message (deposit -30))))))

(spawn manager 0 (patch-seq (assert (observe (deposit ?))) (assert (account 0))))
(spawn observer (void) (assert (observe (account ?))))
(spawn updater (void) (assert (observe (observe (deposit ?)))))
