#lang syndicate/monolithic
;; Hello-worldish "bank account" example.

(struct account (balance) #:prefab)
(struct deposit (amount) #:prefab)

(define (manager e balance)
  (match-event e
    [(message (deposit amount))
     (transition (+ balance amount)
                 (scn (assertion (account (+ balance amount)))))]))

(define (observer e _)
  (when (scn? e) (for [(balance (project-assertions (scn-trie e) (account (?!))))]
                   (printf "Balance changed to ~a\n" balance))))

(define (updater e _)
  (when (and (scn? e) (trie-non-empty? (scn-trie e)))
    (quit (list (message (deposit +100))
                (message (deposit -30))))))

(actor manager 0 (scn/union (assertion (observe (deposit ?))) (assertion (account 0))))
(actor observer (void) (scn (assertion (observe (account ?)))))
(actor updater (void) (scn (assertion (observe (observe (deposit ?))))))
