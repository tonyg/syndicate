#lang syndicate-monolithic
;; Hello-worldish "bank account" example.

(struct account (balance) #:prefab)
(struct deposit (amount) #:prefab)

(define (manager e balance)
  (match e
    [(message (deposit amount))
     (transition (+ balance amount)
                 (scn (assertion (account (+ balance amount)))))]
    [_ #f]))

(define (observer e _)
  (when (scn? e)
    (for [(balance (project-assertions (scn-trie e) (account (?!))))]
      (printf "Balance changed to ~a\n" balance)))
  #f)

(define (updater e _)
  (if (and (scn? e) (trie-non-empty? (scn-trie e)))
      (quit (list (message (deposit +100))
                  (message (deposit -30))))
      #f))

(spawn manager 0 (scn/union (assertion (observe (deposit ?))) (assertion (account 0))))
(spawn observer (void) (scn (assertion (observe (account ?)))))
(spawn updater (void) (scn (assertion (observe (observe (deposit ?))))))
