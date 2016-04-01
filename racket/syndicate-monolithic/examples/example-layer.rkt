#lang syndicate-monolithic
;; Check that nested-world assertions are properly retracted.
;; Should print two "Got SCN:" tries - one nonempty, and one empty.

#;(spawn (lambda (e s)
         (match e
           [(message 'die) (quit)]
           [_ #f]))
       (void)
       (scn/union
        (subscription 'die)
        (subscription (observe 'die))))

(spawn-network
 (spawn (lambda (e s)
          (match e
            [(message (at-meta 'die)) (quit)]
            [_ #f]))
        (void)
        (scn/union
         (subscription 'die #:meta-level 1)
         (subscription (observe 'die) #:meta-level 1))))

(spawn (lambda (e s)
         (match e
           [(scn g)
            (printf "Got SCN:\n")
            (pretty-print-trie g)
            (transition s (if (trie-non-empty? g)
                              (message 'die)
                              '()))]
           [_ #f]))
       (void)
       (scn (subscription (observe 'die))))
