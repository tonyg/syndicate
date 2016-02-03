#lang prospect
;; Demonstrate wildcard assertions.
;; One actor asserts everything except at-meta assertions (which break
;; the ground VM). It therefore *subscribes* to everything too.

(require prospect/pretty)

(spawn (lambda (e s)
         (printf "Subscriber - Aggregate\n")
         (prospect-pretty-print s)
         (printf "Subscriber - Patch\n")
         (prospect-pretty-print e)
         (newline)
         (if (patch? e)
             (transition (update-interests s e) '())
             #f))
       (trie-empty)
       (patch-seq (assert ?)
                  (retract (at-meta ?))))
