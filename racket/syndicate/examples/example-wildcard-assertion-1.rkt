#lang syndicate
;; Demonstrate wildcard assertions.
;; One actor asserts everything except at-meta assertions (which break
;; the ground VM). It therefore *subscribes* to everything too.

(require syndicate/pretty)

(spawn (lambda (e s)
         (printf "Subscriber - Aggregate\n")
         (syndicate-pretty-print s)
         (printf "Subscriber - Patch\n")
         (syndicate-pretty-print e)
         (newline)
         (if (patch? e)
             (transition (update-interests s e) '())
             #f))
       trie-empty
       (patch-seq (assert ?)
                  (retract (at-meta ?))))
