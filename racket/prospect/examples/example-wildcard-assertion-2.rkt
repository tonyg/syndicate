#lang prospect
;; Demonstrate almost-wildcard assertions.
;; One actor subscribes to everything - and so initially sees itself.
;; The other advertises everything except subscriptions and at-meta assertions.
;; The first actor's aggregate view of the network then includes everything
;; except at-meta assertions.

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
       trie-empty
       (sub ?))

(spawn (lambda (e s)
         (printf "Asserter\n")
         (prospect-pretty-print e)
         (newline)
         #f)
       (void)
       (patch-seq (assert ?)
                  (retract (observe ?))
                  (retract (at-meta ?))))
