#lang syndicate
;; Demonstrate almost-wildcard assertions.
;; One actor subscribes to everything - and so initially sees itself.
;; The other advertises everything except subscriptions and at-meta assertions.
;; The first actor's aggregate view of the network then includes everything
;; except at-meta assertions.

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
       (sub ?))

(spawn (lambda (e s)
         (printf "Asserter\n")
         (syndicate-pretty-print e)
         (newline)
         #f)
       (void)
       (patch-seq (assert ?)
                  (retract (observe ?))
                  (retract (at-meta ?))))
