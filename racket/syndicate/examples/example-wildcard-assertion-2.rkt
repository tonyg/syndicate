#lang syndicate
;; Demonstrate almost-wildcard assertions.
;; One actor subscribes to everything except inbound assertions - and so initially sees itself.
;; The other advertises everything except subscriptions and inbound/outbound assertions.
;; The first actor's aggregate view of the dataspace then includes everything
;; except inbound assertions.

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
       (patch-seq (sub ?)
                  (unsub (inbound ?))))

(spawn (lambda (e s)
         (printf "Asserter\n")
         (syndicate-pretty-print e)
         (newline)
         #f)
       (void)
       (patch-seq (assert ?)
                  (retract (observe ?))
                  (retract (outbound ?))
                  (retract (inbound ?))))
