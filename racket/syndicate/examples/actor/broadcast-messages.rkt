#lang syndicate
;; Demonstrate sending a message to multiple receivers.

(require syndicate/actor)

(struct envelope (destination message) #:prefab)

(actor (forever (on (message (envelope 'alice $message))
                    (log-info "Alice received ~v" message))))

(actor (forever (on (message (envelope 'bob $message))
                    (log-info "Bob received ~v" message))))

(actor
 (log-info "Waiting for Alice and Bob.")
 (until (asserted (observe (envelope 'alice _))))
 (until (asserted (observe (envelope 'bob _))))

 (log-info "Sending a few messages...")
 (send! (envelope 'alice "For Alice's eyes only"))
 (send! (envelope 'bob "Dear Bob, how are you? Kind regards, etc."))
 (send! (envelope ? "Important announcement!"))

 (log-info "Sent all the messages."))
