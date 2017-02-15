#lang syndicate/actor

(require/activate syndicate/drivers/irc)

(define NICK "syndicatebot")
(define CHAN "##syndicatelang")
(define C (irc-connection "irc.freenode.net" 6667 NICK))

(spawn #:name 'irc-connection-example

       (on (message (irc-inbound C $who NICK $body))
           (log-info "~a said to me: ~a" who body)
           (send! (irc-outbound C who (format "You said: '~a'" body))))

       (on (asserted (irc-presence C NICK CHAN))
           (send! (irc-outbound C CHAN "Hello, everybody!")))

       (during (irc-presence C $who CHAN)
         (on-start (log-info "~a joins ~a" who CHAN))
         (on-stop (log-info "~a leaves ~a" who CHAN)))

       (on (message (irc-inbound C $who CHAN $body))
           (log-info "~a says: ~a" who body)
           (when (not (equal? who NICK))
             (send! (irc-outbound C CHAN (format "Hey, ~a said '~a'" who body))))))
