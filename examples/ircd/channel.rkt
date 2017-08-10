#lang syndicate

(require "protocol.rkt")
(require "message.rkt")

(require/activate syndicate/reload)

(spawn #:name 'channel-factory
       (stop-when-reloaded)
       (during/spawn (ircd-channel-member $Ch _)
         #:name `(ircd-channel ,Ch)
         (field [topic #f])
         (assert (ircd-channel-topic Ch (topic)))

         (on (message (ircd-action $who (irc-message _ "MODE" (list Ch "b") _)))
             (send! (ircd-event who (irc-message server-prefix 368 (list (lookup-nick who) Ch)
                                                 "End of Channel Ban List"))))

         (on (message (ircd-action $who (irc-message _ "MODE" (list Ch) _)))
             (send! (ircd-event who (irc-message server-prefix 324
                                                 (list (lookup-nick who) Ch "+") #f))))

         (on (message (ircd-action _ (irc-message _ "TOPIC" (list Ch) $new-topic)))
             (topic new-topic))))
