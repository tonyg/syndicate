#lang syndicate/actor

(provide (struct-out ircd-listener)
         (struct-out ircd-motd)

         (struct-out ircd-connection-info)
         (struct-out ircd-channel-member)
         (struct-out ircd-channel-topic)

         (struct-out ircd-action)
         (struct-out ircd-event)

         lookup-nick)

;; A Connection is a TcpAddress

(struct ircd-listener (port) #:prefab) ;; assertion
(struct ircd-motd (lines) #:prefab) ;; assertion

(struct ircd-connection-info (conn nick user) #:prefab) ;;assertion
(struct ircd-channel-member (channel conn) #:prefab) ;; assertion
(struct ircd-channel-topic (channel topic) #:prefab) ;; assertion

(struct ircd-action (conn message) #:prefab) ;; message
(struct ircd-event (conn message) #:prefab) ;; message

;;---------------------------------------------------------------------------

(define (lookup-nick conn)
  (immediate-query [query-value #f (ircd-connection-info conn $N _) N]))
