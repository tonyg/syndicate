#lang syndicate/actor
;; Generic relay for WebSockets/TCP/etc-based participation in a network.

(provide spawn-broker-server
         (struct-out broker-scope)
         (struct-out broker-data))

(require racket/set)
(require racket/match)
(require net/rfc6455)
(require (except-in "../main.rkt" dataspace assert))
(require "../actor.rkt")
(require "../trie.rkt")
(require "../patch.rkt")
(require "../demand-matcher.rkt")
(require "../drivers/timer.rkt")
(require "../drivers/websocket.rkt")
(require json)
(require "protocol.rkt")

(define-logger syndicate-broker)

(struct broker-scope (host port path) #:prefab)
(struct broker-data (scope assertion) #:prefab)

(define broker-data-parenthesis (struct-type->parenthesis struct:broker-data))
(define broker-scope-parenthesis (struct-type->parenthesis struct:broker-scope))

;; Depends on timer driver and websocket driver running at the given metalevel.
(define (spawn-broker-server port
                             #:ssl-options [ssl-options #f])
  (define any-client any-websocket-remote-client)
  (define server-id (websocket-local-server port ssl-options))
  (spawn-demand-matcher (advertise (websocket-message (?! any-client) server-id ?))
                        (observe (websocket-message (?! any-client) server-id ?))
                        #:meta-level 1
			(lambda (c) (spawn-connection-handler c server-id))))

(define (spawn-connection-handler c server-id)
  (actor (define scope (broker-scope (websocket-remote-client-request-host c)
                                     (websocket-remote-client-request-port c)
                                     (websocket-remote-client-request-path c)))

         (define (arm-ping-timer!)
           (send! #:meta-level 1 (set-timer c (ping-interval) 'relative)))

         (define (send-event e)
           (send! #:meta-level 1
                  (websocket-message server-id c (jsexpr->string (lift-json-event e)))))

         (arm-ping-timer!)

         (log-syndicate-broker-info "Starting broker connection from ~v" c)
         (until (retracted (advertise (websocket-message c server-id _)) #:meta-level 1)
           (assert (advertise (websocket-message server-id c _)) #:meta-level 1)

           (on (asserted (websocket-peer-details server-id c _ _ $remote-addr $remote-port)
                         #:meta-level 1)
               (log-syndicate-broker-info "Connection ~v is from ~a:~a" c remote-addr remote-port))

           (on (message (timer-expired c _) #:meta-level 1)
               (arm-ping-timer!)
               (send-event 'ping))

           (on (message (websocket-message c server-id $data) #:meta-level 1)
               (match (drop-json-action (string->jsexpr data))
                 ['ping (send-event 'pong)]
                 ['pong (void)]
                 [(? patch? p) (patch! (log-packet c 'inbound 'patch (wrap-patch scope p)))]
                 [(message body) (send! (log-packet c 'inbound 'message (broker-data scope body)))]))

           (on-event
            [(? patch? p) (send-event (log-packet c 'outbound 'patch (unwrap-patch scope p)))]
            [(message (broker-data (== scope) body))
             (send-event (message (log-packet c 'outbound 'message body)))]))
         (log-syndicate-broker-info "Ending broker connection from ~v" c)))

(define (log-packet c direction kind value)
  (log-syndicate-broker-debug "Broker: ~v: ~a ~a\n~v" c direction kind value)
  value)

(define (unwrap-patch scope p)
  (match-define (patch added removed) p)
  (patch (unwrap-trie scope added) (unwrap-trie scope removed)))

(define (unwrap-trie scope t)
  (if (trie-empty? t)
      t
      (let ((observations (trie-step t observe-parenthesis)))
        (trie-union (trie-prepend observe-parenthesis (unwrap-trie scope observations))
                    (trie-step* t (list broker-data-parenthesis
                                        broker-scope-parenthesis
                                        (broker-scope-host scope)
                                        (broker-scope-port scope)
                                        (broker-scope-path scope)))))))

(define (wrap-patch scope p)
  (match-define (patch added removed) p)
  (patch (wrap-trie scope added) (wrap-trie scope removed)))

(define (wrap-trie scope t)
  (if (trie-empty? t)
      t
      (let ((observations (trie-step t observe-parenthesis)))
        (trie-union (trie-prepend observe-parenthesis (wrap-trie scope observations))
                    (wrap-trie* scope t)))))

(define (wrap-trie* scope t)
  (pattern->trie #t (broker-data scope (embedded-trie t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require/activate syndicate/drivers/timer)
(require/activate syndicate/drivers/websocket)

(let ((ssl-options
       (match (current-command-line-arguments)
         [(vector c p) (websocket-ssl-options c p)]
         [_ #f])))
  (dataspace (schedule-action! (spawn-broker-server 8000))
             (when ssl-options
               (schedule-action! (spawn-broker-server 8443 #:ssl-options ssl-options)))
             (forever)))
