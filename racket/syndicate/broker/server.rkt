#lang racket/base
;; Generic relay for WebSockets/TCP/etc-based participation in a network.

(provide spawn-broker-server)

(require racket/set)
(require racket/match)
(require net/rfc6455)
(require (except-in "../main.rkt" dataspace assert))
(require "../actor.rkt")
(require "../trie.rkt")
(require "../demand-matcher.rkt")
(require "../drivers/timer.rkt")
(require "../drivers/websocket.rkt")
(require json)
(require "protocol.rkt")

;; Depends on timer driver and websocket driver running at the given metalevel.
(define (spawn-broker-server port
                             #:ssl-options [ssl-options #f])
  (define any-client (websocket-remote-client ?))
  (define server-id (websocket-local-server port ssl-options))
  (spawn-demand-matcher (advertise (websocket-message (?! any-client) server-id ?))
                        (observe (websocket-message (?! any-client) server-id ?))
                        #:meta-level 1
			(lambda (c) (spawn-connection-handler c server-id))))

(define (spawn-connection-handler c server-id)
  (actor (define (arm-ping-timer!)
           (send! #:meta-level 1 (set-timer c (ping-interval) 'relative)))

         (define (send-event e)
           (send! #:meta-level 1
                  (websocket-message server-id c (jsexpr->string (lift-json-event e)))))

         (arm-ping-timer!)

         (log-info "\nStarting broker connection from ~v" c)
         (until (retracted (advertise (websocket-message c server-id _)) #:meta-level 1)
           (assert (advertise (websocket-message server-id c _)) #:meta-level 1)

           (on (asserted (websocket-peer-details server-id c _ _ $remote-addr $remote-port)
                         #:meta-level 1)
               (log-info "Connection ~v is from ~a:~a" c remote-addr remote-port))

           (on (message (timer-expired c _) #:meta-level 1)
               (arm-ping-timer!)
               (send-event 'ping))

           (on (message (websocket-message c server-id $data) #:meta-level 1)
               (match (drop-json-action (string->jsexpr data))
                 ['ping (send-event 'pong)]
                 ['pong (void)]
                 [(? patch? p) (patch! (log-packet c 'inbound 'patch (patch-without-at-meta p)))]
                 [(message (at-meta _)) (void)]
                 [(message body) (send! (log-packet c 'inbound 'message body))]))

           (on-event
            [(? patch? p) (send-event (log-packet c 'outbound 'patch (clean-patch p)))]
            [(message (at-meta _)) #f]
            [(message body) (send-event (message (log-packet c 'outbound 'message body)))]))
         (log-info "\nEnding broker connection from ~v" c)))

(define (log-packet c direction kind value)
  (log-info "\nBroker: ~v: ~a ~a\n~v" c direction kind value)
  value)

(define stuff-to-prune
  (trie-union-all #:combiner (lambda (v1 v2) (trie-success #t))
                  (list (pattern->trie #t (at-meta ?))
                        (pattern->trie #t (observe (at-meta ?))))))

(define (clean-patch p)
  ;; TODO: this is gross. Linkage shouldn't be visible, and there
  ;; should be some clean way of getting rid of observe(atMeta(...))
  ;; and so on.
  (patch-without-linkage (patch-pruned-by p stuff-to-prune)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (require syndicate/ground)
  (require syndicate/actor)
  (require syndicate/drivers/timer)
  (require syndicate/drivers/websocket)
  (run-ground (spawn-timer-driver)
              (spawn-websocket-driver)
              (dataspace (perform-core-action! (spawn-broker-server 8000))
                         (forever))))
