#lang syndicate/actor
;; Generic relay for WebSockets/TCP/etc-based participation in a network.

(provide spawn-broker-server
         spawn-broker-server-connection
         (struct-out broker-scope)
         (struct-out broker-data))

(require racket/dict)
(require racket/set)
(require racket/match)
(require net/rfc6455)
(require json)
;; (require (except-in "../main.rkt" dataspace assert))
;; (require "../actor.rkt")
(require syndicate/trie)
(require syndicate/patch)
(require syndicate/demand-matcher)
(require syndicate/protocol/advertise)

(require/activate syndicate/drivers/timestate)
(require/activate syndicate/drivers/web)
(require "protocol.rkt")

(define-logger syndicate-broker)

(struct broker-scope (host port path) #:prefab)
(struct broker-data (scope assertion) #:prefab)

(define broker-data-parenthesis (struct-type->parenthesis struct:broker-data))
(define broker-scope-parenthesis (struct-type->parenthesis struct:broker-scope))

;; Depends on timer driver and web driver.
;; Does not, itself, assert a web-virtual-host; the context must do this.
(define (spawn-broker-server port
                             #:hostname [hostname ?]
                             #:path [resource-path-str "/"])
  (actor #:name 'broker:dm
         (on (web-request-get (id req)
                              (web-virtual-host "http" hostname port)
                              ,(string->resource-path resource-path-str))
             (when (equal? (dict-ref (web-request-header-headers req) 'upgrade #f) "websocket")
               (spawn-broker-server-connection id req)))))

(define (spawn-broker-server-connection req-id http-req)
  (actor #:name (list 'broker:connection req-id)

         (on-start (log-syndicate-broker-info "Starting broker connection ~v" req-id))
         (on-stop (log-syndicate-broker-info "Ending broker connection ~v" req-id))
         (on (asserted (web-request-peer-details req-id _ _ $addr $port))
             (log-syndicate-broker-info "Connection ~v is from ~a:~a" req-id addr port))

         (assert (web-response-websocket req-id))
         (stop-when (websocket-connection-closed req-id))

         (define (send-event e)
           (websocket-message-send! req-id (jsexpr->string (lift-json-event e))))

         (define http-resource (web-request-header-resource http-req))
         (define http-vh (web-resource-virtual-host http-resource))
         (define scope (broker-scope (web-virtual-host-name http-vh)
                                     (web-virtual-host-port http-vh)
                                     (resource-path->string (web-resource-path http-resource))))

         (field [ping-time-deadline 0])
         (on (asserted (later-than (ping-time-deadline)))
             (ping-time-deadline (+ (current-inexact-milliseconds) (ping-interval)))
             (send-event 'ping))

         (on (websocket-message-recv req-id $data)
             (match (drop-json-action (string->jsexpr data))
               ['ping (send-event 'pong)]
               ['pong (void)]
               [(? patch? p) (patch! (log-packet req-id 'inbound 'patch (wrap-patch scope p)))]
               [(message body) (send! (log-packet req-id 'inbound 'message (broker-data scope body)))]))

         (on-event
          [(? patch? p) (send-event (log-packet req-id 'outbound 'patch (unwrap-patch scope p)))]
          [(message (broker-data (== scope) body))
           (send-event (message (log-packet req-id 'outbound 'message body)))])))

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
                    (pattern->trie #t (broker-data scope (embedded-trie t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(actor #:name 'broker:vh (assert (web-virtual-host "http" _ 8000)))
(spawn-broker-server 8000)
