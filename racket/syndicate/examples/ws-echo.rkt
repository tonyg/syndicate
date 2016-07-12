#lang syndicate/actor
;; Websocket server that echoes all it receives

(require/activate syndicate/drivers/websocket)

(define any-client any-websocket-remote-client)
(define tcp-server-id (websocket-local-server 8081 #f))
(define ssl-server-id (websocket-local-server 8084 (websocket-ssl-options "server-cert.pem"
                                                                          "private-key.pem")))

(actor (forever (assert (advertise (observe (websocket-message any-client tcp-server-id _))))
                (on (asserted (advertise (websocket-message ($ c any-client) tcp-server-id _)))
                    (handle-connection tcp-server-id c))))
(actor (forever (assert (advertise (observe (websocket-message any-client ssl-server-id _))))
                (on (asserted (advertise (websocket-message ($ c any-client) ssl-server-id _)))
                    (handle-connection ssl-server-id c))))

(define (handle-connection s c)
  (actor (until (retracted (advertise (websocket-message c s _)))
                (on (asserted (websocket-peer-details s c $la _ $ra _))
                    (log-info "~a: local ~v :: remote ~v" c la ra))
                (on (message (websocket-message c s $body))
                    (log-info "~a: ~v" c body)
                    (send! (websocket-message s c body))))
         (log-info "~a: disconnected" c)))
