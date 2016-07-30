#lang syndicate/actor
;; Websocket echo client
;; racket ws-echo-client.rkt ws://localhost:8081/
;; racket ws-echo-client.rkt wss://localhost:8084/

(require/activate syndicate/drivers/websocket)
(require syndicate/protocol/advertise)
(require racket/port)

(define url
  (match (current-command-line-arguments)
    [(vector url) url]
    [(vector) "http://localhost:8081/ws-echo"]))

(define c (websocket-local-client (gensym 'c)))
(define s (websocket-remote-server url))

(let ((e (read-bytes-line-evt (current-input-port) 'any)))
  (define reader-count 0)
  (define (generate-reader-id)
    (begin0 reader-count
      (set! reader-count (+ reader-count 1))))
  (actor (react (assert (advertise (websocket-message c s _)))
                (on (asserted (websocket-peer-details c s $la _ $ra _))
                    (log-info "~a: local ~v :: remote ~v" c la ra))
                (on (message (inbound (external-event e (list (? bytes? $bs)))))
                    (send! (websocket-message c s bs)))
                (on (message (websocket-message s c $bs))
                    (printf "(From server: ~v)\n" bs))
                (stop-when (message (inbound (external-event e (list (? eof-object? _)))))
                           (printf "Local EOF. Terminating.\n"))
                (stop-when (retracted (advertise (websocket-message s c _)))
                           (printf "Server disconnected.\n")))))
