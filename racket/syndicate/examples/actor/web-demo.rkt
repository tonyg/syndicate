#lang syndicate/actor
;; Simple demo of web driver. See web-demo.rkt for a more realistic example.

(require/activate syndicate/drivers/timer)
(require/activate syndicate/drivers/web)
(require net/url)

(spawn #:name 'server
       (define vh (web-virtual-host "http" ? 9090))

       (assert vh)

       (on (web-request-incoming (id req) vh _ ("ws" ()))
           (spawn
            (assert (web-response-websocket id))
            (stop-when (websocket-connection-closed id) (log-info "Connection dropped"))
            (stop-when (websocket-message-recv id "quit") (log-info "Received quit command"))
            (on (websocket-message-recv id $str)
                (log-info "Got ~v" str)
                (websocket-message-send! id str))))

       (field [counter 0])
       (on (web-request-get (id req) vh ("foo" ,$path))
           (define req-num (counter))
           (counter (+ (counter) 1))
           (web-respond/xexpr! id
                               `(html
                                 (body
                                  (h1 "Hi there.")
                                  (p ,(format "Your path was ~v, and this is request ~a"
                                              path
                                              req-num)))))))
