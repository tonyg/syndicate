#lang syndicate-monolithic

(require "../drivers/websocket.rkt")
(require "../demand-matcher.rkt")

(spawn-websocket-driver)

(define any-client (websocket-remote-client ?))
(define server-id (websocket-local-server 8081 #f))

(define (spawn-connection-handler c)
  (log-info "spawn-connection-handler ~v" c)
  (define (connection-handler e n)
    (when e (log-info "connection-handler ~v: ~v /// ~v" c e n))
    (match e
      [(scn (? trie-empty?)) (quit)]
      [_
       (if (< n 20)
	   (transition (+ n 1) (message (websocket-message server-id c (format "msg ~v" n))))
	   #f)]))
  (spawn connection-handler
	 0
         (scn/union (subscription (advertise (websocket-message c server-id ?)))
                    (subscription (websocket-message c server-id ?))
                    (advertisement (websocket-message server-id c ?)))))

(spawn-demand-matcher (advertise (websocket-message (?! any-client) server-id ?))
                      (observe (websocket-message (?! any-client) server-id ?))
		      spawn-connection-handler
		      (lambda (c)
			(log-info "Connection handler ~v decided to exit" c)
			'()))
