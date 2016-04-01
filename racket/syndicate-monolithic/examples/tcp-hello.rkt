#lang syndicate-monolithic

(require "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(spawn-tcp-driver)

(define server-id (tcp-listener 5999))

(define (spawn-connection-handler c)
  (log-info "spawn-connection-handler ~v" c)
  (define (connection-handler e n)
    (when e (log-info "connection-handler ~v: ~v /// ~v" c e n))
    (match e
      [(scn (? trie-empty?)) (quit)]
      [(message (tcp-channel src dst #"quit\n"))
       (quit (message (tcp-channel dst src #"OK, then.\n")))]
      [(message (tcp-channel src dst bs))
       (transition n (message (tcp-channel dst src (string->bytes/utf-8
                                                    (format "You said: ~a" bs)))))]
      [_
       (and (< n 5)
            (transition (+ n 1) (message (tcp-channel server-id c (string->bytes/utf-8
                                                                   (format "msg ~v\n" n))))))]))
  (spawn connection-handler
	 0
         (scn/union (subscription (advertise (tcp-channel c server-id ?)))
                    (subscription (tcp-channel c server-id ?))
                    (advertisement (tcp-channel server-id c ?)))))

(spawn-demand-matcher (advertise (tcp-channel (?!) server-id ?))
                      (observe (tcp-channel (?!) server-id ?))
		      spawn-connection-handler
		      (lambda (c)
			(log-info "Connection handler ~v decided to exit" c)
			'()))
