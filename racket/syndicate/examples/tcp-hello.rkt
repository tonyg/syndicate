#lang syndicate

(require syndicate/protocol/advertise)
(require/activate "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(define server-id (tcp-listener 5999))

(define (spawn-connection-handler c)
  (log-info "spawn-connection-handler ~v" c)
  (define (connection-handler e n)
    (when e (log-info "connection-handler ~v: ~v /// ~v" c e n))
    (match e
      [(? patch/removed?) (quit)]
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
         (patch-seq (sub (advertise (tcp-channel c server-id ?)))
                    (sub (tcp-channel c server-id ?))
                    (pub (tcp-channel server-id c ?)))))

(spawn-demand-matcher (advertise (tcp-channel (?!) server-id ?))
                      (observe (tcp-channel (?!) server-id ?))
		      spawn-connection-handler
		      (lambda (c)
			(log-info "Connection handler ~v decided to exit" c)
			'()))
