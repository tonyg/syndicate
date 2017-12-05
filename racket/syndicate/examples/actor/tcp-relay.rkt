#lang syndicate
;; Simple TCP relay

(require/activate syndicate/drivers/tcp2)

(define (read-tcp-line id)
  (react/suspend (k)
    (on (message (tcp-in-line id $line-bytes))
        (k (bytes->string/utf-8 line-bytes)))))

(spawn #:name 'server
       (during (tcp-connection $id (tcp-listener 5000))
         (assert (tcp-accepted id))
         (on-start (printf "Accepted ~a\n" id))
         (on-stop (printf "Disconnected ~a\n" id))
         (define connection-facet-id (current-facet-id))
         (on-start (send! (tcp-out id #"Please enter the host to connect to: "))
                   (define host (read-tcp-line id))
                   (send! (tcp-out id #"Please enter the port to connect to: "))
                   (define port (string->number (read-tcp-line id)))
                   (define outbound-id (gensym 'outbound-id))
                   (react (assert (tcp-connection outbound-id (tcp-address host port)))
                          (during (tcp-accepted outbound-id)
                            (on-start (printf "Connected ~a => ~a\n" id outbound-id))
                            (on-stop (printf "Disconnected ~a => ~a\n" id outbound-id))
                            (on-stop (stop-facet connection-facet-id))
                            (on (message (tcp-in id $bs))
                                (printf "Relaying ~a -> ~a: ~v\n" id outbound-id bs)
                                (send! (tcp-out outbound-id bs)))
                            (on (message (tcp-in outbound-id $bs))
                                (printf "Relaying ~a <- ~a: ~v\n" id outbound-id bs)
                                (send! (tcp-out id bs))))))))
