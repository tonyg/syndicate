#lang syndicate-monolithic

(require "../drivers/udp.rkt")

(spawn-udp-driver)

(spawn (lambda (e s)
         (match e
           [(message (udp-packet src dst #"quit\n"))
            (log-info "Got quit request")
            (quit (message (udp-packet dst src #"Goodbye!\n")))]
           [(message (udp-packet src dst body))
            (log-info "Got packet from ~v: ~v" src body)
            (define reply (string->bytes/utf-8 (format "You said: ~a" body)))
            (transition s (message (udp-packet dst src reply)))]
           [_ #f]))
       (void)
       (scn (subscription (udp-packet ? (udp-listener 5999) ?))))
