#lang syndicate

(require syndicate/actor)
(require syndicate/drivers/timer)
(require syndicate/drivers/udp)
(require racket/random file/sha1)

(spawn-timer-driver)
(spawn-udp-driver)

(actor (define me (bytes->hex-string (crypto-random-bytes 8)))
       (define h (udp-listener 5999))

       (define (rearm!) (send! (set-timer h 1000 'relative)))

       (rearm!)

       (forever
        (assert (udp-multicast-group-member h "224.0.0.251" #f))
        (assert (udp-multicast-loopback h #t))
        (on (message (udp-packet $source h $body))
            (printf "~a: ~a\n" source body))
        (on (message (timer-expired h $now))
            (rearm!)
            (send! (udp-packet h
                               (udp-remote-address "224.0.0.251" 5999)
                               (string->bytes/utf-8 (format "~a ~a" me now)))))))
