#lang syndicate/actor

(require/activate syndicate/drivers/timer)
(require/activate syndicate/drivers/udp)
(require racket/random file/sha1)

;; IANA offers guidelines for choosing multicast addresses [1].
;;
;; Reasonable candidates for local experimentation include:
;;  - 224.0.1.20, "any private experiment"
;;  - 233.252.0.0 - 233.252.0.255, "MCAST-TEST-NET", for examples and documentation (only)
;;
;; For production and semi-production use, registering an address may
;; be an option; failing that, the Administratively Scoped Block
;; (239/8; see RFC 2365) may be used:
;;  - 239.255.0.0 - 239.255.255.255, "IPv4 Local Scope"
;;  - 239.192.0.0 - 239.195.255.255, "Organization Local Scope"
;;
;; [1] http://www.iana.org/assignments/multicast-addresses/

(define group-address "233.252.0.101") ;; falls within MCAST-TEST-NET
(define group-port 5999) ;; make sure your firewall is open to UDP on this port

(spawn (define me (bytes->hex-string (crypto-random-bytes 8)))
       (define h (udp-listener group-port))

       (define (rearm!) (send! (set-timer h 1000 'relative)))

       (on-start (rearm!))

       (assert (udp-multicast-group-member h group-address #f))
       (assert (udp-multicast-loopback h #t))
       (on (message (udp-packet $source h $body))
           (printf "~a: ~a\n" source body))
       (on (message (timer-expired h $now))
           (rearm!)
           (send! (udp-packet h
                              (udp-remote-address group-address group-port)
                              (string->bytes/utf-8 (format "~a ~a" me now))))))
