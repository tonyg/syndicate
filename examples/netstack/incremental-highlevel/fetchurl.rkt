#lang syndicate

(require/activate syndicate/drivers/timer)
(require/activate "ethernet.rkt")
(require/activate "arp.rkt")
(require/activate "ip.rkt")
(require/activate "tcp.rkt")
(require/activate "udp.rkt")
(require/activate "demo-config.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define local-handle (tcp-handle 'httpclient))
  (define remote-handle (tcp-address "81.4.107.66" 80))

  (actor (assert (advertise (tcp-channel local-handle remote-handle _)))
         (on (asserted (advertise (tcp-channel remote-handle local-handle _)))
             (send! (tcp-channel local-handle
                                 remote-handle
                                 #"GET / HTTP/1.0\r\nHost: leastfixedpoint.com\r\n\r\n")))
         (stop-when (retracted (advertise (tcp-channel remote-handle local-handle _)))
                    (printf "URL fetcher exiting.\n"))
         (on (message (tcp-channel remote-handle local-handle $bs))
             (printf "----------------------------------------\n~a\n" bs)
             (printf "----------------------------------------\n"))))
