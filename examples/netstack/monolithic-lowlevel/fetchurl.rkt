#lang syndicate/monolithic

(require syndicate/drivers/timer)
(require "demo-config.rkt")
(require "ethernet.rkt")
(require "arp.rkt")
(require "ip.rkt")
(require "tcp.rkt")
(require "udp.rkt")

;;(log-events-and-actions? #t)

(spawn-timer-driver)
(spawn-ethernet-driver)
(spawn-arp-driver)
(spawn-ip-driver)
(spawn-tcp-driver)
(spawn-udp-driver)
(spawn-demo-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define local-handle (tcp-handle 'httpclient))
  (define remote-handle (tcp-address "129.10.115.92" 80))

  (spawn (lambda (e seen-peer?)
	   (match e
	     [(scn g)
	      (define peer-present? (trie-non-empty? g))
              (if (and (not peer-present?) seen-peer?)
                  (begin (printf "URL fetcher exiting.\n")
                         (quit))
                  (transition (or seen-peer? peer-present?)
			      (message
                               (tcp-channel
                                local-handle
                                remote-handle
                                #"GET / HTTP/1.0\r\nHost: stockholm.ccs.neu.edu\r\n\r\n"))))]
	     [(message (tcp-channel _ _ bs))
	      (printf "----------------------------------------\n~a\n" bs)
	      (printf "----------------------------------------\n")
	      #f]
	     [_ #f]))
	 #f
	 (scn/union (advertisement (tcp-channel local-handle remote-handle ?))
                    (subscription (tcp-channel remote-handle local-handle ?))
                    (subscription (advertise (tcp-channel remote-handle local-handle ?))))))
