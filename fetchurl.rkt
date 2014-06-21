#lang minimart

(require minimart/demand-matcher)
(require minimart/drivers/timer)
(require (only-in mzlib/os gethostname))
(require "configuration.rkt")
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

(spawn (lambda (e s) #f)
       (void)
       (match (gethostname)
	 ["skip"
	  (gestalt-union (pub (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) "en0"))
			 (pub (host-route (bytes 192 168 1 222) 24 "en0")))]
	 ["hop"
	  (gestalt-union (pub (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) "wlan0"))
			 (pub (host-route (bytes 192 168 1 222) 24 "wlan0")))]
	 ["stockholm.ccs.neu.edu"
	  (gestalt-union (pub (host-route (bytes 129 10 115 94) 24 "eth0"))
			 (pub (host-route (bytes 192 168 56 222) 24 "vboxnet0"))
			 (pub (gateway-route (bytes 0 0 0 0) 0 (bytes 129 10 115 1) "eth0")))]
	 [else
	  (error 'stack-configuration "No setup for hostname ~a" (gethostname))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define local-handle (tcp-handle 'httpclient))
  (define remote-handle (tcp-address "129.10.115.92" 80))

  (spawn (lambda (e seen-peer?)
	   (match e
	     [(routing-update g)
	      (define peer-present? (not (gestalt-empty? g)))
	      (transition (or seen-peer? peer-present?)
			  (if (and (not peer-present?) seen-peer?)
			      (quit)
			      (send (tcp-channel
				     local-handle
				     remote-handle
				     #"GET / HTTP/1.0\r\nHost: stockholm.ccs.neu.edu\r\n\r\n"))))]
	     [(message (tcp-channel _ _ bs) _ _)
	      (printf "----------------------------------------\n~a\n" bs)
	      (printf "----------------------------------------\n")
	      #f]
	     [_ #f]))
	 #f
	 (gestalt-union (pub (tcp-channel local-handle remote-handle ?))
			(sub (tcp-channel remote-handle local-handle ?))
			(sub (tcp-channel remote-handle local-handle ?) #:level 1))))
