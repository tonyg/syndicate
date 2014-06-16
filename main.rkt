#lang minimart

(require minimart/drivers/timer)
(require "ethernet.rkt")
(require "arp.rkt")
(require "ip.rkt")
(require "tcp.rkt")

(define interface "wlan0")

;;(log-events-and-actions? #t)

(spawn-timer-driver)
(spawn-ethernet-driver)
(spawn-arp-driver interface)
(spawn-ip-driver interface (bytes 192 168 56 222))
(spawn-tcp-driver)

(spawn (lambda (e s)
	 ;; (log-info "SPY: ~v" e)
	 #f)
       (void)
       (gestalt-union (sub ? #:level 5)))
