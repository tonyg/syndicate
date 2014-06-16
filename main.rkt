#lang minimart

(require minimart/drivers/timer)
(require "ethernet.rkt")
(require "arp.rkt")
(require "ip.rkt")

(define interface "wlan0")

;;(log-events-and-actions? #t)

(spawn-timer-driver)
(spawn-ethernet-driver)
(spawn-arp-driver interface)
(spawn-ip-driver interface (bytes 192 168 1 222))

(spawn (lambda (e s)
	 ;; (log-info "SPY: ~v" e)
	 #f)
       (void)
       (gestalt-union (sub ? #:level 5)))
