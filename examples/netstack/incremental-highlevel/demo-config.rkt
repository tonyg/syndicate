#lang syndicate/actor
;; Demonstration stack configuration for various hosts.

(require racket/match)
(require (only-in mzlib/os gethostname))
(require "configuration.rkt")

(spawn
 (match (gethostname)
   ["stockholm.ccs.neu.edu"
    (assert (host-route (bytes 129 10 115 94) 24 "eth0"))
    (assert (gateway-route (bytes 0 0 0 0) 0 (bytes 129 10 115 1) "eth0"))]
   [other ;; assume a private network
    (define interface
      (match other
        ["skip" "en0"]
        ["leap" "wlp4s0"] ;; wtf
        [_ "wlan0"]))
    (assert (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) interface))
    (assert (host-route (bytes 192 168 1 222) 24 interface))]))
