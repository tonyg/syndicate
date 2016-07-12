#lang syndicate/actor
;; Demonstration stack configuration for various hosts.

(require racket/match)
(require (only-in mzlib/os gethostname))
(require "configuration.rkt")

(actor
 (react
  (match (gethostname)
    ["skip"
     (assert (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) "en0"))
     (assert (host-route (bytes 192 168 1 222) 24 "en0"))]
    [(or "hop" "walk")
     (assert (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) "wlan0"))
     (assert (host-route (bytes 192 168 1 222) 24 "wlan0"))]
    ["stockholm.ccs.neu.edu"
     (assert (host-route (bytes 129 10 115 94) 24 "eth0"))
     (assert (gateway-route (bytes 0 0 0 0) 0 (bytes 129 10 115 1) "eth0"))]
    [other
     (error 'demo-config "No setup for hostname ~a" other)])))
