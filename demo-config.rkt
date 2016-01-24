#lang racket/base
;; Demonstration stack configuration for various hosts.

(require racket/match)
(require prospect-monolithic)
(require (only-in mzlib/os gethostname))
(require "configuration.rkt")

(provide spawn-demo-config)

(define (spawn-demo-config)
  (spawn (lambda (e s) #f)
         (void)
         (match (gethostname)
           ["skip"
            (scn/union (assertion (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) "en0"))
                       (assertion (host-route (bytes 192 168 1 222) 24 "en0")))]
           [(or "hop" "walk")
            (scn/union (assertion (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) "wlan0"))
                       (assertion (host-route (bytes 192 168 1 222) 24 "wlan0")))]
           ["stockholm.ccs.neu.edu"
            (scn/union (assertion (host-route (bytes 129 10 115 94) 24 "eth0"))
                       (assertion (host-route (bytes 192 168 56 222) 24 "vboxnet0"))
                       (assertion (gateway-route (bytes 0 0 0 0) 0 (bytes 129 10 115 1) "eth0")))]
           [else
            (error 'spawn-demo-config "No setup for hostname ~a" (gethostname))])))
