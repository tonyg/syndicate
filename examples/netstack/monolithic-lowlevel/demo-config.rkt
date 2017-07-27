#lang racket/base
;; Demonstration stack configuration for various hosts.

(require racket/match)
(require syndicate/monolithic)
(require (only-in mzlib/os gethostname))
(require (only-in racket/string string-split))
(require "configuration.rkt")

(provide spawn-demo-config)

(define (spawn-demo-config)
  (actor (lambda (e s) #f)
         (void)
         (match (gethostname)
           ["stockholm.ccs.neu.edu"
            (scn/union (assertion (host-route (bytes 129 10 115 94) 24 "eth0"))
                       (assertion (gateway-route (bytes 0 0 0 0) 0 (bytes 129 10 115 1) "eth0")))]
           [other ;; assume a private network
            (define interface
              (match (car (string-split other "."))
                ["skip" "en0"]
                ["leap" "wlp4s0"] ;; wtf
                [_ "wlan0"]))
            (scn/union (assertion (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) interface))
                       (assertion (host-route (bytes 192 168 1 222) 24 interface)))])))
