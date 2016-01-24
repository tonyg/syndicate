#lang racket/base
;; UDP/TCP port allocator

(provide spawn-port-allocator
	 (struct-out port-allocation-request))

(require racket/set)
(require racket/match)
(require prospect-monolithic)
(require "ip.rkt")

(struct port-allocation-request (type k) #:prefab)

(struct port-allocator-state (used-ports local-ips) #:transparent)

(define (spawn-port-allocator allocator-type observer-gestalt compute-used-ports)
  (spawn (lambda (e s)
	   (match e
	     [(scn g)
	      (define local-ips (or (gestalt->local-ip-addresses g) (set)))
	      (define new-used-ports (compute-used-ports g local-ips))
	      (log-info "port-allocator ~v used ports: ~v" allocator-type new-used-ports)
	      (transition (port-allocator-state new-used-ports local-ips) '())]
	     [(message (port-allocation-request _ k))
	      (define currently-used-ports (port-allocator-state-used-ports s))
	      (let randomly-allocate-until-unused ()
		(define p (+ 1024 (random 64512)))
		(if (set-member? currently-used-ports p)
		    (randomly-allocate-until-unused)
		    (transition (struct-copy port-allocator-state s
				  [used-ports (set-add currently-used-ports p)])
				(k p (port-allocator-state-local-ips s)))))]
	     [_ #f]))
	 (port-allocator-state (set) (set))
         (scn/union (subscription (port-allocation-request allocator-type ?))
                    observe-local-ip-addresses-gestalt
                    observer-gestalt)))
