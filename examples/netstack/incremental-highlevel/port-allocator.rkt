#lang syndicate/actor
;; UDP/TCP port allocator

(provide spawn-port-allocator
         allocate-port!
	 (struct-out port-allocation-request)
         (struct-out port-allocation-reply))

(require racket/set)
(require "ip.rkt")

(struct port-allocation-request (reqid type) #:prefab)
(struct port-allocation-reply (reqid port) #:prefab)

(define (spawn-port-allocator allocator-type query-used-ports)
  (actor #:name (list 'port-allocator allocator-type)
         (react
          (define local-ips (query-local-ip-addresses))
          (define used-ports (query-used-ports))

          (begin/dataflow
            (log-info "port-allocator ~v used ports: ~v" allocator-type (used-ports)))

          (on (message (port-allocation-request $reqid allocator-type))
              (define currently-used-ports (used-ports))
              (let randomly-allocate-until-unused ()
                (define p (+ 1024 (random 64512)))
                (if (set-member? currently-used-ports p)
                    (randomly-allocate-until-unused)
                    (begin (used-ports (set-add currently-used-ports p))
                           (send! (port-allocation-reply reqid p)))))))))

(define (allocate-port! type)
  (define reqid (gensym 'allocate-port!))
  (react/suspend (done)
    (stop-when (message (port-allocation-reply reqid $port)) (done port))
    (on-start (send! (port-allocation-request reqid type)))))
