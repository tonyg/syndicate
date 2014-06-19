#lang racket/base

(provide (struct-out udp-remote-address)
	 (struct-out udp-handle)
	 (struct-out udp-listener)
	 udp-address?
	 udp-local-address?
	 (struct-out udp-packet)
	 spawn-udp-driver)

(require racket/set)
(require racket/match)
(require minimart)
(require minimart/demand-matcher)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "checksum.rkt")
(require "ip.rkt")
(require "port-allocator.rkt")

;; udp-address/udp-address : "kernel" udp connection state machines
;; udp-handle/udp-address : "user" outbound connections
;; udp-listener/udp-address : "user" inbound connections

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol messages

(struct udp-remote-address (host port) #:prefab)
(struct udp-handle (id) #:prefab)
(struct udp-listener (port) #:prefab)

(define (udp-address? x)
  (or (udp-remote-address? x)
      (udp-local-address? x)))

(define (udp-local-address? x)
  (or (udp-handle? x)
      (udp-listener? x)))

(struct udp-packet (source destination body) #:prefab)

(define any-remote (udp-remote-address ? ?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-accessible driver startup

(define (spawn-udp-driver)
  (list
   (spawn-demand-matcher (udp-packet ? (?! (udp-listener ?)) ?)
			 #:demand-is-subscription? #t
			 (lambda (handle) (spawn-udp-relay (udp-listener-port handle) handle)))
   (spawn-demand-matcher (udp-packet ? (?! (udp-handle ?)) ?)
			 #:demand-is-subscription? #t
			 (lambda (handle)
			   (send (port-allocation-request
				  'udp
				  (lambda (port local-ips) (spawn-udp-relay port handle))))))
   (spawn-port-allocator 'udp
			 (list (project-subs (udp-packet (udp-remote-address (?!) (?!)) ? ?))
			       (project-subs (udp-packet ? (udp-remote-address (?!) (?!)) ?))))
   (spawn-kernel-udp-driver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relaying

(define (spawn-udp-relay local-port local-user-addr)
  (log-info "Spawning UDP relay ~v / ~v" local-port local-user-addr)

  (define local-peer-gestalt (pub (udp-packet any-remote local-user-addr ?) #:level 1))

  (define (compute-gestalt local-ips)
    (for/fold [(g (gestalt-union local-peer-gestalt
				 observe-local-ip-addresses-gestalt
				 (pub (udp-packet any-remote local-user-addr ?))
				 (sub (udp-packet local-user-addr any-remote ?))))]
	[(ip (in-set local-ips))]
      (define hostname (ip-address->hostname ip))
      (define local-network-addr (udp-remote-address hostname local-port))
      (gestalt-union g
		     (sub (udp-packet any-remote local-network-addr ?))
		     (pub (udp-packet local-network-addr any-remote ?)))))

  (spawn (lambda (e local-ips)
	   (log-info "RELAY ~v" e)
	   (match e
	     [(routing-update g)
	      (define new-local-ips (gestalt->local-ip-addresses g))
	      (log-info "Updating relay gestalt:\n~a" (gestalt->pretty-string (compute-gestalt new-local-ips)))
	      (transition new-local-ips
			  (list
			   (when (gestalt-empty? (gestalt-filter g local-peer-gestalt)) (quit))
			   (routing-update (compute-gestalt new-local-ips))))]
	     [(message (udp-packet (== local-user-addr) remote-addr bs) _ _)
	      ;; Choose arbitrary local IP address for outbound packet!
	      ;; TODO: what can be done? Must I examine the routing table?
	      (define local-network-addr
		(udp-remote-address (ip-address->hostname (set-first local-ips)) local-port))
	      (transition local-ips (send (udp-packet local-network-addr remote-addr bs)))]
	     [(message (udp-packet remote-addr (udp-remote-address _ _) bs) _ _)
	      (transition local-ips (send (udp-packet remote-addr local-user-addr bs)))]
	     [_ #f]))
	 (set)
	 (compute-gestalt (set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codec & kernel-level driver

(define PROTOCOL-UDP 17)

(define (spawn-kernel-udp-driver)
  (spawn (lambda (e local-ips)
	   (match e
	     [(routing-update g)
	      (transition (gestalt->local-ip-addresses g) '())]
	     [(message (ip-packet source-if src-ip dst-ip _ _ body) _ _)
	      #:when (and source-if (set-member? local-ips dst-ip))
	      (define src-host (ip-address->hostname src-ip))
	      (define dst-host (ip-address->hostname dst-ip))
	      (bit-string-case body
		([ (src-port :: integer bytes 2)
		   (dst-port :: integer bytes 2)
		   (length :: integer bytes 2)
		   (checksum :: integer bytes 2) ;; TODO: check checksum
		   (data :: binary) ]
		 (bit-string-case data
		   ([ (payload :: binary bytes (- length 8)) ;; min UDP header size is 8 bytes
		      (:: binary) ]
		    (transition local-ips (send (udp-packet (udp-remote-address src-host src-port)
							    (udp-remote-address dst-host dst-port)
							    (bit-string->bytes payload)))))
		   (else #f)))
		(else #f))]
	     [(message (udp-packet (udp-remote-address sh sp) (udp-remote-address dh dp) bs) _ _)
	      (define src-ip (ip-string->ip-address sh))
	      (define dst-ip (ip-string->ip-address dh))
	      (and (set-member? local-ips src-ip)
		   (let* ((payload (bit-string (sp :: integer bytes 2)
					       (dp :: integer bytes 2)
					       ((+ 8 (bit-string-byte-count bs))
						:: integer bytes 2)
					       (0 :: integer bytes 2) ;; checksum location
					       (bs :: binary)))
			  (pseudo-header (bit-string (src-ip :: binary bytes 4)
						     (dst-ip :: binary bytes 4)
						     0
						     PROTOCOL-UDP
						     ((bit-string-byte-count payload)
						      :: integer bytes 2)))
			  (checksummed-payload (ip-checksum #:pseudo-header pseudo-header
							    6 payload)))
		     (transition local-ips (send (ip-packet #f
							    src-ip
							    dst-ip
							    PROTOCOL-UDP
							    #""
							    checksummed-payload)))))]
	     [_ #f]))
	 (set)
	 (gestalt-union (pub (ip-packet #f ? ? PROTOCOL-UDP ? ?))
			(sub (ip-packet ? ? ? PROTOCOL-UDP ? ?))
			(sub (udp-packet any-remote any-remote ?))
			(pub (udp-packet any-remote any-remote ?))
			observe-local-ip-addresses-gestalt)))
