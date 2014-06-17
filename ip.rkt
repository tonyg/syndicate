#lang racket/base

(provide (struct-out ip-packet)
	 (struct-out ip-interface)
	 ip-address->hostname
	 broadcast-ip-address
	 spawn-ip-driver)

(require racket/set)
(require racket/match)
(require minimart)
(require minimart/drivers/timer)
(require minimart/demand-matcher)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "checksum.rkt")
(require "ethernet.rkt")
(require "arp.rkt")

(struct ip-packet (source destination protocol options body) #:prefab) ;; TODO: more fields
(struct ip-interface (address ethernet) #:prefab)

(define IPv4-ethertype #x0800)

(define IP-VERSION 4)
(define IP-MINIMUM-HEADER-LENGTH 5)

(define PROTOCOL-ICMP 1)
;; (define PROTOCOL-TCP 6)
;; (define PROTOCOL-UDP 17)

(define default-ttl 64)

(define (ip-address->hostname bs)
  (bit-string-case bs
    ([ a b c d ] (format "~a.~a.~a.~a" a b c d))))

(define broadcast-ip-address (bytes 255 255 255 255))

(struct state (hwaddr) #:transparent)

(define (spawn-ip-driver interface-name my-address)

  (define (analyze-incoming-packet body s)
    ;; (log-info "IP ~a got body ~a" (pretty-bytes my-address) (pretty-bytes body))
    (bit-string-case body
      ([ (= IP-VERSION :: bits 4)
	 (header-length :: bits 4)
	 service-type
	 (total-length :: bits 16)
	 (id :: bits 16)
	 (flags :: bits 3)
	 (fragment-offset :: bits 13)
	 ttl
	 protocol
	 (header-checksum :: bits 16) ;; TODO: check checksum
	 (source-ip0 :: binary bits 32)
	 (destination-ip0 :: binary bits 32)
	 (rest :: binary) ]
       (let ((source-ip (bit-string->bytes source-ip0))
	     (destination-ip (bit-string->bytes destination-ip0))
	     (options-length (* 4 (- header-length IP-MINIMUM-HEADER-LENGTH))))
	 (if (and (not (equal? source-ip my-address))
		  (or (equal? destination-ip my-address)
		      (equal? destination-ip broadcast-ip-address))
		  (>= header-length 5)
		  (>= (bit-string-byte-count body) (* header-length 4)))
	     (bit-string-case rest
	       ([ (opts :: binary bytes options-length)
		  (data :: binary) ]
		(transition s (send (ip-packet (bit-string->bytes source-ip)
					       (bit-string->bytes destination-ip)
					       protocol
					       (bit-string->bytes opts)
					       (bit-string->bytes data))))))
	     #f)))
      (else #f)))

  (define (analyze-gestalt g s)
    (define hwaddr (gestalt->hwaddr g interface-name))
    (define new-s (struct-copy state s [hwaddr hwaddr]))
    (transition new-s (routing-update (compute-gestalt new-s))))

  (define (compute-gestalt s)
    (gestalt-union
     (pub (arp-assertion IPv4-ethertype my-address))
     (sub (ethernet-packet-pattern interface-name #t IPv4-ethertype))
     (sub (ethernet-packet-pattern interface-name #t IPv4-ethertype) #:level 1)
     (pub (ethernet-packet-pattern interface-name #f IPv4-ethertype))
     (sub (ip-packet my-address ? ? ? ?))
     (pub (ip-packet ? my-address ? ? ?))
     (if (state-hwaddr s)
	 (pub (ip-interface my-address (ethernet-interface interface-name (state-hwaddr s))))
	 (gestalt-empty))))

  (list
   (spawn-icmp-driver my-address)
   (let ((state0 (state #f)))
     (spawn (lambda (e s)
	      (match e
		[(routing-update g)
		 (analyze-gestalt g s)]
		[(message (ethernet-packet _ _ _ _ _ body) _ _)
		 (analyze-incoming-packet body s)]
		[(message (ip-packet _ peer-address protocol options body) _ _)
		 (define header-length ;; TODO: ensure options is a multiple of 4 bytes
		   (+ IP-MINIMUM-HEADER-LENGTH (quotient (bit-string-byte-count options) 4)))
		 (define header0
		   (bit-string (IP-VERSION :: bits 4)
			       (header-length :: bits 4)
			       0 ;; TODO: service type
			       ((+ (* header-length 4) (bit-string-byte-count body))
				:: bits 16)
			       (0 :: bits 16) ;; TODO: identifier
			       (0 :: bits 3)  ;; TODO: flags
			       (0 :: bits 13) ;; TODO: fragments
			       default-ttl
			       protocol
			       (0 :: bits 16)
			       (my-address :: binary bits 32)
			       (peer-address :: binary bits 32)
			       (options :: binary)))
		 (define full-packet (bit-string ((ip-checksum 10 header0) :: binary)
						 (body :: binary)))
		 (transition s (spawn-packet-sender interface-name
						    (state-hwaddr s)
						    peer-address
						    full-packet))]
		[_ #f]))
	    state0
	    (compute-gestalt state0)))))

(define arp-result-projection (compile-gestalt-projection (arp-query ? ? (?!))))

(define (spawn-packet-sender interface-name local-hwaddr remote-ip full-packet)
  (define timer-id (list (gensym 'packet) remote-ip))
  (list
   (send (set-timer timer-id 5000 'relative))
   (spawn (lambda (e s)
	    (match e
	      [(routing-update g)
	       (define all-results
		 (set->list (matcher-key-set (gestalt-project g 0 0 #t arp-result-projection))))
	       (match all-results
		 [#f (error 'ip "Someone has published a wildcard arp result")]
		 ['() ;; no results yet, keep waiting
		  #f]
		 [(list* (list remote-hwaddr) rest)
		  (unless (null? rest)
		    (log-warning "Ambiguous arp result for ~a: ~v"
				 (ip-address->hostname remote-ip)
				 all-results))
		  (transition s
			      (list
			       (send (ethernet-packet (ethernet-interface interface-name
									  local-hwaddr)
						      #f
						      local-hwaddr
						      remote-hwaddr
						      IPv4-ethertype
						      full-packet))
			       (quit)))])]
	      [(message (timer-expired _ _) _ _)
	       (log-warning "ARP lookup failed, packet dropped")
	       (transition s (quit))]
	      [_ #f]))
	  (void)
	  (gestalt-union (sub (timer-expired timer-id ?))
			 (sub (arp-query IPv4-ethertype remote-ip ?) #:level 1)))))

(define (spawn-icmp-driver my-address)
  (spawn (lambda (e s)
	   (match e
	     [(message (ip-packet peer-address _ _ _ body) _ _)
	      (bit-string-case body
		([ type code (checksum :: integer bytes 2) (rest :: binary) ] ;; TODO: check cksum
		 (case type
		   [(8) ;; ECHO (0 is ECHO-REPLY)
		    (log-info "Ping of ~a from ~a"
			      (pretty-bytes my-address)
			      (pretty-bytes peer-address))
		    (define reply-data0 (bit-string 0
						    code
						    (0 :: integer bytes 2) ;; TODO
						    (rest :: binary)))
		    (transition s (send (ip-packet my-address
						   peer-address
						   PROTOCOL-ICMP
						   #""
						   (ip-checksum 2 reply-data0))))]
		   [else
		    (log-info "ICMP ~a/~a (cksum ~a) to ~a from ~a:\n~a"
			      type
			      code
			      checksum
			      (pretty-bytes my-address)
			      (pretty-bytes peer-address)
			      (dump-bytes->string rest))
		    #f]))
		(else #f))]
	     [_ #f]))
	 (void)
	 (gestalt-union (pub (ip-packet my-address ? PROTOCOL-ICMP ? ?))
			(sub (ip-packet ? my-address PROTOCOL-ICMP ? ?)))))
