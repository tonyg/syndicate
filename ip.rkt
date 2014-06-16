#lang racket/base

(provide ;; (struct-out ip-packet)
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

(struct ip-packet (source destination ttl protocol options body) #:prefab) ;; TODO: more fields

(define IPv4-ethertype #x0800)

(define IP-VERSION 4)
(define IP-MINIMUM-HEADER-LENGTH 5)

(define PROTOCOL-ICMP 1)
;; (define PROTOCOL-TCP 6)
;; (define PROTOCOL-UDP 17)

(define default-ttl 64)

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
	 (header-checksum :: bits 16)
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
		  (>= (bit-string-length body) (* header-length 4)))
	     (bit-string-case rest
	       ([ (opts :: binary bytes options-length)
		  (data :: binary) ]
		(transition s (send (ip-packet (bit-string->bytes source-ip)
					       (bit-string->bytes destination-ip)
					       ttl
					       protocol
					       (bit-string->bytes opts)
					       (bit-string->bytes data))))))
	     #f)))
      (else #f)))

  (define (analyze-gestalt g s)
    (define hwaddr (gestalt->hwaddr g interface-name))
    (define new-s (struct-copy state s [hwaddr hwaddr]))
    (transition new-s '()))

  (list
   (spawn-icmp-driver my-address)
   (spawn (lambda (e s)
	    (match e
	      [(routing-update g)
	       (analyze-gestalt g s)]
	      [(message (ethernet-packet _ _ _ _ _ body) _ _)
	       (analyze-incoming-packet body s)]
	      [(message (ip-packet _ peer-address ttl protocol options body) _ _)
	       (define header-length ;; TODO: ensure options is a multiple of 4 bytes
		 (+ IP-MINIMUM-HEADER-LENGTH (quotient (bit-string-length options) 32)))
	       (define header0
		 (bit-string (IP-VERSION :: bits 4)
			     (header-length :: bits 4)
			     0 ;; TODO: service type
			     ((+ (* header-length 4)
				 (/ (bit-string-length body) 8))
			      :: bits 16)
			     (0 :: bits 16) ;; TODO: identifier
			     (0 :: bits 3)  ;; TODO: flags
			     (0 :: bits 13) ;; TODO: fragments
			     ttl
			     protocol
			     (0 :: bits 16)
			     (my-address :: binary bits 32)
			     (peer-address :: binary bits 32)
			     (options :: binary)))
	       (transition s (send (ethernet-packet (ethernet-interface interface-name
									(state-hwaddr s))
						    #f
						    (state-hwaddr s)
						    broadcast-ethernet-address
						    IPv4-ethertype
						    (bit-string ((ip-checksum 10 header0) :: binary)
								(body :: binary)))))]
	      [_ #f]))
	  (state #f)
	  (gestalt-union
	   (pub (arp-assertion IPv4-ethertype my-address))
	   (sub (ethernet-packet-pattern interface-name #t IPv4-ethertype))
	   (sub (ethernet-packet-pattern interface-name #t IPv4-ethertype) #:level 1)
	   (pub (ethernet-packet-pattern interface-name #f IPv4-ethertype))
	   (sub (ip-packet my-address ? ? ? ? ?))))))

(define (spawn-icmp-driver my-address)
  (spawn (lambda (e s)
	   (match e
	     [(message (ip-packet peer-address _ _ _ _ body) _ _)
	      (bit-string-case body
		([ type code (checksum :: integer bytes 2) (rest :: binary) ]
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
						   default-ttl
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
	 (gestalt-union (pub (ip-packet my-address ? ? PROTOCOL-ICMP ? ?))
			(sub (ip-packet ? my-address ? PROTOCOL-ICMP ? ?)))))