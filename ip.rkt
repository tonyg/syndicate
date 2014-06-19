#lang racket/base

(provide (struct-out ip-packet)
	 ip-address->hostname
	 apply-netmask
	 ip-address-in-subnet?
	 gestalt->local-ip-addresses
	 observe-local-ip-addresses-gestalt
	 broadcast-ip-address
	 spawn-ip-driver)

(require racket/set)
(require racket/match)
(require minimart)
(require minimart/drivers/timer)
(require minimart/demand-matcher)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "configuration.rkt")
(require "checksum.rkt")
(require "ethernet.rkt")
(require "arp.rkt")

(struct ip-packet (source-interface ;; string for an ethernet interface, or #f for local interfaces
		   source
		   destination
		   protocol
		   options
		   body)
	#:prefab) ;; TODO: more fields

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ip-address->hostname bs)
  (bit-string-case bs
    ([ a b c d ] (format "~a.~a.~a.~a" a b c d))))

(define (apply-netmask addr netmask)
  (bit-string-case addr
    ([ (n :: integer bytes 4) ]
     (bit-string ((bitwise-and n (arithmetic-shift #x-100000000 (- netmask)))
		  :: integer bytes 4)))))

(define (ip-address-in-subnet? addr network netmask)
  (equal? (apply-netmask network netmask)
	  (apply-netmask addr netmask)))

(define broadcast-ip-address (bytes 255 255 255 255))

(define local-ip-address-projector (project-pubs (host-route (?!) ? ?)))
(define (gestalt->local-ip-addresses g) (gestalt-project/single g local-ip-address-projector))
(define observe-local-ip-addresses-gestalt (sub (host-route ? ? ?) #:level 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spawn-ip-driver)
  (list
   (spawn-demand-matcher (host-route (?!) (?!) (?!))
			 #:supply-level 1
			 spawn-host-route)
   (spawn-demand-matcher (net-route (?!) (?!) (?!))
			 #:supply-level 1
			 spawn-net-route)))

(define (host-route-supply ip-addr netmask interface-name)
  (sub (host-route ip-addr netmask interface-name) #:level 1))

(define (net-route-supply network-addr netmask link)
  (sub (net-route network-addr netmask link) #:level 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local IP route

(define (spawn-host-route my-address netmask interface-name)
  (list
   (let ((network-addr (apply-netmask my-address netmask)))
     (spawn-normal-ip-route (host-route-supply my-address netmask interface-name)
			    network-addr
			    netmask
			    interface-name))
   (spawn (lambda (e s)
	    (match e
	      [(routing-update g)
	       (transition s (when (gestalt-empty? g) (quit)))]
	      [(message (ip-packet _ peer-address _ _ _ body) _ _)
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
		     (transition s (send (ip-packet #f
						    my-address
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
	  (gestalt-union (pub (ip-packet ? my-address ? PROTOCOL-ICMP ? ?))
			 (sub (ip-packet ? ? my-address PROTOCOL-ICMP ? ?))
			 (pub (arp-assertion IPv4-ethertype my-address interface-name))
			 (host-route-supply my-address netmask interface-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gateway IP route

(define (spawn-net-route network-addr netmask link)
  (cond
   [(bytes? link) (spawn-gateway-ip-route network-addr netmask link)]
   [(string? link) (spawn-normal-ip-route (net-route-supply network-addr netmask link)
					  network-addr
					  netmask
					  link)]
   [else (error 'ip "Invalid net-route: ~v ~v ~v" network-addr netmask link)]))

(define (spawn-gateway-ip-route network netmask gateway-addr)
  (lookup-arp gateway-addr
	      ?
	      (net-route-supply network netmask gateway-addr)
	      (lambda (interface gateway-hwaddr)
		(spawn-gateway-ip-route* network netmask gateway-addr interface gateway-hwaddr))))

(define (spawn-gateway-ip-route* network netmask gateway-addr interface gateway-hwaddr)
  (define host-route-projector (project-subs (host-route (?!) ? ?)))
  (define net-route-projector (project-subs (net-route (?!) (?!) ?)))

  (define (covered-by-some-other-route? addr routes)
    (for/or ([r (in-set routes)])
      (match-define (list net msk) r)
      (and (positive? msk)
	   (ip-address-in-subnet? addr net msk))))

  (spawn (lambda (e routes)
	   (match e
	     [(routing-update g)
	      (define host-ips (gestalt-project/single g host-route-projector))
	      (define net-ips+netmasks (gestalt-project/keys g net-route-projector))
	      (transition (set-union (for/set ([ip host-ips]) (list ip 32)) net-ips+netmasks)
			  (when (gestalt-empty? (gestalt-filter g (net-route-supply network
										    netmask
										    gateway-addr)))
			    (quit)))]
	     [(message (? ip-packet? p) _ _)
	      (and (not (equal? (ip-packet-source-interface p) (ethernet-interface-name interface)))
		   (not (covered-by-some-other-route? (ip-packet-destination p) routes))
		   (transition routes
			       (send (ethernet-packet interface
						      #f
						      (ethernet-interface-hwaddr interface)
						      gateway-hwaddr
						      IPv4-ethertype
						      (format-ip-packet p)))))]
	     [_ #f]))
	 (set)
	 (gestalt-union (if (zero? netmask)
			    (net-route-supply network netmask gateway-addr)
			    (gestalt-empty))
			observe-local-ip-addresses-gestalt
			(sub (ip-packet ? ? ? ? ? ?))
			(pub (ip-packet ? ? ? ? ? ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal IP route

(define (spawn-normal-ip-route gestalt-for-supply network netmask interface-name)
  (spawn (lambda (e s)
	   (match e
	     [(routing-update g)
	      (log-info "normal-ip route ~v/~v/~v quitting:\n~a"
			network
			netmask
			interface-name
			(gestalt->pretty-string g))
	      (transition s (when (gestalt-empty? g) (quit)))]
	     [(message (ethernet-packet _ _ _ _ _ body) _ _)
	      (define p (parse-ip-packet interface-name body))
	      (and p (transition s (send p)))]
	     [(message (? ip-packet? p) _ _)
	      (define destination (ip-packet-destination p))
	      (and (not (equal? (ip-packet-source-interface p) interface-name))
		   (ip-address-in-subnet? destination network netmask)
		   (transition
		    s
		    (lookup-arp destination
				(ethernet-interface interface-name ?)
				(gestalt-empty)
				(lambda (interface destination-hwaddr)
				  (send (ethernet-packet interface
							 #f
							 (ethernet-interface-hwaddr interface)
							 destination-hwaddr
							 IPv4-ethertype
							 (format-ip-packet p)))))))]
	     [_ #f]))
	 (void)
	 (gestalt-union gestalt-for-supply
			(sub (ethernet-packet-pattern interface-name #t IPv4-ethertype))
			(sub (ethernet-packet-pattern interface-name #t IPv4-ethertype) #:level 1)
			(pub (ethernet-packet-pattern interface-name #f IPv4-ethertype))
			(pub (arp-interface interface-name))
			(sub (ip-packet ? ? ? ? ? ?))
			(pub (ip-packet ? ? ? ? ? ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define IPv4-ethertype #x0800)

(define IP-VERSION 4)
(define IP-MINIMUM-HEADER-LENGTH 5)

(define PROTOCOL-ICMP 1)
;; (define PROTOCOL-TCP 6)
;; (define PROTOCOL-UDP 17)

(define default-ttl 64)

(define (parse-ip-packet interface-name body)
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
     (let* ((source-ip (bit-string->bytes source-ip0))
	    (destination-ip (bit-string->bytes destination-ip0))
	    (options-length (* 4 (- header-length IP-MINIMUM-HEADER-LENGTH)))
	    (data-length (- total-length (* 4 header-length))))
       (if (and (>= header-length 5)
		(>= (bit-string-byte-count body) (* header-length 4)))
	   (bit-string-case rest
	     ([ (opts :: binary bytes options-length)
		(data :: binary bytes data-length)
		(:: binary) ] ;; Very short ethernet packets have a trailer of zeros
	      (ip-packet interface-name
			 (bit-string->bytes source-ip)
			 (bit-string->bytes destination-ip)
			 protocol
			 (bit-string->bytes opts)
			 (bit-string->bytes data))))
	   #f)))
    (else #f)))

(define (format-ip-packet p)
  (match-define (ip-packet _ src dst protocol options body) p)

  (define header-length ;; TODO: ensure options is a multiple of 4 bytes
    (+ IP-MINIMUM-HEADER-LENGTH (quotient (bit-string-byte-count options) 4)))

  (define header0 (bit-string (IP-VERSION :: bits 4)
			      (header-length :: bits 4)
			      0 ;; TODO: service type
			      ((+ (* header-length 4) (bit-string-byte-count body))
			       :: bits 16)
			      (0 :: bits 16)  ;; TODO: identifier
			      (0 :: bits 3)   ;; TODO: flags
			      (0 :: bits 13)  ;; TODO: fragments
			      default-ttl
			      protocol
			      (0 :: bits 16)
			      (src :: binary bits 32)
			      (dst :: binary bits 32)
			      (options :: binary)))
  (define full-packet (bit-string ((ip-checksum 10 header0) :: binary) (body :: binary)))

  full-packet)

(define (lookup-arp ipaddr query-interface-pattern base-gestalt k)
  (on-gestalt (lambda (_g arp-results)
		(if (not arp-results)
		    (error 'ip "Someone has published a wildcard arp result")
		    (and (not (set-empty? arp-results))
			 (match (set-first arp-results)
			   [(list interface hwaddr)
			    (log-info "ARP lookup yielded ~a on ~a for ~a"
				      (pretty-bytes hwaddr)
				      (ethernet-interface-name interface)
				      (ip-address->hostname ipaddr))
			    (when (> (set-count arp-results) 1)
			      (log-warning "Ambiguous ARP result for ~a: ~v"
					   (ip-address->hostname ipaddr)
					   arp-results))
			    (k interface hwaddr)]))))
	      base-gestalt
	      (project-pubs (arp-query IPv4-ethertype ipaddr (?! query-interface-pattern) (?!)))
	      #:timeout-msec 5000
	      #:on-timeout (lambda ()
			     (log-warning "ARP lookup of ~a failed, packet dropped"
					  (ip-address->hostname ipaddr))
			     '())))
