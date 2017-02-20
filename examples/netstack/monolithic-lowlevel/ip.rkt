#lang racket/base

(provide (struct-out ip-packet)
	 ip-address->hostname
	 ip-string->ip-address
	 apply-netmask
	 ip-address-in-subnet?
	 gestalt->local-ip-addresses
	 observe-local-ip-addresses-gestalt
	 broadcast-ip-address
	 spawn-ip-driver)

(require racket/set)
(require racket/match)
(require (only-in racket/string string-split))
(require syndicate/monolithic)
(require syndicate/drivers/timer)
(require syndicate/demand-matcher)
(require syndicate/protocol/advertise)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "configuration.rkt")
(require "checksum.rkt")
(require "ethernet.rkt")
(require "arp.rkt")
(require "on-claim.rkt")

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

(define (ip-string->ip-address str)
  (list->bytes (map string->number (string-split str "."))))

(define (apply-netmask addr netmask)
  (bit-string-case addr
    ([ (n :: integer bytes 4) ]
     (bit-string ((bitwise-and n (arithmetic-shift #x-100000000 (- netmask)))
		  :: integer bytes 4)))))

(define (ip-address-in-subnet? addr network netmask)
  (equal? (apply-netmask network netmask)
	  (apply-netmask addr netmask)))

(define broadcast-ip-address (bytes 255 255 255 255))

(define local-ip-address-projector (host-route (?!) ? ?))
(define (gestalt->local-ip-addresses g) (trie-project/set/single g local-ip-address-projector))
(define observe-local-ip-addresses-gestalt (subscription (host-route ? ? ?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spawn-ip-driver)
  (list
   (spawn-demand-matcher (host-route (?!) (?!) (?!))
                         (route-up (host-route (?!) (?!) (?!)))
			 spawn-host-route)
   (spawn-demand-matcher (gateway-route (?!) (?!) (?!) (?!))
                         (route-up (gateway-route (?!) (?!) (?!) (?!)))
			 spawn-gateway-route)
   (spawn-demand-matcher (net-route (?!) (?!) (?!))
                         (route-up (net-route (?!) (?!) (?!)))
			 spawn-net-route)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local IP route

(define (spawn-host-route my-address netmask interface-name)
  (list
   (let ((network-addr (apply-netmask my-address netmask)))
     (spawn-normal-ip-route (host-route my-address netmask interface-name)
			    network-addr
			    netmask
			    interface-name))
   (actor (lambda (e s)
	    (match e
	      [(scn (? trie-empty?)) (quit)]
	      [(message (ip-packet _ peer-address _ _ _ body))
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
		     (transition s (message (ip-packet #f
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
          (scn/union (advertisement (ip-packet ? my-address ? PROTOCOL-ICMP ? ?))
                     (subscription (ip-packet ? ? my-address PROTOCOL-ICMP ? ?))
                     (assertion (arp-assertion IPv4-ethertype my-address interface-name))
                     (subscription (host-route my-address netmask interface-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gateway IP route

(struct gateway-route-state (routes gateway-interface gateway-hwaddr) #:transparent)

(define (spawn-gateway-route network netmask gateway-addr interface-name)
  (define the-route (gateway-route network netmask gateway-addr interface-name))

  (define host-route-projector (host-route (?!) (?!) ?))
  (define gateway-route-projector (gateway-route (?!) (?!) ? ?))
  (define net-route-projector (net-route (?!) (?!) ?))
  (define gateway-arp-projector (arp-query IPv4-ethertype
                                           gateway-addr
                                           (?! (ethernet-interface interface-name ?))
                                           (?!)))

  (define (covered-by-some-other-route? addr routes)
    (for/or ([r (in-set routes)])
      (match-define (list net msk) r)
      (and (positive? msk)
	   (ip-address-in-subnet? addr net msk))))

  (actor (lambda (e s)
	   (match e
	     [(scn g)
	      (define host-ips+netmasks (trie-project/set #:take 2 g host-route-projector))
	      (define gw-nets+netmasks (trie-project/set #:take 2 g gateway-route-projector))
	      (define net-nets+netmasks (trie-project/set #:take 2 g net-route-projector))
	      (define gw-ip+hwaddr
                (let ((vs (trie-project/set #:take 2 g gateway-arp-projector)))
                  (and vs (not (set-empty? vs)) (set-first vs))))
	      (when (and gw-ip+hwaddr (not (gateway-route-state-gateway-hwaddr s)))
		(log-info "Discovered gateway ~a at ~a on interface ~a."
			  (ip-address->hostname gateway-addr)
			  (ethernet-interface-name (car gw-ip+hwaddr))
			  (pretty-bytes (cadr gw-ip+hwaddr))))
              (if (trie-empty? (project-assertions g (?! the-route)))
                  (quit)
                  (transition (gateway-route-state
                               (set-union host-ips+netmasks
                                          gw-nets+netmasks
                                          net-nets+netmasks)
                               (and gw-ip+hwaddr (car gw-ip+hwaddr))
                               (and gw-ip+hwaddr (cadr gw-ip+hwaddr)))
                              '()))]
	     [(message (? ip-packet? p))
	      (define gw-if (gateway-route-state-gateway-interface s))
	      (when (not gw-if)
		(log-warning "Gateway hwaddr for ~a not known, packet dropped."
			     (ip-address->hostname gateway-addr)))
	      (and gw-if
		   (not (equal? (ip-packet-source-interface p) (ethernet-interface-name gw-if)))
		   (not (covered-by-some-other-route? (ip-packet-destination p)
						      (gateway-route-state-routes s)))
		   (transition s
			       (message (ethernet-packet gw-if
                                                         #f
                                                         (ethernet-interface-hwaddr gw-if)
                                                         (gateway-route-state-gateway-hwaddr s)
                                                         IPv4-ethertype
                                                         (format-ip-packet p)))))]
	     [_ #f]))
	 (gateway-route-state (set) #f #f)
         (scn/union (subscription the-route)
                    (assertion (route-up the-route))
                    (subscription (ip-packet ? ? ? ? ? ?))
                    observe-local-ip-addresses-gestalt
                    (subscription (net-route ? ? ?))
                    (subscription (gateway-route ? ? ? ?))
                    (subscription (projection->pattern gateway-arp-projector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General net route

(define (spawn-net-route network-addr netmask link)
  (spawn-normal-ip-route (net-route network-addr netmask link) network-addr netmask link))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal IP route

(define (spawn-normal-ip-route the-route network netmask interface-name)
  (actor (lambda (e s)
	   (match e
	     [(scn (? trie-empty?)) (quit)]
	     [(message (ethernet-packet _ _ _ _ _ body))
	      (define p (parse-ip-packet interface-name body))
	      (and p (transition s (message p)))]
	     [(message (? ip-packet? p))
	      (define destination (ip-packet-destination p))
	      (and (not (equal? (ip-packet-source-interface p) interface-name))
		   (ip-address-in-subnet? destination network netmask)
		   (transition
		    s
		    (lookup-arp destination
				(ethernet-interface interface-name ?)
                                trie-empty
				(lambda (interface destination-hwaddr)
				  (message (ethernet-packet interface
                                                            #f
                                                            (ethernet-interface-hwaddr interface)
                                                            destination-hwaddr
                                                            IPv4-ethertype
                                                            (format-ip-packet p)))))))]
	     [_ #f]))
	 (void)
         (scn/union (subscription the-route)
                    (assertion (route-up the-route))
                    (subscription (ethernet-packet-pattern interface-name #t IPv4-ethertype))
                    (assertion (arp-interface interface-name))
                    (subscription (ip-packet ? ? ? ? ? ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define IPv4-ethertype #x0800)

(define IP-VERSION 4)
(define IP-MINIMUM-HEADER-LENGTH 5)

(define PROTOCOL-ICMP 1)

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
  (on-claim #:name (string->symbol (format "lookup-arp:~a" (ip-address->hostname ipaddr)))
            (lambda (_g arp-results)
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
            (arp-query IPv4-ethertype ipaddr (?! query-interface-pattern) (?!))
            #:timeout-msec 5000
            #:on-timeout (lambda ()
                           (log-warning "ARP lookup of ~a failed, packet dropped"
                                        (ip-address->hostname ipaddr))
                           '())))
