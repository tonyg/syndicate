#lang syndicate

(provide (struct-out ip-packet)
	 ip-address->hostname
	 ip-string->ip-address
	 apply-netmask
	 ip-address-in-subnet?
         query-local-ip-addresses
	 broadcast-ip-address
	 spawn-ip-driver)

(require racket/set)
(require (only-in racket/string string-split))
(require bitsyntax)
(require syndicate/protocol/advertise)

(require "dump-bytes.rkt")
(require "configuration.rkt")
(require "checksum.rkt")

(require/activate syndicate/drivers/timer)
(require/activate "ethernet.rkt")
(require/activate "arp.rkt")

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

(define (query-local-ip-addresses)
  (query-set local-ips (host-route $addr _ _) addr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spawn-ip-driver)
  (spawn #:name 'ip-driver
   (during/spawn (host-route $my-address $netmask $interface-name)
                 (assert (route-up (host-route my-address netmask interface-name)))
                 (do-host-route my-address netmask interface-name))
   (during/spawn (gateway-route $network $netmask $gateway-addr $interface-name)
                 (assert (route-up
                          (gateway-route $network $netmask $gateway-addr $interface-name)))
                 (do-gateway-route network netmask gateway-addr interface-name))
   (during/spawn (net-route $network-addr $netmask $link)
                 (assert (route-up (net-route network-addr netmask link)))
                 (do-net-route network-addr netmask link))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local IP route

(define (do-host-route my-address netmask interface-name)
  (let ((network-addr (apply-netmask my-address netmask)))
    (do-normal-ip-route (host-route my-address netmask interface-name)
                        network-addr
                        netmask
                        interface-name))

  (assert (advertise (ip-packet _ my-address _ PROTOCOL-ICMP _ _)))
  (assert (arp-assertion IPv4-ethertype my-address interface-name))
  (on (message (ip-packet _ $peer-address my-address PROTOCOL-ICMP _ $body))
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
            (send! (ip-packet #f
                              my-address
                              peer-address
                              PROTOCOL-ICMP
                              #""
                              (ip-checksum 2 reply-data0)))]
           [else
            (log-info "ICMP ~a/~a (cksum ~a) to ~a from ~a:\n~a"
                      type
                      code
                      checksum
                      (pretty-bytes my-address)
                      (pretty-bytes peer-address)
                      (dump-bytes->string rest))]))
        (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gateway IP route

(struct gateway-route-state (routes gateway-interface gateway-hwaddr) #:transparent)

(define (do-gateway-route network netmask gateway-addr interface-name)
  (define the-route (gateway-route network netmask gateway-addr interface-name))

  (field [routes (set)])
  (query-set* routes (host-route $addr $netmask _) (list addr netmask))
  (query-set* routes (gateway-route $addr $netmask _ _) (list addr netmask))
  (query-set* routes (net-route $addr $netmask _) (list addr netmask))

  (field [gateway-interface #f]
         [gateway-hwaddr #f])
  (on (asserted (arp-query IPv4-ethertype
                           gateway-addr
                           ($ iface (ethernet-interface interface-name _))
                           $hwaddr))
      (when (not (gateway-hwaddr))
        (log-info "Discovered gateway ~a at ~a on interface ~a."
                  (ip-address->hostname gateway-addr)
                  (ethernet-interface-name iface)
                  (pretty-bytes hwaddr)))
      (gateway-interface iface)
      (gateway-hwaddr hwaddr))

  (define (covered-by-some-other-route? addr)
    (for/or ([r (in-set (routes))])
      (match-define (list net msk) r)
      (and (positive? msk)
	   (ip-address-in-subnet? addr net msk))))

  (on (message ($ p (ip-packet _ _ _ _ _ _)))
      (when (not (gateway-interface))
        (log-warning "Gateway hwaddr for ~a not known, packet dropped."
                     (ip-address->hostname gateway-addr)))
      (when (and (gateway-interface)
                 (not (equal? (ip-packet-source-interface p)
                              (ethernet-interface-name (gateway-interface))))
                 (not (covered-by-some-other-route? (ip-packet-destination p))))
        (send! (ethernet-packet (gateway-interface)
                                #f
                                (ethernet-interface-hwaddr (gateway-interface))
                                (gateway-hwaddr)
                                IPv4-ethertype
                                (format-ip-packet p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General net route

(define (do-net-route network-addr netmask link)
  (do-normal-ip-route (net-route network-addr netmask link) network-addr netmask link))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal IP route

(define (do-normal-ip-route the-route network netmask interface-name)
  (assert (arp-interface interface-name))
  (on (message (ethernet-packet (ethernet-interface interface-name _) #t _ _ IPv4-ethertype $body))
      (define p (parse-ip-packet interface-name body))
      (when p (send! p)))
  (on (message ($ p (ip-packet _ _ _ _ _ _)))
      (define destination (ip-packet-destination p))
      (when (and (not (equal? (ip-packet-source-interface p) interface-name))
                 (ip-address-in-subnet? destination network netmask))
        (define timer-id (gensym 'ippkt))
        (react (on-start (send! (set-timer timer-id 5000 'relative)))
               (stop-when (message (timer-expired timer-id _))
                          (log-warning "ARP lookup of ~a failed, packet dropped"
                                       (ip-address->hostname destination)))
               (stop-when (asserted (arp-query IPv4-ethertype
                                               destination
                                               ($ interface (ethernet-interface interface-name _))
                                               $destination-hwaddr))
                          (send! (ethernet-packet interface
                                                  #f
                                                  (ethernet-interface-hwaddr interface)
                                                  destination-hwaddr
                                                  IPv4-ethertype
                                                  (format-ip-packet p))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-ip-driver)
