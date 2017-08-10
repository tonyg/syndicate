#lang syndicate
;; ARP protocol, http://tools.ietf.org/html/rfc826
;; Only does ARP-over-ethernet.

(provide (struct-out arp-query)
	 (struct-out arp-assertion)
	 (struct-out arp-interface)
         spawn-arp-driver)

(require racket/set)
(require racket/match)
(require/activate syndicate/drivers/timer)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "configuration.rkt")
(require/activate "ethernet.rkt")

(struct arp-query (protocol protocol-address interface link-address) #:prefab)
(struct arp-assertion (protocol protocol-address interface-name) #:prefab)
(struct arp-interface (interface-name) #:prefab)

(struct arp-interface-up (interface-name) #:prefab)

(define ARP-ethertype #x0806)
(define cache-entry-lifetime-msec (* 14400 1000))
(define wakeup-interval 5000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spawn-arp-driver)
  (spawn #:name 'arp-driver
         (during/spawn (arp-interface $interface-name)
                       #:name (list 'arp-interface interface-name)
                       (assert (arp-interface-up interface-name))
                       (on-start (define hwaddr (lookup-ethernet-hwaddr interface-name))
                                 (when (not hwaddr)
                                   (error 'arp "Failed to look up ARP interface ~v"
                                          interface-name))
                                 (react (run-arp-interface interface-name hwaddr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct cache-key (protocol address) #:transparent)
(struct cache-value (expiry interface address) #:transparent)

(define (expire-cache c)
  (define now (current-inexact-milliseconds))
  (define (not-expired? v) (< now (cache-value-expiry v)))
  (for/hash [((k v) (in-hash c)) #:when (not-expired? v)]
    (values k v)))

(define (run-arp-interface interface-name hwaddr)
  (log-info "ARP interface ~v ~v" interface-name hwaddr)
  (define interface (ethernet-interface interface-name hwaddr))

  (define (build-packet dest-mac ptype oper sender-ha sender-pa target-ha target-pa)
    (define hlen (bytes-length target-ha))
    (define plen (bytes-length target-pa))
    (define packet (bit-string->bytes
		    (bit-string (1 :: integer bytes 2)
				(ptype :: integer bytes 2)
				hlen
				plen
				(oper :: integer bytes 2)
				(sender-ha :: binary bytes hlen)
				(sender-pa :: binary bytes plen)
				(target-ha :: binary bytes hlen)
				(target-pa :: binary bytes plen))))
    (ethernet-packet interface
		     #f
		     hwaddr
		     dest-mac
		     ARP-ethertype
		     packet))

  (define (some-asserted-pa ptype)
    (match (filter (lambda (k) (equal? (cache-key-protocol k) ptype)) (set->list (assertions)))
      ['() #f]
      [(list* k _) (cache-key-address k)]))

  (define (send-questions!)
    (for [(q (set-subtract (queries) (list->set (hash-keys (cache)))))]
      (define pa (some-asserted-pa (cache-key-protocol q)))
      (log-info "~a ARP Asking for ~a from ~a"
                interface-name
                (pretty-bytes (cache-key-address q))
                (and pa (pretty-bytes pa)))
      (when pa
        (send! (build-packet broadcast-ethernet-address
                             (cache-key-protocol q)
                             1 ;; request
                             hwaddr
                             pa
                             zero-ethernet-address
                             (cache-key-address q))))))

  (field [cache (hash)]
         [queries (set)]
         [assertions (set)])

  (on-start (define timer-key (list 'arp interface-name))
            (define (arm-timer!) (send! (set-timer timer-key wakeup-interval 'relative)))
            (arm-timer!)
            (react (on (message (timer-expired timer-key _))
                       (cache (expire-cache (cache)))
                       (send-questions!)
                       (arm-timer!))))

  (on (message ($ p (ethernet-packet-pattern interface-name #t ARP-ethertype)))
      (match-define (ethernet-packet _ _ source destination _ body) p)
      (bit-string-case body
         ([ (= 1 :: integer bytes 2)
            (ptype :: integer bytes 2)
            hlen
            plen
            (oper :: integer bytes 2)
            (sender-hardware-address0 :: binary bytes hlen)
            (sender-protocol-address0 :: binary bytes plen)
            (target-hardware-address0 :: binary bytes hlen)
            (target-protocol-address0 :: binary bytes plen)
            (:: binary) ;; The extra zeros exist because ethernet packets
                        ;; have a minimum size. This is, in part, why IPv4
                        ;; headers have a total-length field, so that the
                        ;; zero padding can be removed.
            ]
          (let ()
            (define sender-protocol-address (bit-string->bytes sender-protocol-address0))
            (define sender-hardware-address (bit-string->bytes sender-hardware-address0))
            (define target-protocol-address (bit-string->bytes target-protocol-address0))
            (define learned-key (cache-key ptype sender-protocol-address))

            (when (and (set-member? (queries) learned-key) ;; it is relevant to our interests
                       (not (equal? sender-hardware-address
                                    (cache-value-address (hash-ref (cache)
                                                                   learned-key
                                                                   (lambda ()
                                                                     (cache-value #f #f #f)))))))
              (log-info "~a ARP Adding ~a = ~a to cache"
                        interface-name
                        (pretty-bytes sender-protocol-address)
                        (pretty-bytes sender-hardware-address)))

            (cache (hash-set (expire-cache (cache))
                             learned-key
                             (cache-value (+ (current-inexact-milliseconds)
                                             cache-entry-lifetime-msec)
                                          interface
                                          sender-hardware-address)))
            (case oper
              [(1) ;; request
               (when (set-member? (assertions) (cache-key ptype target-protocol-address))
                 (log-info "~a ARP answering request for ~a/~a"
                           interface-name
                           ptype
                           (pretty-bytes target-protocol-address))
                 (send! (build-packet sender-hardware-address
                                      ptype
                                      2 ;; reply
                                      hwaddr
                                      target-protocol-address
                                      sender-hardware-address
                                      sender-protocol-address)))]
              [(2) (void)] ;; reply
              [else (void)])))
         (else #f)))

  (during (arp-assertion $protocol $protocol-address interface-name)
          (define a (cache-key protocol protocol-address))
          (on-start (assertions (set-add (assertions) a))
                    (log-info "~a ARP Announcing ~a as ~a"
                              interface-name
                              (pretty-bytes (cache-key-address a))
                              (pretty-bytes hwaddr))
                    (send! (build-packet broadcast-ethernet-address
                                         (cache-key-protocol a)
                                         2 ;; reply -- gratuitous announcement
                                         hwaddr
                                         (cache-key-address a)
                                         hwaddr
                                         (cache-key-address a))))
          (on-stop (assertions (set-remove (assertions) a))))

  (during (observe (arp-query $protocol $protocol-address interface _))
          (define key (cache-key protocol protocol-address))
          (on-start (queries (set-add (queries) key))
                    (send-questions!))
          (on-stop (queries (set-remove (queries) key)))
          (assert #:when (hash-has-key? (cache) key)
                  (match (hash-ref (cache) key)
                    [(cache-value _ ifname addr)
                     (arp-query protocol protocol-address ifname addr)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-arp-driver)
