#lang syndicate/actor

(provide (struct-out udp-remote-address)
	 (struct-out udp-handle)
	 (struct-out udp-listener)
	 udp-address?
	 udp-local-address?
	 (struct-out udp-packet)
	 spawn-udp-driver)

(require racket/set)
(require bitsyntax)
(require syndicate/protocol/advertise)

(require "dump-bytes.rkt")
(require "checksum.rkt")
(require "configuration.rkt")
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

;; USER-level protocol
(struct udp-packet (source destination body) #:prefab)

;; KERNEL-level protocol
(struct udp-datagram (source-ip source-port destination-ip destination-port body) #:prefab)
(struct udp-port-allocation (port handle) #:prefab) ;; (udp-port-allocation Number UdpLocalAddress)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-accessible driver startup

(define (spawn-udp-driver)
  (spawn-port-allocator 'udp (lambda () (query-set udp-ports (udp-port-allocation $p _) p)))
  (spawn-kernel-udp-driver)
  (actor #:name 'udp-driver
   (on (asserted (observe (udp-packet _ ($ h (udp-listener _)) _)))
       (spawn-udp-relay (udp-listener-port h) h))
   (on (asserted (observe (udp-packet _ ($ h (udp-handle _)) _)))
       (actor #:name (list 'udp-transient h)
              (on-start (spawn-udp-relay (allocate-port! 'udp) h))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relaying

(define (spawn-udp-relay local-port local-user-addr)
  (actor #:name (list 'udp-relay local-port local-user-addr)
         (on-start (log-info "Spawning UDP relay ~v / ~v" local-port local-user-addr))

         (define any-remote (udp-remote-address ? ?))

         (stop-when (retracted (observe (udp-packet any-remote local-user-addr _))))
         (assert (advertise (udp-packet any-remote local-user-addr _)))
         (assert (udp-port-allocation local-port local-user-addr))

         (during (host-route $ip _ _)
                 (assert (advertise (udp-datagram ip local-port _ _ _)))
                 (on (message (udp-datagram $source-ip $source-port ip local-port $bs))
                     (send!
                      (udp-packet (udp-remote-address (ip-address->hostname source-ip)
                                                      source-port)
                                  local-user-addr
                                  bs))))

         (define local-ips (query-local-ip-addresses))
         (on (message (udp-packet local-user-addr ($ remote-addr any-remote) $bs))
             ;; Choose arbitrary local IP address for outbound packet!
             ;; TODO: what can be done? Must I examine the routing table?
             (match-define (udp-remote-address remote-host remote-port) remote-addr)
             (define remote-ip (ip-string->ip-address remote-host))
             (send! (udp-datagram (set-first (local-ips))
                                  local-port
                                  remote-ip
                                  remote-port
                                  bs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codec & kernel-level driver

(define PROTOCOL-UDP 17)

(define (spawn-kernel-udp-driver)
  (actor #:name 'kernel-udp-driver
         (assert (advertise (ip-packet #f _ _ PROTOCOL-UDP _ _)))

         (define local-ips (query-local-ip-addresses))

         (on (message (ip-packet $source-if $src-ip $dst-ip PROTOCOL-UDP _ $body))
             (when (and source-if (set-member? (local-ips) dst-ip))
               (bit-string-case body
                 ([ (src-port :: integer bytes 2)
                    (dst-port :: integer bytes 2)
                    (length :: integer bytes 2)
                    (checksum :: integer bytes 2) ;; TODO: check checksum
                    (data :: binary) ]
                  (bit-string-case data
                    ([ (payload :: binary bytes (- length 8)) ;; min UDP header size is 8 bytes
                       (:: binary) ]
                     (send! (udp-datagram src-ip src-port dst-ip dst-port
                                          (bit-string->bytes payload))))
                    (else #f)))
                 (else #f))))

         (on (message (udp-datagram $src-ip $src-port $dst-ip $dst-port $bs))
             (when (set-member? (local-ips) src-ip)
               (let* ((payload (bit-string (src-port :: integer bytes 2)
                                           (dst-port :: integer bytes 2)
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
                 (send! (ip-packet #f src-ip dst-ip PROTOCOL-UDP #""
                                   checksummed-payload)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-udp-driver)
