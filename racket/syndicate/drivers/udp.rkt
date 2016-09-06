#lang syndicate

(require (prefix-in udp: racket/udp))
(require syndicate/demand-matcher)
(require syndicate/protocol/advertise)

(provide (struct-out udp-remote-address)
	 (struct-out udp-handle)
	 (struct-out udp-listener)
         (struct-out udp-multicast-group-member)
         (struct-out udp-multicast-loopback)
	 udp-address?
	 udp-local-address?
	 (struct-out udp-packet)
	 spawn-udp-driver)

;; A UdpAddress is one of
;; -- a (udp-remote-address String Uint16), representing a remote socket
;; -- a (udp-handle Any), representing a local socket on a kernel-assigned port
;; -- a (udp-listener Uint16), representing a local socket on a user-assigned port
;; Note that udp-handle-ids must be chosen carefully: they are scoped
;; to the local VM, i.e. shared between processes in that VM, so
;; processes must make sure not to accidentally clash in handle ID
;; selection.
(struct udp-remote-address (host port) #:prefab)
(struct udp-handle (id) #:prefab)
(struct udp-listener (port) #:prefab)

(define (udp-address? x)
  (or (udp-remote-address? x)
      (udp-local-address? x)))

(define (udp-local-address? x)
  (or (udp-handle? x)
      (udp-listener? x)))

;; A UdpMembership is a (udp-multicast-group-member UdpLocalAddress String String),
;; where the latter two arguments correspond to the last two arguments
;; of `udp-multicast-join-group!`.
(struct udp-multicast-group-member (local-address group-address interface) #:prefab)

;; A UdpLoopback is a (udp-multicast-loopback UdpLocalAddress Boolean).
(struct udp-multicast-loopback (local-address enabled?) #:prefab)

;; A UdpPacket is a (udp-packet UdpAddress UdpAddress Bytes), and
;; represents a packet appearing on our local "subnet" of the full UDP
;; network, complete with source, destination and contents.
(struct udp-packet (source destination body) #:prefab)

;; -> Action
;; Spawns a process acting as a UDP socket factory.
(define (spawn-udp-driver)
  (spawn-demand-matcher #:name 'udp-driver
                        (observe (udp-packet ? (?! (udp-listener ?)) ?))
                        (advertise (udp-packet ? (?! (udp-listener ?)) ?))
                        spawn-udp-socket))

;; UdpLocalAddress -> Action
(define (spawn-udp-socket local-addr)
  (define socket (udp:udp-open-socket #f #f))

  (match local-addr
    [(udp-listener port) (udp:udp-bind! socket #f port #t)]
    [(udp-handle _) (udp:udp-bind! socket #f 0)]) ;; kernel-allocated port number

  (define control-ch (make-channel))
  (thread (lambda () (udp-receiver-thread local-addr socket control-ch)))

  (define peer-interest (observe (udp-packet (udp-remote-address ? ?) local-addr ?)))
  (define (peer-quit? p) (not (trie-empty? (trie-project (patch-removed p) (?! peer-interest)))))

  (define (update-multicast! p)
    (for-trie ([(udp-multicast-group-member _ $group $interface) (patch-removed p)])
      (udp:udp-multicast-leave-group! socket group interface))
    (for-trie ([(udp-multicast-group-member _ $group $interface) (patch-added p)])
      (udp:udp-multicast-join-group! socket group interface))
    (for-trie ([(udp-multicast-loopback _ $enabled?) (patch-added p)])
      (udp:udp-multicast-set-loopback! socket enabled?)))

  (spawn #:name (list 'udp-socket local-addr)
         (lambda (e s)
	   (match e
             [(? patch? p)
              (update-multicast! p)
              (when (peer-quit? p)
                (channel-put control-ch 'quit)
                (quit))]
	     [(message (inbound (? udp-packet? p)))
	      (transition s (message p))]
	     [(message (udp-packet _ (udp-remote-address host port) body))
	      (udp:udp-send-to socket host port body)
	      #f]
	     [_ #f]))
	 (void)
         (patch-seq (sub (inbound (udp-packet ? local-addr ?)))
                    (sub (udp-packet local-addr (udp-remote-address ? ?) ?))
                    (pub (udp-packet (udp-remote-address ? ?) local-addr ?))
                    (sub (udp-multicast-group-member local-addr ? ?))
                    (sub (udp-multicast-loopback local-addr ?))
                    (sub peer-interest))))

;; UdpLocalAddress UdpSocket Channel -> Void
(define (udp-receiver-thread local-addr socket control-ch)
  (define buffer (make-bytes 65536))
  (let loop ()
    (sync (handle-evt control-ch (match-lambda ['quit (void)]))
	  (handle-evt (udp:udp-receive!-evt socket buffer)
		      (lambda (receive-results)
			(match-define (list len source-hostname source-port) receive-results)
			(send-ground-message
			 (udp-packet (udp-remote-address source-hostname source-port)
				     local-addr
				     (subbytes buffer 0 len)))
			(loop)))))
  (udp:udp-close socket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-udp-driver)
