#lang racket/base

(require racket/match)
(require (prefix-in udp: racket/udp))
(require "../main.rkt")
(require "../demand-matcher.rkt")

(provide (struct-out udp-remote-address)
	 (struct-out udp-handle)
	 (struct-out udp-listener)
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

;; A UdpPacket is a (udp-packet UdpAddress UdpAddress Bytes), and
;; represents a packet appearing on our local "subnet" of the full UDP
;; network, complete with source, destination and contents.
(struct udp-packet (source destination body) #:prefab)

;; -> Action
;; Spawns a process acting as a UDP socket factory.
(define (spawn-udp-driver)
  (spawn-demand-matcher (observe (udp-packet ? (?! (udp-listener ?)) ?))
                        (advertise (udp-packet ? (?! (udp-listener ?)) ?))
                        spawn-udp-socket))

;; UdpLocalAddress -> Action
(define (spawn-udp-socket local-addr)
  (define socket (udp:udp-open-socket #f #f))

  (match local-addr
    [(udp-listener port) (udp:udp-bind! socket #f port)]
    [(udp-handle _) (udp:udp-bind! socket #f 0)]) ;; kernel-allocated port number

  (define control-ch (make-channel))
  (thread (lambda () (udp-receiver-thread local-addr socket control-ch)))

  (spawn (lambda (e s)
	   (match e
             [(? patch? p)
              (cond [(trie-empty? (patch-removed p)) #f] ;; peer hasn't quit yet: do nothing.
                    [else (channel-put control-ch 'quit)
                          (quit)])]
	     [(message (at-meta (? udp-packet? p)))
	      (transition s (message p))]
	     [(message (udp-packet _ (udp-remote-address host port) body))
	      (udp:udp-send-to socket host port body)
	      #f]
	     [_ #f]))
	 (void)
         (patch-seq (sub (udp-packet ? local-addr ?) #:meta-level 1)
                    (sub (udp-packet local-addr (udp-remote-address ? ?) ?))
                    (pub (udp-packet (udp-remote-address ? ?) local-addr ?))
                    (sub (observe (udp-packet (udp-remote-address ? ?) local-addr ?))))))

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
