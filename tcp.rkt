#lang racket/base

(provide (struct-out tcp-address)
	 (struct-out tcp-handle)
	 (struct-out tcp-listener)
	 (struct-out tcp-channel)
	 spawn-tcp-driver)

(require racket/set)
(require racket/match)
(require minimart)
(require minimart/drivers/timer)
(require minimart/demand-matcher)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "checksum.rkt")
(require "ip.rkt")

;; tcp-address/tcp-address : "kernel" tcp connection state machines
;; tcp-handle/tcp-address : "user" outbound connections
;; tcp-listener/tcp-address : "user" inbound connections

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol messages

(struct tcp-address (host port) #:prefab)
(struct tcp-handle (id) #:prefab)
(struct tcp-listener (port) #:prefab)

(struct tcp-channel (source destination subpacket) #:prefab)

(struct tcp-packet (from-wire?
		    source-ip
		    source-port
		    destination-ip
		    destination-port
		    sequence-number
		    ack-number
		    flags
		    window-size
		    options
		    data)
	#:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-accessible driver startup

(define (spawn-tcp-driver)
  (list (spawn-demand-matcher (tcp-channel ? (?! (tcp-listener ?)) ?)
			      #:demand-is-subscription? #t
			      #:demand-level 1
			      #:supply-level 2
			      (lambda (server-addr)
				(match-define (tcp-listener port) server-addr)
				(spawn-demand-matcher
				 (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-address ? port)) ?)
				 (spawn-relay server-addr))))
	(spawn-demand-matcher (tcp-channel (?! (tcp-handle ?)) (?! (tcp-address ? ?)) ?)
			      (lambda (local-addr remote-addr)
				(send (tcp-port-allocation-request local-addr remote-addr))))
	(spawn-port-allocator)
	(spawn-kernel-tcp-driver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Port allocator

(struct tcp-port-allocation-request (local-addr remote-addr) #:prefab)

(struct port-allocator-state (used-ports local-ips) #:transparent)

(define (spawn-port-allocator)
  (define port-projector
    (compile-gestalt-projection (tcp-channel (tcp-address (?!) (?!)) (tcp-address (?!) (?!)) ?)))
  (define ip-projector
    (compile-gestalt-projection (ip-interface (?!) ?)))

  ;; TODO: Choose a sensible IP address for the outbound connection.
  ;; We don't have enough information to do this well at the moment,
  ;; so just pick some available local IP address.
  ;;
  ;; Interesting note: In some sense, the right answer is "?". This
  ;; would give us a form of mobility, where IP addresses only route
  ;; to a given bucket-of-state and ONLY the port number selects a
  ;; substate therein. That's not how TCP is defined however so we
  ;; can't do that.
  (define (appropriate-ip s)
    (set-first (port-allocator-state-local-ips s)))

  (spawn (lambda (e s)
	   (match e
	     [(routing-update g)
	      (define extracted-ips (matcher-key-set (gestalt-project g 0 0 #t ip-projector)))
	      (define extracted-ports (matcher-key-set (gestalt-project g 0 0 #f port-projector)))
	      (if (or (not extracted-ports) (not extracted-ips))
		  (error 'tcp "Someone has published a wildcard TCP address or IP interface")
		  (transition (let ((local-ips (for/set [(e (in-set extracted-ips))] (car e))))
				(port-allocator-state
				 (for/fold [(s (set))] [(e (in-set extracted-ports))]
				   (match-define (list si sp di dp) e)
				   (let* ((s (if (set-member? local-ips si) (set-add s sp) s))
					  (s (if (set-member? local-ips di) (set-add s dp) s)))
				     s))
				 local-ips))
			      '()))]
	     [(message (tcp-port-allocation-request local-addr remote-addr) _ _)
	      (define currently-used-ports (port-allocator-state-used-ports s))
	      (let randomly-allocate-until-unused ()
		(define p (+ 1024 (random 64512)))
		(if (set-member? currently-used-ports p)
		    (randomly-allocate-until-unused)
		    (transition (struct-copy port-allocator-state s
				  [used-ports (set-add currently-used-ports p)])
				((spawn-relay local-addr)
				 remote-addr
				 (tcp-channel (appropriate-ip s) p)))))]
	     [_ #f]))
	 (port-allocator-state (set) (set))
	 (gestalt-union (sub (tcp-port-allocation-request ? ?))
			(sub (projection->pattern ip-projector) #:level 1)
			(pub (projection->pattern port-projector) #:level 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relay between kernel-level and user-level

(define ((spawn-relay local-user-addr) remote-addr local-tcp-addr)
  (define local-peer-traffic (pub (tcp-channel remote-addr local-user-addr ?) #:level 1))
  (define remote-peer-traffic (sub (tcp-channel remote-addr local-tcp-addr ?) #:level 1))
  (spawn (lambda (e seen-local-peer?)
	   (local-require racket/pretty)
	   (pretty-write `(RELAY (local-user-addr ,local-user-addr)
				 (remote-addr ,remote-addr)
				 (local-tcp-addr ,local-tcp-addr)
				 (seen-local-peer? ,seen-local-peer?)
				 (e ,e)))
	   (flush-output)
	   (match e
	     [(routing-update g)
	      (define local-peer-absent? (gestalt-empty? (gestalt-filter g local-peer-traffic)))
	      (transition (or seen-local-peer? (not local-peer-absent?))
			  (when (or (and seen-local-peer? local-peer-absent?)
				    (gestalt-empty? (gestalt-filter g remote-peer-traffic)))
			    (quit)))]
	     [(message (tcp-channel (== local-user-addr) (== remote-addr) bs) _ _)
	      (transition seen-local-peer? (send (tcp-channel local-tcp-addr remote-addr bs)))]
	     [(message (tcp-channel (== remote-addr) (== local-tcp-addr) bs) _ _)
	      (transition seen-local-peer? (send (tcp-channel remote-addr local-user-addr bs)))]
	     [_ #f]))
	 #f
	 (gestalt-union local-peer-traffic
			remote-peer-traffic
			(sub (tcp-channel remote-addr local-tcp-addr ?))
			(sub (tcp-channel local-user-addr remote-addr ?))
			(pub (tcp-channel remote-addr local-user-addr ?))
			(pub (tcp-channel local-tcp-addr remote-addr ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codec & kernel-level driver

(define PROTOCOL-TCP 6)

(struct codec-state (active-state-vectors) #:transparent)

(define (spawn-kernel-tcp-driver)

  (define (flip-statevec statevec)
    (match-define (list si sp di dp) statevec)
    (list di dp si sp))

  (define (state-vector-active? statevec s)
    (or (set-member? (codec-state-active-state-vectors s) statevec)
	(set-member? (codec-state-active-state-vectors s) (flip-statevec statevec))))

  (define (analyze-incoming-packet src-ip dst-ip body s)
    (bit-string-case body
      ([ (src-port :: integer bytes 2)
	 (dst-port :: integer bytes 2)
	 (sequence-number :: integer bytes 4)
	 (ack-number :: integer bytes 4)
	 (data-offset :: integer bits 4)
	 (reserved :: integer bits 3)
	 (ns :: integer bits 1)
	 (cwr :: integer bits 1)
	 (ece :: integer bits 1)
	 (urg :: integer bits 1)
	 (ack :: integer bits 1)
	 (psh :: integer bits 1)
	 (rst :: integer bits 1)
	 (syn :: integer bits 1)
	 (fin :: integer bits 1)
	 (window-size :: integer bytes 2)
	 (checksum :: integer bytes 2) ;; TODO: check checksum
	 (urgent-pointer :: integer bytes 2)
	 (rest :: binary) ]
       (let* ((flags (set))
	      (statevec (list src-ip src-port dst-ip dst-port))
	      (old-active-state-vectors (codec-state-active-state-vectors s))
	      (spawn-needed? (and (not (state-vector-active? statevec s))
				  (zero? rst)))) ;; don't bother spawning if it's a rst
	 (define-syntax-rule (set-flags! v ...)
	   (begin (unless (zero? v) (set! flags (set-add flags 'v))) ...))
	 (set-flags! ns cwr ece urg ack psh rst syn fin)
	 (log-info "TCP ~a:~a -> ~a:~a (seq ~a, ack ~a, flags ~a, window ~a)"
		   (ip-address->hostname src-ip)
		   src-port
		   (ip-address->hostname dst-ip)
		   dst-port
		   sequence-number
		   ack-number
		   flags
		   window-size)
	 (when spawn-needed? (log-info "  - spawn needed!"))
	 (bit-string-case rest
	   ([ (opts :: binary bytes (- (* data-offset 4) 20))
	      (data :: binary) ]
	    (let ((packet (tcp-packet #t
				      src-ip
				      src-port
				      dst-ip
				      dst-port
				      sequence-number
				      ack-number
				      flags
				      window-size
				      (bit-string->bytes opts)
				      (bit-string->bytes data))))
	      (transition (if spawn-needed?
			      (struct-copy codec-state s
				[active-state-vectors
				 (set-add old-active-state-vectors statevec)])
			      s)
			  (list
			   (when spawn-needed? (spawn-state-vector src-ip src-port
								   dst-ip dst-port))
			   ;; TODO: get packet to the new state-vector process somehow
			   (send packet)))))
	   (else #f))))
      (else #f)))

  (define statevec-projection
    (compile-gestalt-projection
     (tcp-packet ? (?!) (?!) (?!) (?!) ? ? ? ? ? ?)))

  (define (analyze-gestalt g s)
    (define statevecs (matcher-key-set (gestalt-project g 0 0 #f statevec-projection)))
    (log-info "gestalt yielded statevecs ~v" statevecs)
    (transition (struct-copy codec-state s [active-state-vectors statevecs]) '()))

  (define (deliver-outbound-packet p s)
    (match-define (tcp-packet #f
			      src-ip
			      src-port
			      dst-ip
			      dst-port
			      sequence-number
			      ack-number
			      flags
			      window-size
			      options
			      data)
      p)
    (log-info "TCP ~a:~a -> ~a:~a (seq ~a, ack ~a, flags ~a, window ~a)"
	      (ip-address->hostname src-ip)
	      src-port
	      (ip-address->hostname dst-ip)
	      dst-port
	      sequence-number
	      ack-number
	      flags
	      window-size)
    (define (flag-bit sym) (if (set-member? flags sym) 1 0))
    (define payload (bit-string (src-port :: integer bytes 2)
				(dst-port :: integer bytes 2)
				(sequence-number :: integer bytes 4)
				(ack-number :: integer bytes 4)
				((+ 5 (quotient (bit-string-byte-count options) 4))
				 :: integer bits 4) ;; TODO: enforce 4-byte alignment
				(0 :: integer bits 3)
				((flag-bit 'ns) :: integer bits 1)
				((flag-bit 'cwr) :: integer bits 1)
				((flag-bit 'ece) :: integer bits 1)
				((flag-bit 'urg) :: integer bits 1)
				((flag-bit 'ack) :: integer bits 1)
				((flag-bit 'psh) :: integer bits 1)
				((flag-bit 'rst) :: integer bits 1)
				((flag-bit 'syn) :: integer bits 1)
				((flag-bit 'fin) :: integer bits 1)
				(window-size :: integer bytes 2)
				(0 :: integer bytes 2) ;; checksum location
				(0 :: integer bytes 2) ;; TODO: urgent pointer
				(data :: binary)))
    (define pseudo-header (bit-string (src-ip :: binary bytes 4)
				      (dst-ip :: binary bytes 4)
				      0
				      PROTOCOL-TCP
				      ((bit-string-byte-count payload) :: integer bytes 2)))
    (transition s (send (ip-packet src-ip dst-ip PROTOCOL-TCP #""
				   (ip-checksum 16 payload #:pseudo-header pseudo-header)))))

  (spawn (lambda (e s)
	   (match e
	     [(routing-update g)
	      (analyze-gestalt g s)]
	     [(message (ip-packet src dst _ _ body) _ _)
	      (analyze-incoming-packet src dst body s)]
	     [(message (? tcp-packet? p) _ _)
	      #:when (not (tcp-packet-from-wire? p))
	      (deliver-outbound-packet p s)]
	     [_ #f]))
	 (codec-state (set))
	 (gestalt-union (pub (ip-packet ? ? PROTOCOL-TCP ? ?))
			(sub (ip-packet ? ? PROTOCOL-TCP ? ?))
			(sub (tcp-packet #f ? ? ? ? ? ? ? ? ? ?))
			(pub (tcp-packet #t ? ? ? ? ? ? ? ? ? ?))
			(pub (tcp-packet #t ? ? ? ? ? ? ? ? ? ?)
			     #:level 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per-connection state vector process

(struct buffer (data ;; bit-string
		seqn ;; names leftmost byte in data
		window ;; counts bytes from leftmost byte in data
		finished?) ;; boolean: true after FIN
	#:transparent)

(struct conn-state (outbound ;; buffer
		    inbound ;; buffer
		    syn-acked? ;; boolean
		    latest-activity-time ;; from current-inexact-milliseconds
		    local-peer-seen?) ;; boolean
	#:transparent)

(define transmit-check-interval-msec 100)
(define inbound-buffer-limit 65535)
(define maximum-segment-size 536) ;; bytes
(define maximum-segment-lifetime-sec (* 2 60)) ;; two minutes; 2MSL is TIME-WAIT timeout

(define (spawn-state-vector src-ip src-port dst-ip dst-port)
  (define src (tcp-address (ip-address->hostname src-ip) src-port))
  (define dst (tcp-address (ip-address->hostname dst-ip) dst-port))
  (define (timer-name kind) (list 'tcp-timer kind src dst))

  (define (next-expected-seqn s)
    (define b (conn-state-inbound s))
    (define v (buffer-seqn b))
    (and v (seq+ v (bit-string-byte-count (buffer-data b)))))

  (define (buffer-push b data)
    (struct-copy buffer b [data (bit-string-append (buffer-data b) data)]))

  ;; ConnState -> ConnState
  (define (set-inbound-seqn seqn s)
    (struct-copy conn-state s
      [inbound (struct-copy buffer (conn-state-inbound s) [seqn seqn])]))

  ;; Bitstring ConnState -> Transition
  (define (incorporate-segment data s)
    (transition
     (if (buffer-finished? (conn-state-inbound s))
	 s
	 (struct-copy conn-state s [inbound (buffer-push (conn-state-inbound s) data)]))
     '()))

  (define (seq+ a b) (bitwise-and #xffffffff (+ a b)))

  ;; Always positive
  (define (seq- larger smaller)
    (if (< larger smaller) ;; wraparound has occurred
	(+ (- larger smaller) #x100000000)
	(- larger smaller)))

  (define (seq> a b)
    (< (seq- a b) #x80000000))

  ;; ConnState -> Gestalt
  (define (compute-gestalt s)
    (gestalt-union (sub (timer-expired (timer-name ?) ?))
		   (sub (tcp-packet #t src-ip src-port dst-ip dst-port ? ? ? ? ? ?))
		   (pub (tcp-packet #f dst-ip dst-port src-ip src-port ? ? ? ? ? ?))
		   (sub (tcp-channel dst src ?))
		   (if (not (buffer-finished? (conn-state-inbound s)))
		       (pub (tcp-channel src dst ?))
		       (gestalt-empty))
		   (pub (tcp-channel src dst ?) #:level 1)))

  ;; ConnState -> Transition
  (define (deliver-inbound-locally s)
    (define b (conn-state-inbound s))
    (if (bit-string-empty? (buffer-data b))
	(transition s '())
	(let ((chunk (bit-string->bytes (buffer-data b))))
	  (transition (struct-copy conn-state s
			[inbound (struct-copy buffer b
				   [data #""]
				   [seqn (seq+ (buffer-seqn b) (bytes-length chunk))])])
		      (send (tcp-channel src dst chunk))))))

  ;; (Setof Symbol) -> ConnState -> Transition
  (define ((check-fin flags) s)
    (define b (conn-state-inbound s))
    (unless (bit-string-empty? (buffer-data b)) ;; assured by deliver-inbound-locally
      (error 'check-fin "Nonempty inbound buffer"))
    (if (set-member? flags 'fin)
	(let ((new-s (struct-copy conn-state s
		       [inbound (struct-copy buffer b
				  [seqn (seq+ (buffer-seqn b) 1)] ;; reliable: count fin as a byte
				  [finished? #t])])))
	  (transition new-s (routing-update (compute-gestalt new-s))))
	(transition s '())))

  ;; Boolean SeqNum -> ConnState -> Transition
  (define ((discard-acknowledged-outbound ack? ackn) s)
    (transition
     (if (not ack?)
	 s
	 (let* ((b (conn-state-outbound s))
		(limit (seq+ (buffer-seqn b) (bit-string-byte-count (buffer-data b))))
		(ackn (if (seq> ackn limit) limit ackn))
		(dist (seq- ackn (buffer-seqn b))))
	   (define remaining-data (bit-string-drop (buffer-data b) (* dist 8))) ;; bit offset!
	   (struct-copy conn-state s
	     [outbound (struct-copy buffer b [data remaining-data] [seqn ackn])]
	     [syn-acked? (or (conn-state-syn-acked? s)
			     (positive? dist))])))
     '()))

  ;; Nat -> ConnState -> Transition
  (define ((update-outbound-window peer-window) s)
    (transition (struct-copy conn-state s
		  [outbound (struct-copy buffer (conn-state-outbound s)
			      [window peer-window])])
		'()))

  ;; ConnState -> Boolean
  (define (all-output-acknowledged? s)
    (bit-string-empty? (buffer-data (conn-state-outbound s))))

  ;; (Option SeqNum) -> ConnState -> Transition
  (define ((send-outbound old-ackn) s)
    (define b (conn-state-outbound s))
    (define pending-byte-count (max 0 (- (bit-string-byte-count (buffer-data b))
					 (if (buffer-finished? b) 1 0))))

    (define segment-size (min maximum-segment-size
			      (if (conn-state-syn-acked? s) (buffer-window b) 1)
			      ;; ^ can only send SYN until SYN is acked
			      pending-byte-count))
    (define segment-offset (if (conn-state-syn-acked? s) 0 1))
    (define chunk0 (bit-string-take (buffer-data b) (* segment-size 8))) ;; bit offset!
    (define chunk (bit-string-drop chunk0 (* segment-offset 8))) ;; bit offset!
    (define ackn (next-expected-seqn s))
    (define flags (set))
    (when ackn
      (set! flags (set-add flags 'ack)))
    (when (not (conn-state-syn-acked? s))
      (set! flags (set-add flags 'syn)))
    (when (and (buffer-finished? b)
	       (conn-state-syn-acked? s)
	       (= segment-size pending-byte-count)
	       (not (all-output-acknowledged? s))) ;; TODO: reexamine. This looks fishy
      (set! flags (set-add flags 'fin)))
    (define window (min 65535 ;; limit of field width
			(max 0 ;; can't be negative
			     (- (buffer-window (conn-state-inbound s))
				(bit-string-byte-count
				 (buffer-data (conn-state-inbound s)))))))
    (transition s
		(unless (and (equal? ackn old-ackn)
			     (conn-state-syn-acked? s)
			     (not (set-member? flags 'fin))
			     (zero? (bit-string-byte-count chunk)))
		  (local-require racket/pretty)
		  (pretty-write `(send-outbound (old-ackn ,old-ackn)
						(s ,s)
						(flags ,flags)))
		  (flush-output)
		  (send (tcp-packet #f dst-ip dst-port src-ip src-port
				    (buffer-seqn b)
				    (or ackn 0)
				    flags
				    window
				    #""
				    chunk)))))

  ;; ConnState -> Transition
  (define (bump-activity-time s)
    (transition (struct-copy conn-state s
		  [latest-activity-time (current-inexact-milliseconds)])
		'()))

  ;; ConnState -> Transition
  (define (quit-when-done s)
    (transition s (when (and (buffer-finished? (conn-state-outbound s))
			     (buffer-finished? (conn-state-inbound s))
			     (all-output-acknowledged? s)
			     (> (- (current-inexact-milliseconds)
				   (conn-state-latest-activity-time s))
				(* 2 1000 maximum-segment-lifetime-sec)))
		    (quit))))

  ;; Action
  (define send-set-transmit-check-timer
    (send (set-timer (timer-name 'transmit-check)
		     transmit-check-interval-msec
		     'relative)))

  ;; ConnState -> Transition
  (define (reset seqn ackn is-fin? s)
    (log-warning "Sending RST from ~a:~a to ~a:~a"
		 (ip-address->hostname dst-ip)
		 dst-port
		 (ip-address->hostname src-ip)
		 src-port)
    (transition s
		(list
		 (send (tcp-packet #f dst-ip dst-port src-ip src-port
				   seqn
				   (seq+ ackn (if is-fin? 1 0))
				   (set 'ack 'rst)
				   0
				   #""
				   #""))
		 (quit))))

  ;; ConnState -> ConnState
  (define (close-outbound-stream s)
    (transition
     (struct-copy conn-state s
       [outbound (struct-copy buffer (buffer-push (conn-state-outbound s) #"!") ;; dummy FIN byte
		   [finished? #t])])
     '()))

  (define (state-vector-behavior e s)
    (define old-ackn (buffer-seqn (conn-state-inbound s)))
    (match e
      [(routing-update g)
       (log-info "State vector routing-update:\n~a" (gestalt->pretty-string g))
       (define local-peer-present? (not (gestalt-empty? g)))
       (cond
	[(and local-peer-present? (not (conn-state-local-peer-seen? s)))
	 (transition (struct-copy conn-state s [local-peer-seen? #t]) '())]
	[(and (not local-peer-present?) (conn-state-local-peer-seen? s))
	 (log-info "Closing outbound stream.")
	 (sequence-transitions (close-outbound-stream s)
			       (send-outbound old-ackn)
			       bump-activity-time
			       quit-when-done)]
	[else #f])]
      [(message (tcp-packet #t _ _ _ _ seqn ackn flags window options data) _ _)
       (define expected (next-expected-seqn s))
       (if (and (not expected) ;; no syn yet
		(not (set-member? flags 'syn))) ;; and this isn't it
	   (reset ackn ;; this is *our* seqn
		  seqn ;; this is what we should acknowledge...
		  (set-member? flags 'fin) ;; ... +1, if fin is set
		  s)
	   (sequence-transitions (cond
				  [(not expected) ;; haven't seen syn yet, but we know this is it
				   (incorporate-segment data (set-inbound-seqn (seq+ seqn 1) s))]
				  [(= expected seqn)
				   (incorporate-segment data s)]
				  [else
				   (transition s '())])
				 deliver-inbound-locally
				 (check-fin flags)
				 (discard-acknowledged-outbound (set-member? flags 'ack) ackn)
				 (update-outbound-window window)
				 (send-outbound old-ackn)
				 bump-activity-time
				 quit-when-done))]
      [(message (tcp-channel _ _ bs) _ _)
       (sequence-transitions (transition (struct-copy conn-state s
					   [outbound (buffer-push (conn-state-outbound s) bs)])
					 '())
			     (send-outbound old-ackn)
			     bump-activity-time
			     quit-when-done)]
      [(message (timer-expired (== (timer-name 'transmit-check)) _) _ _)
       (sequence-transitions (transition s send-set-transmit-check-timer)
			     (send-outbound old-ackn)
			     quit-when-done)]
      [_ #f]))

  ;; (local-require racket/trace)
  ;; (trace state-vector-behavior)

  (define initial-outbound-seqn
    ;; Yuck
    (inexact->exact (truncate (* #x100000000 (random)))))

  ;; TODO accept input from user process
  ;; TODO append a dummy byte at FIN position in outbound buffer
  (list
   send-set-transmit-check-timer
   (let ((state0 (conn-state (buffer #"!" initial-outbound-seqn 0 #f) ;; dummy data at SYN position
			     (buffer #"" #f inbound-buffer-limit #f)
			     #f
			     (current-inexact-milliseconds)
			     #f)))
     (spawn state-vector-behavior
	    state0
	    (compute-gestalt state0)))))
