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

(struct tcp-address (host port) #:prefab)
(struct tcp-handle (id) #:prefab)
(struct tcp-listener (port) #:prefab)

(struct tcp-channel (source destination subpacket) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define PROTOCOL-TCP 6)

(struct codec-state (active-state-vectors) #:transparent)

(define (spawn-tcp-driver)

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
	      (spawn-needed? (not (state-vector-active? statevec s))))
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

(struct buffer (data ;; bit-string
		seqn ;; names leftmost byte in data
		window ;; counts bytes from leftmost byte in data
		finished?) ;; boolean: true after FIN
	#:transparent)

(struct conn-state (outbound ;; buffer
		    inbound ;; buffer
		    syn-acked? ;; boolean
		    latest-activity-time) ;; from current-inexact-milliseconds
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
    (transition
     (if (set-member? flags 'fin)
	 (struct-copy conn-state s
	   [inbound (struct-copy buffer b
		      [seqn (seq+ (buffer-seqn b) 1)] ;; reliable: count fin as a byte
		      [finished? #t])])
	 s)
     '()))

  ;; Boolean Nat -> ConnState -> Transition
  (define ((discard-acknowledged-outbound ack? ackn) s)
    (transition
     (if (not ack?)
	 s
	 (let* ((b (conn-state-outbound s))
		(limit (seq+ (buffer-seqn b) (bit-string-byte-count (buffer-data b))))
		(ackn (if (seq> ackn limit) limit ackn))
		(dist (seq- ackn (buffer-seqn b))))
	   (define-values (discarded-acknowledged-data remaining-data)
	     (bit-string-split-at (buffer-data b) (* dist 8))) ;; bit offset!
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

  ;; ConnState -> Transition
  (define ((send-outbound old-ackn) s)
    (define b (conn-state-outbound s))
    (define pending-byte-count (max 0 (- (bit-string-byte-count (buffer-data b))
					 (if (buffer-finished? b) 1 0))))
    (define segment-size (min maximum-segment-size
			      (buffer-window b)
			      pending-byte-count))
    (define segment-offset (if (conn-state-syn-acked? s) 0 1))
    (define-values (chunk0 remaining-data)
      (bit-string-split-at (buffer-data b) (* segment-size 8))) ;; bit offset!
    (define-values (discarded-dummy-syn-data chunk)
      (bit-string-split-at chunk0 (* segment-offset 8))) ;; bit offset!
    (define ackn (next-expected-seqn s))
    (define flags (set))
    (when ackn
      (set! flags (set-add flags 'ack)))
    (when (not (conn-state-syn-acked? s))
      (set! flags (set-add flags 'syn)))
    (when (and (buffer-finished? b)
	       (= segment-size pending-byte-count)
	       (not (all-output-acknowledged? s)))
      (set! flags (set-add flags 'fin)))
    (define window (min 65535 ;; limit of field width
			(max 0 ;; can't be negative
			     (- (buffer-window (conn-state-inbound s))
				(bit-string-byte-count
				 (buffer-data (conn-state-inbound s)))))))
    (transition s
		(unless (and (equal? ackn old-ackn)
			     (conn-state-syn-acked? s)
			     (zero? (bit-string-byte-count chunk)))
		  (send (tcp-packet #f
				    dst-ip
				    dst-port
				    src-ip
				    src-port
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
    (if (and (buffer-finished? (conn-state-outbound s))
	     (buffer-finished? (conn-state-inbound s))
	     (all-output-acknowledged? s)
	     (> (- (current-inexact-milliseconds) (conn-state-latest-activity-time s))
		(* 2 1000 maximum-segment-lifetime-sec)))
	(transition s (quit))
	(transition s '())))

  ;; Action
  (define send-set-transmit-check-timer
    (send (set-timer (timer-name 'transmit-check)
		     transmit-check-interval-msec
		     'relative)))

  (define (state-vector-behavior e s)
    (define old-ackn (buffer-seqn (conn-state-inbound s)))
    (match e
      [(message (tcp-packet #t _ _ _ _ seqn ackn flags window options data) _ _)
       (define expected (next-expected-seqn s))
       (sequence-transitions (if (not expected) ;; haven't seen syn yet...
				 (if (set-member? flags 'syn) ;; ... and this is it
				     (incorporate-segment data
							  (set-inbound-seqn (seq+ seqn 1) s))
				     (transition s '()))
				 (if (= expected seqn)
				     (incorporate-segment data s)
				     (transition s '())))
			     deliver-inbound-locally
			     (check-fin flags)
			     (discard-acknowledged-outbound (set-member? flags 'ack) ackn)
			     (update-outbound-window window)
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
   (spawn state-vector-behavior
	  (conn-state (buffer #"!" initial-outbound-seqn 0 #f) ;; dummy data at SYN position
		      (buffer #"" #f inbound-buffer-limit #f)
		      #f
		      (current-inexact-milliseconds))
	  (gestalt-union (sub (timer-expired (timer-name ?) ?))
			 (sub (tcp-packet #t src-ip src-port dst-ip dst-port ? ? ? ? ? ?))
			 (pub (tcp-packet #f dst-ip dst-port src-ip src-port ? ? ? ? ? ?))
			 (sub (tcp-channel dst src ?))
			 (pub (tcp-channel src dst ?))))))