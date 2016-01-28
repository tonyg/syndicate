#lang racket/base

(provide (struct-out tcp-address)
	 (struct-out tcp-handle)
	 (struct-out tcp-listener)
	 (struct-out tcp-channel)
	 spawn-tcp-driver)

(require racket/set)
(require racket/match)
(require prospect-monolithic)
(require prospect-monolithic/drivers/timer)
(require prospect-monolithic/demand-matcher)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "checksum.rkt")
(require "ip.rkt")
(require "port-allocator.rkt")

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

;; (tcp-port-allocation Number (U TcpHandle TcpListener))
(struct tcp-port-allocation (port handle) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-accessible driver startup

(define (spawn-tcp-driver)
  (list (spawn-demand-matcher #:name 'tcp-inbound-driver
                              (advertise (observe (tcp-channel ? (?! (tcp-listener ?)) ?)))
                              (advertise (advertise (tcp-channel ? (?! (tcp-listener ?)) ?)))
			      (lambda (server-addr)
				(match-define (tcp-listener port) server-addr)
				;; TODO: have listener shut down once user-level listener does
                                (list
                                 (spawn #:name (string->symbol
                                                (format "tcp-listener-port-reservation:~a" port))
                                        (lambda (e s) #f)
                                        (void)
                                        (scn (assertion (tcp-port-allocation port server-addr))))
                                 (spawn-demand-matcher
                                  #:name (string->symbol (format "tcp-listener:~a" port))
                                  (advertise (tcp-channel (?! (tcp-address ? ?))
                                                          (?! (tcp-address ? port))
                                                          ?))
                                  (observe (tcp-channel (?! (tcp-address ? ?))
                                                        (?! (tcp-address ? port))
                                                        ?))
                                  (spawn-relay server-addr)))))
	(spawn-demand-matcher #:name 'tcp-outbound-driver
                              (advertise (tcp-channel (?! (tcp-handle ?)) (?! (tcp-address ? ?)) ?))
                              (observe (tcp-channel (?! (tcp-handle ?)) (?! (tcp-address ? ?)) ?))
                              allocate-port-and-spawn-socket)
	(spawn-tcp-port-allocator)
	(spawn-kernel-tcp-driver)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Port allocation

(define (spawn-tcp-port-allocator)
  (spawn-port-allocator 'tcp
                        (subscription (tcp-port-allocation ? ?))
			(lambda (g local-ips)
                          (project-assertions g (tcp-port-allocation (?!) ?)))))

(define (allocate-port-and-spawn-socket local-addr remote-addr)
  (message (port-allocation-request
            'tcp
            (lambda (port local-ips)
              ;; TODO: Choose a sensible IP address for the outbound
              ;; connection. We don't have enough information to do this
              ;; well at the moment, so just pick some available local IP
              ;; address.
              ;;
              ;; Interesting note: In some sense, the right answer is
              ;; "?". This would give us a form of mobility, where IP
              ;; addresses only route to a given bucket-of-state and ONLY
              ;; the port number selects a substate therein. That's not
              ;; how TCP is defined however so we can't do that.
              (define appropriate-ip (set-first local-ips))
              (define appropriate-host (ip-address->hostname appropriate-ip))
              (match-define (tcp-address remote-host remote-port) remote-addr)
              (define remote-ip (ip-string->ip-address remote-host))
              (list
               ((spawn-relay local-addr) remote-addr (tcp-address appropriate-host port))
               (spawn-state-vector remote-ip remote-port appropriate-ip port))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relay between kernel-level and user-level

(define relay-peer-wait-time-msec 5000)

(define ((spawn-relay local-user-addr) remote-addr local-tcp-addr)
  (define timer-name (list 'spawn-relay local-tcp-addr remote-addr))
  (define local-peer-traffic (?! (observe (tcp-channel remote-addr local-user-addr ?))))
  (define remote-peer-traffic (?! (advertise (tcp-channel remote-addr local-tcp-addr ?))))
  (list
   (message (set-timer timer-name relay-peer-wait-time-msec 'relative))
   (spawn #:name (string->symbol (format "tcp-relay:~v:~v:~v"
                                         local-user-addr
                                         remote-addr
                                         local-tcp-addr))
          (lambda (e state)
	    (match e
	      [(scn g)
	       (define local-peer-absent?
                 (trie-empty? (trie-project g (compile-projection local-peer-traffic))))
	       (define remote-peer-absent?
                 (trie-empty? (trie-project g (compile-projection remote-peer-traffic))))
	       (define new-state (+ (if local-peer-absent? 0 1) (if remote-peer-absent? 0 1)))
               (if (< new-state state)
                   (quit)
                   (transition new-state '()))]
	      [(message (tcp-channel (== local-user-addr) (== remote-addr) bs))
	       (transition state (message (tcp-channel local-tcp-addr remote-addr bs)))]
	      [(message (tcp-channel (== remote-addr) (== local-tcp-addr) bs))
	       (transition state (message (tcp-channel remote-addr local-user-addr bs)))]
	      [(message (timer-expired _ _))
	       #:when (< state 2) ;; we only care if we're not fully connected
	       (error 'spawn-relay "TCP relay process timed out waiting for peer")]
	      [_ #f]))
	  0
	  (scn/union (subscription (projection->pattern local-peer-traffic))
                     (subscription (projection->pattern remote-peer-traffic))
                     (assertion (tcp-port-allocation (tcp-address-port local-tcp-addr)
                                                     local-user-addr))
                     (subscription (tcp-channel remote-addr local-tcp-addr ?))
                     (subscription (tcp-channel local-user-addr remote-addr ?))
                     (advertisement (tcp-channel remote-addr local-user-addr ?))
                     (advertisement (tcp-channel local-tcp-addr remote-addr ?))
                     (subscription (timer-expired timer-name ?))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codec & kernel-level driver

(define PROTOCOL-TCP 6)

(struct codec-state (local-ips active-state-vectors) #:transparent)

(define (spawn-kernel-tcp-driver)

  (define (state-vector-active? statevec s)
    (set-member? (codec-state-active-state-vectors s) statevec))

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
			   (message packet)))))
	   (else #f))))
      (else #f)))

  (define statevec-projection
    (compile-projection (observe (tcp-packet ? (?!) (?!) (?!) (?!) ? ? ? ? ? ?))))

  (define (analyze-gestalt g s)
    (define local-ips (gestalt->local-ip-addresses g))
    (define statevecs (trie-project/set g statevec-projection))
    (log-info "gestalt yielded statevecs ~v and local-ips ~v" statevecs local-ips)
    (transition (struct-copy codec-state s
		  [local-ips local-ips]
		  [active-state-vectors statevecs]) '()))

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
    (transition s (message (ip-packet #f src-ip dst-ip PROTOCOL-TCP #""
                                      (ip-checksum 16 payload #:pseudo-header pseudo-header)))))

  (spawn #:name 'kernel-tcp-driver
         (lambda (e s)
	   (match e
	     [(scn g)
	      (analyze-gestalt g s)]
	     [(message (ip-packet source-if src dst _ _ body))
	      #:when (and source-if ;; source-if == #f iff packet originates locally
			  (set-member? (codec-state-local-ips s) dst))
	      (analyze-incoming-packet src dst body s)]
	     [(message (? tcp-packet? p))
	      #:when (not (tcp-packet-from-wire? p))
	      (deliver-outbound-packet p s)]
	     [_ #f]))
	 (codec-state (set) (set))
	 (scn/union (subscription (ip-packet ? ? ? PROTOCOL-TCP ? ?))
                    (subscription (tcp-packet #f ? ? ? ? ? ? ? ? ? ?))
                    (subscription (observe (tcp-packet #t ? ? ? ? ? ? ? ? ? ?)))
                    observe-local-ip-addresses-gestalt)))

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
		    local-peer-seen? ;; boolean
		    listener-listening?) ;; boolean
	#:transparent)

(define transmit-check-interval-msec 2000)
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
    ;; (log-info "GOT INBOUND STUFF TO DELIVER ~v" data)
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

  (define local-peer-detector (?! (observe (tcp-channel src dst ?))))
  (define listener-detector (?! (observe (advertise (tcp-channel ? (tcp-listener dst-port) ?)))))

  ;; ConnState -> Gestalt
  (define (compute-gestalt s)
    (define worldward-facing-gestalt
      (subscription (tcp-packet #t src-ip src-port dst-ip dst-port ? ? ? ? ? ?)))
    (define appward-facing-gestalt
      (assertion-set-union
       (subscription (projection->pattern local-peer-detector))
       (subscription (projection->pattern listener-detector))
       (subscription (tcp-channel dst src ?))
       (if (and (conn-state-syn-acked? s)
		(not (buffer-finished? (conn-state-inbound s))))
	   (advertisement (tcp-channel src dst ?))
	   (trie-empty))))
    (assertion-set-union (subscription (timer-expired (timer-name ?) ?))
                         worldward-facing-gestalt
                         appward-facing-gestalt))

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
		      (message (tcp-channel src dst chunk))))))

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
	  (log-info "Closing inbound stream.")
	  (transition new-s (scn (compute-gestalt new-s))))
	(transition s '())))

  ;; Boolean SeqNum -> ConnState -> Transition
  (define ((discard-acknowledged-outbound ack? ackn) s)
    (if (not ack?)
	(transition s '())
	(let* ((b (conn-state-outbound s))
	       (limit (seq+ (buffer-seqn b) (bit-string-byte-count (buffer-data b))))
	       (ackn (if (seq> ackn limit) limit ackn))
	       (dist (seq- ackn (buffer-seqn b))))
	  (define remaining-data (bit-string-drop (buffer-data b) (* dist 8))) ;; bit offset!
	  (define new-s (struct-copy conn-state s
			  [outbound (struct-copy buffer b [data remaining-data] [seqn ackn])]
			  [syn-acked? (or (conn-state-syn-acked? s)
					  (positive? dist))]))
	  (transition new-s
		      (when (and (not (conn-state-syn-acked? s)) (positive? dist))
			(scn (compute-gestalt new-s)))))))

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
		  (message (tcp-packet #f dst-ip dst-port src-ip src-port
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
             (> (- (current-inexact-milliseconds)
                   (conn-state-latest-activity-time s))
                (* 2 1000 maximum-segment-lifetime-sec)))
        (quit)
        #f))

  ;; Action
  (define send-set-transmit-check-timer
    (message (set-timer (timer-name 'transmit-check)
                        transmit-check-interval-msec
                        'relative)))

  ;; SeqNum SeqNum ConnState -> Transition
  (define (reset seqn ackn s)
    (log-warning "Sending RST from ~a:~a to ~a:~a"
		 (ip-address->hostname dst-ip)
		 dst-port
		 (ip-address->hostname src-ip)
		 src-port)
    (quit (message (tcp-packet #f dst-ip dst-port src-ip src-port
                               seqn
                               ackn
                               (set 'ack 'rst)
                               0
                               #""
                               #""))))

  ;; ConnState -> Transition
  (define (close-outbound-stream s)
    (transition
     (struct-copy conn-state s
       [outbound (struct-copy buffer (buffer-push (conn-state-outbound s) #"!") ;; dummy FIN byte
		   [finished? #t])])
     '()))

  (define (state-vector-behavior e s)
    (define old-ackn (buffer-seqn (conn-state-inbound s)))
    (match e
      [(scn g)
       (log-info "State vector routing-update:\n~a" (trie->pretty-string g))
       (define local-peer-present?
         (trie-non-empty? (trie-project g (compile-projection local-peer-detector))))
       (define listening?
         (trie-non-empty? (trie-project g (compile-projection listener-detector))))
       (define new-s (struct-copy conn-state s [listener-listening? listening?]))
       (cond
	[(and local-peer-present? (not (conn-state-local-peer-seen? s)))
	 (transition (struct-copy conn-state new-s [local-peer-seen? #t]) '())]
	[(and (not local-peer-present?) (conn-state-local-peer-seen? s))
	 (log-info "Closing outbound stream.")
	 (sequence-transitions (close-outbound-stream new-s)
			       (send-outbound old-ackn)
			       bump-activity-time
			       quit-when-done)]
	[else (transition new-s '())])]
      [(message (tcp-packet #t _ _ _ _ seqn ackn flags window options data))
       (define expected (next-expected-seqn s))
       (define is-syn? (set-member? flags 'syn))
       (define is-fin? (set-member? flags 'fin))
       (cond
	[(set-member? flags 'rst) (quit)]
	[(and (not expected)	 ;; no syn yet
	      (or (not is-syn?)	 ;; and this isn't it
		  (and (not (conn-state-listener-listening? s)) ;; or it is, but no listener...
		       (not (conn-state-local-peer-seen? s))))) ;; ...and no outbound client
	 (reset ackn ;; this is *our* seqn
		(seq+ seqn (+ (if is-syn? 1 0) (if is-fin? 1 0)))
		;; ^^ this is what we should acknowledge...
		s)]
	[else
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
			       quit-when-done)])]
      [(message (tcp-channel _ _ bs))
       ;; (log-info "GOT MORE STUFF TO DELIVER ~v" bs)
       (sequence-transitions (transition (struct-copy conn-state s
					   [outbound (buffer-push (conn-state-outbound s) bs)])
					 '())
			     (send-outbound old-ackn)
			     bump-activity-time
			     quit-when-done)]
      [(message (timer-expired (== (timer-name 'transmit-check)) _))
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
			     #f
			     #f)))
     (spawn #:name
            (string->symbol (format "tcp-state-vector:~a:~a:~a:~a"
                                    (ip-address->hostname src-ip)
                                    src-port
                                    (ip-address->hostname dst-ip)
                                    dst-port))
            state-vector-behavior
	    state0
	    (scn (compute-gestalt state0))))))
