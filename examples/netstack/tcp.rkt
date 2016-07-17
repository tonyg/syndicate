#lang syndicate/actor

(provide (struct-out tcp-address)
	 (struct-out tcp-handle)
	 (struct-out tcp-listener)
	 (struct-out tcp-channel)
	 spawn-tcp-driver)

(require racket/set)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "checksum.rkt")

(require/activate syndicate/drivers/timer)
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
  (spawn-port-allocator 'tcp (lambda () (query-set tcp-ports (tcp-port-allocation $p _) p)))
  (spawn-kernel-tcp-driver)
  (actor #:name 'tcp-inbound-driver
   (react
    (during/actor (advertise (observe (tcp-channel _ ($ server-addr (tcp-listener _)) _)))
                  #:name (list 'tcp-listen server-addr)
                  (match-define (tcp-listener port) server-addr)
                  (assert (tcp-port-allocation port server-addr))
                  (on (asserted (advertise (tcp-channel ($ remote-addr (tcp-address _ _))
                                                        ($ local-addr (tcp-address _ port))
                                                        _)))
                      (spawn-relay server-addr remote-addr local-addr)))))
  (actor #:name 'tcp-outbound-driver
   (react
    (define local-ips (query-local-ip-addresses))
    (on (asserted (advertise (tcp-channel ($ local-addr (tcp-handle _))
                                          ($ remote-addr (tcp-address _ _))
                                          _)))
        (define port (allocate-port! 'tcp))
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
        (define appropriate-ip (set-first (local-ips)))
        (define appropriate-host (ip-address->hostname appropriate-ip))
        (match-define (tcp-address remote-host remote-port) remote-addr)
        (define remote-ip (ip-string->ip-address remote-host))
        (spawn-relay local-addr remote-addr (tcp-address appropriate-host port))
        (spawn-state-vector remote-ip remote-port appropriate-ip port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relay between kernel-level and user-level

(define relay-peer-wait-time-msec 5000)

(define (spawn-relay local-user-addr remote-addr local-tcp-addr)
  (define timer-name (list 'spawn-relay local-tcp-addr remote-addr))

  (actor #:name (list 'tcp-relay local-user-addr remote-addr local-tcp-addr)
   (react
    (assert (tcp-port-allocation (tcp-address-port local-tcp-addr) local-user-addr))
    (assert (advertise (tcp-channel remote-addr local-user-addr _)))
    (assert (advertise (tcp-channel local-tcp-addr remote-addr _)))

    (field [local-peer-present? #f]
           [remote-peer-present? #f])

    (on-start (send! (set-timer timer-name relay-peer-wait-time-msec 'relative)))
    (on (message (timer-expired timer-name _))
        (when (not (and (local-peer-present?) (remote-peer-present?)))
          (error 'spawn-relay "TCP relay process timed out waiting for peer")))

    (on (asserted (observe (tcp-channel remote-addr local-user-addr _)))
        (local-peer-present? #t))
    (stop-when (retracted (observe (tcp-channel remote-addr local-user-addr _))))

    (on (asserted (advertise (tcp-channel remote-addr local-tcp-addr _)))
        (remote-peer-present? #t))
    (stop-when (retracted (advertise (tcp-channel remote-addr local-tcp-addr _))))

    (on (message (tcp-channel local-user-addr remote-addr $bs))
        (send! (tcp-channel local-tcp-addr remote-addr bs)))

    (on (message (tcp-channel remote-addr local-tcp-addr $bs))
        (send! (tcp-channel remote-addr local-user-addr bs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codec & kernel-level driver

(define PROTOCOL-TCP 6)

(define (spawn-kernel-tcp-driver)
  (actor #:name 'kernel-tcp-driver
   (forever
    (define local-ips (query-local-ip-addresses))

    (define active-state-vectors
      (query-set active-state-vectors
                 (observe (observe (tcp-packet _ $si $sp $di $dp _ _ _ _ _ _)))
                 (list si sp di dp)))

    (define (state-vector-active? statevec)
      (set-member? (active-state-vectors) statevec))

    (define (analyze-incoming-packet src-ip dst-ip body)
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
                (old-active-state-vectors (active-state-vectors))
                (spawn-needed? (and (not (state-vector-active? statevec))
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
                (when spawn-needed?
                  (active-state-vectors (set-add (active-state-vectors) statevec))
                  (spawn-state-vector src-ip src-port dst-ip dst-port))
                ;; TODO: get packet to the new state-vector process somehow
                (send! packet)))
             (else #f))))
        (else #f)))

    ;; TODO: again, want to print this when local-ips or
    ;; active-state-vectors change.
    ;; (log-info "gestalt yielded statevecs ~v and local-ips ~v" statevecs local-ips)

    (define (deliver-outbound-packet p)
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
      (send! (ip-packet #f src-ip dst-ip PROTOCOL-TCP #""
                        (ip-checksum 16 payload #:pseudo-header pseudo-header))))

    (on (message (ip-packet $source-if $src $dst PROTOCOL-TCP _ $body))
        (when (and source-if ;; source-if == #f iff packet originates locally
                   (set-member? (local-ips) dst))
          (analyze-incoming-packet src dst body)))

    (on (message ($ p (tcp-packet #f _ _ _ _ _ _ _ _ _ _)))
        (deliver-outbound-packet p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per-connection state vector process

(struct buffer (data ;; bit-string
		seqn ;; names leftmost byte in data
		window ;; counts bytes from leftmost byte in data
		finished?) ;; boolean: true after FIN
	#:transparent)

(define (buffer-push b data)
  (struct-copy buffer b [data (bit-string-append (buffer-data b) data)]))

(define transmit-check-interval-msec 2000)
(define inbound-buffer-limit 65535)
(define maximum-segment-size 536) ;; bytes
(define maximum-segment-lifetime-sec (* 2 60)) ;; two minutes; 2MSL is TIME-WAIT timeout
(define user-timeout-msec (* 5 60 1000)) ;; per RFC 793, this should be per-connection, but I
        ;; cheat; RFC 793 says "the present global default is five minutes", which is
        ;; reasonable to be getting on with

(define (seq+ a b) (bitwise-and #xffffffff (+ a b)))

;; Always positive
(define (seq- larger smaller)
  (if (< larger smaller) ;; wraparound has occurred
      (+ (- larger smaller) #x100000000)
      (- larger smaller)))

(define (seq> a b)
  (< (seq- a b) #x80000000))

(define (spawn-state-vector src-ip src-port dst-ip dst-port)
  (define src (tcp-address (ip-address->hostname src-ip) src-port))
  (define dst (tcp-address (ip-address->hostname dst-ip) dst-port))
  (define (timer-name kind) (list 'tcp-timer kind src dst))

  (actor
   #:name (list 'tcp-state-vector
                (ip-address->hostname src-ip)
                src-port
                (ip-address->hostname dst-ip)
                dst-port)
   (react

    (define initial-outbound-seqn
      ;; Yuck
      (inexact->exact (truncate (* #x100000000 (random)))))

    (field [outbound (buffer #"!" initial-outbound-seqn 0 #f)] ;; dummy data at SYN position
           [inbound (buffer #"" #f inbound-buffer-limit #f)]
           [syn-acked? #f]
           [latest-peer-activity-time (current-inexact-milliseconds)]
           ;; ^ the most recent time we heard from our peer
           [user-timeout-base-time (current-inexact-milliseconds)]
           ;; ^ when the index of the first outbound unacknowledged byte changed
           [most-recent-time (current-inexact-milliseconds)]
           ;; ^ updated by timer expiry; a field, to trigger quit checks
           [quit-because-reset? #f])

    (let ()
      (local-require (submod syndicate/actor priorities))
      (on-event #:priority *query-priority*
                [_ (most-recent-time (current-inexact-milliseconds))]))

    (define (next-expected-seqn)
      (define b (inbound))
      (define v (buffer-seqn b))
      (and v (seq+ v (bit-string-byte-count (buffer-data b)))))

    (define (set-inbound-seqn! seqn)
      (inbound (struct-copy buffer (inbound) [seqn seqn])))

    (define (incorporate-segment! data)
      ;; (log-info "GOT INBOUND STUFF TO DELIVER ~v" data)
      (when (not (buffer-finished? (inbound)))
        (inbound (buffer-push (inbound) data))))

    (define (deliver-inbound-locally!)
      (define b (inbound))
      (when (not (bit-string-empty? (buffer-data b)))
	(define chunk (bit-string->bytes (buffer-data b)))
        (send! (tcp-channel src dst chunk))
        (inbound (struct-copy buffer b
                              [data #""]
                              [seqn (seq+ (buffer-seqn b) (bytes-length chunk))]))))

    ;; (Setof Symbol) -> Void
    (define (check-fin! flags)
      (define b (inbound))
      (unless (bit-string-empty? (buffer-data b)) ;; assured by deliver-inbound-locally
        (error 'check-fin "Nonempty inbound buffer"))
      (when (set-member? flags 'fin)
        (log-info "Closing inbound stream.")
        (inbound (struct-copy buffer b
                              [seqn (seq+ (buffer-seqn b) 1)] ;; reliable: count fin as a byte
                              [finished? #t]))))

    ;; Boolean SeqNum -> Void
    (define (discard-acknowledged-outbound! ack? ackn)
      (when ack?
	(let* ((b (outbound))
               (base (buffer-seqn b))
	       (limit (seq+ (buffer-seqn b) (bit-string-byte-count (buffer-data b))))
	       (ackn (if (seq> ackn limit) limit ackn))
               (ackn (if (seq> base ackn) base ackn))
	       (dist (seq- ackn base)))
	  (define remaining-data (bit-string-drop (buffer-data b) (* dist 8))) ;; bit offset!
          (user-timeout-base-time (current-inexact-milliseconds))
          (outbound (struct-copy buffer b [data remaining-data] [seqn ackn]))
          (syn-acked? (or (syn-acked?) (positive? dist))))))

    ;; Nat -> Void
    (define (update-outbound-window! peer-window)
      (outbound (struct-copy buffer (outbound) [window peer-window])))

    (define (all-output-acknowledged?)
      (bit-string-empty? (buffer-data (outbound))))

    ;; (Option SeqNum) -> Void
    (define (send-outbound! old-ackn)
      (define b (outbound))
      (define pending-byte-count (max 0 (- (bit-string-byte-count (buffer-data b))
                                           (if (buffer-finished? b) 1 0))))

      (define segment-size (min maximum-segment-size
                                (if (syn-acked?) (buffer-window b) 1)
                                ;; ^ can only send SYN until SYN is acked
                                pending-byte-count))
      (define segment-offset (if (syn-acked?) 0 1))
      (define chunk0 (bit-string-take (buffer-data b) (* segment-size 8))) ;; bit offset!
      (define chunk (bit-string-drop chunk0 (* segment-offset 8))) ;; bit offset!
      (define ackn (next-expected-seqn))
      (define flags (set))
      (when ackn
        (set! flags (set-add flags 'ack)))
      (when (not (syn-acked?))
        (set! flags (set-add flags 'syn)))
      (when (and (buffer-finished? b)
                 (syn-acked?)
                 (= segment-size pending-byte-count)
                 (not (all-output-acknowledged?))) ;; TODO: reexamine. This looks fishy
        (set! flags (set-add flags 'fin)))
      (define window (min 65535 ;; limit of field width
                          (max 0 ;; can't be negative
                               (- (buffer-window (inbound))
                                  (bit-string-byte-count (buffer-data (inbound)))))))
      (unless (and (equal? ackn old-ackn)
                   (syn-acked?)
                   (not (set-member? flags 'fin))
                   (zero? (bit-string-byte-count chunk)))
        (local-require racket/pretty)
        (pretty-write `(send-outbound (old-ackn ,old-ackn)
                                      (flags ,flags)))
        (flush-output)
        (send! (tcp-packet #f dst-ip dst-port src-ip src-port
                           (buffer-seqn b)
                           (or ackn 0)
                           flags
                           window
                           #""
                           chunk))))

    (define (bump-peer-activity-time!)
      (latest-peer-activity-time (current-inexact-milliseconds)))

    ;; Number -> Boolean
    (define (heard-from-peer-within-msec? msec)
      (<= (- (most-recent-time) (latest-peer-activity-time)) msec))

    (define (user-timeout-expired?)
      (and (not (all-output-acknowledged?))
           (> (- (most-recent-time) (user-timeout-base-time))
              user-timeout-msec)))

    (define (send-set-transmit-check-timer!)
      (send! (set-timer (timer-name 'transmit-check)
                        transmit-check-interval-msec
                        'relative)))

    (define (reset! seqn ackn)
      (log-warning "Sending RST from ~a:~a to ~a:~a"
                   (ip-address->hostname dst-ip)
                   dst-port
                   (ip-address->hostname src-ip)
                   src-port)
      (quit-because-reset? #t)
      (send! (tcp-packet #f dst-ip dst-port src-ip src-port
                         seqn
                         ackn
                         (set 'ack 'rst)
                         0
                         #""
                         #"")))

    (define (close-outbound-stream!)
      (define b (outbound))
      (when (not (buffer-finished? b))
        (outbound (struct-copy buffer (buffer-push b #"!") ;; dummy FIN byte
                               [finished? #t]))))

    (assert #:when (and (syn-acked?) (not (buffer-finished? (inbound))))
            (advertise (tcp-channel src dst _)))

    (stop-when
     (rising-edge
      (and (buffer-finished? (outbound))
           (buffer-finished? (inbound))
           (all-output-acknowledged?)
           (not (heard-from-peer-within-msec? (* 2 1000 maximum-segment-lifetime-sec)))))
     ;; Everything is cleanly shut down, and we just need to wait a while for unexpected
     ;; packets before we release the state vector.
     )

    (stop-when
     (rising-edge (user-timeout-expired?))
     ;; We've been plaintively retransmitting for user-timeout-msec without hearing anything
     ;; back; this is a crude approximation of the real condition for TCP_USER_TIMEOUT, but
     ;; it will do for now? TODO
     (log-info "TCP_USER_TIMEOUT fired."))

    (stop-when (rising-edge (quit-because-reset?)))

    (define/query-value local-peer-seen? #f (observe (tcp-channel src dst _)) #t
      #:on-remove (begin
                    (log-info "Closing outbound stream.")
                    (close-outbound-stream!)
                    (send-outbound! (buffer-seqn (inbound)))))

    (define/query-value listener-listening?
      #f
      (observe (advertise (tcp-channel _ (tcp-listener dst-port) _)))
      #t)

    (on (message (tcp-packet #t src-ip src-port dst-ip dst-port
                             $seqn $ackn $flags $window $options $data))
        (define old-ackn (buffer-seqn (inbound)))
        (define expected (next-expected-seqn))
        (define is-syn? (set-member? flags 'syn))
        (define is-fin? (set-member? flags 'fin))
        (cond
          [(set-member? flags 'rst) (quit-because-reset? #t)]
          [(and (not expected) ;; no syn yet
                (or (not is-syn?) ;; and this isn't it
                    (and (not (listener-listening?)) ;; or it is, but no listener...
                         (not (local-peer-seen?))))) ;; ...and no outbound client
           (reset! ackn ;; this is *our* seqn
                   (seq+ seqn (+ (if is-syn? 1 0) (if is-fin? 1 0)))
                   ;; ^^ this is what we should acknowledge...
                   )]
          [else
           (cond
             [(not expected) ;; haven't seen syn yet, but we know this is it
              (set-inbound-seqn! (seq+ seqn 1))
              (incorporate-segment! data)]
             [(= expected seqn)
              (incorporate-segment! data)]
             [else (void)])
           (deliver-inbound-locally!)
           (check-fin! flags)
           (discard-acknowledged-outbound! (set-member? flags 'ack) ackn)
           (update-outbound-window! window)
           (send-outbound! old-ackn)
           (bump-peer-activity-time!)]))

    (on (message (tcp-channel dst src $bs))
        (define old-ackn (buffer-seqn (inbound)))
        ;; (log-info "GOT MORE STUFF TO DELIVER ~v" bs)

        (when (all-output-acknowledged?)
          ;; Only move user-timeout-base-time if there wasn't
          ;; already some outstanding output.
          (user-timeout-base-time (current-inexact-milliseconds)))

        (outbound (buffer-push (outbound) bs))
        (send-outbound! old-ackn))

    (on-start (send-set-transmit-check-timer!))
    (on (message (timer-expired (timer-name 'transmit-check) _))
        (define old-ackn (buffer-seqn (inbound)))
        ;; TODO: I am abusing this timer for multiple tasks. Notably, this is a (crude) means of
        ;; retransmitting outbound data as well as a means of checking for an expired
        ;; TCP_USER_TIMEOUT. A better design would have separate timers and a more fine-grained
        ;; approach.
        (send-set-transmit-check-timer!)
        (send-outbound! old-ackn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-tcp-driver)
