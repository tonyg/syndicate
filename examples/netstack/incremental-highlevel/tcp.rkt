#lang syndicate

(provide (struct-out tcp-address)
	 (struct-out tcp-handle)
	 (struct-out tcp-listener)
	 (struct-out tcp-channel)
	 spawn-tcp-driver)

(require racket/set)
(require bitsyntax)
(require syndicate/protocol/advertise)

(require "dump-bytes.rkt")
(require "checksum.rkt")

(require/activate syndicate/drivers/timestate)
(require "ip.rkt")
(require "port-allocator.rkt")

(module+ test (require rackunit))

(define-logger netstack/tcp)

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

(define (summarize-tcp-packet packet)
  (format "(~a) ~a:~a -> ~a:~a (seq ~a, ack ~a, flags ~a, window ~a, payload ~a)"
          (if (tcp-packet-from-wire? packet) "I" "O")
          (ip-address->hostname (tcp-packet-source-ip packet))
          (tcp-packet-source-port packet)
          (ip-address->hostname (tcp-packet-destination-ip packet))
          (tcp-packet-destination-port packet)
          (tcp-packet-sequence-number packet)
          (tcp-packet-ack-number packet)
          (tcp-packet-flags packet)
          (tcp-packet-window-size packet)
          (bit-string-byte-count (tcp-packet-data packet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-accessible driver startup

(define (spawn-tcp-driver)
  (spawn-port-allocator 'tcp (lambda () (query-set tcp-ports (tcp-port-allocation $p _) p)))
  (spawn-kernel-tcp-driver)
  (spawn #:name 'tcp-inbound-driver
   (during/spawn (advertise (observe (tcp-channel _ ($ server-addr (tcp-listener _)) _)))
                 #:name (list 'tcp-listen server-addr)
                 (match-define (tcp-listener port) server-addr)
                 (assert (tcp-port-allocation port server-addr))
                 (on (asserted (advertise (tcp-channel ($ remote-addr (tcp-address _ _))
                                                       ($ local-addr (tcp-address _ port))
                                                       _)))
                     (spawn-relay server-addr remote-addr local-addr))))
  (spawn #:name 'tcp-outbound-driver
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
       (spawn-state-vector remote-ip remote-port appropriate-ip port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relay between kernel-level and user-level

(define relay-peer-wait-time-msec 5000)

(define (spawn-relay local-user-addr remote-addr local-tcp-addr)
  (define timer-name (list 'spawn-relay local-tcp-addr remote-addr))

  (spawn #:name (list 'tcp-relay local-user-addr remote-addr local-tcp-addr)
   (assert (tcp-port-allocation (tcp-address-port local-tcp-addr) local-user-addr))
   (assert (advertise (tcp-channel remote-addr local-user-addr _)))
   (assert (advertise (tcp-channel local-tcp-addr remote-addr _)))

   (field [local-peer-present? #f]
          [remote-peer-present? #f])

   (on-timeout relay-peer-wait-time-msec
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
       (send! (tcp-channel remote-addr local-user-addr bs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codec & kernel-level driver

(define PROTOCOL-TCP 6)

(define (spawn-kernel-tcp-driver)
  (spawn #:name 'kernel-tcp-driver
   (define local-ips (query-local-ip-addresses))

   (define active-state-vectors
     (query-set active-state-vectors
                (observe (tcp-packet #t $si $sp $di $dp _ _ _ _ _ _))
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
               (log-netstack/tcp-debug "TCP ~a" (summarize-tcp-packet packet))
               (when spawn-needed?
                 (log-netstack/tcp-debug "  - spawn needed!")
                 (active-state-vectors (set-add (active-state-vectors) statevec))
                 (spawn-state-vector src-ip src-port dst-ip dst-port))
               (send! packet)))
            (else #f))))
       (else #f)))

   (begin/dataflow
     (log-netstack/tcp-debug "SCN yielded statevecs ~v and local-ips ~v"
                             (active-state-vectors)
                             (local-ips)))

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
     (log-netstack/tcp-debug "TCP ~a" (summarize-tcp-packet p))
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
       (deliver-outbound-packet p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Per-connection state vector process

;;---------------------------------------------------------------------------
;; From the RFC:
;;
;;     Send Sequence Variables
;;
;;       SND.UNA - send unacknowledged
;;       SND.NXT - send next
;;       SND.WND - send window
;;       SND.UP  - send urgent pointer
;;       SND.WL1 - segment sequence number used for last window update
;;       SND.WL2 - segment acknowledgment number used for last window
;;                 update
;;       ISS     - initial send sequence number
;;
;;     Receive Sequence Variables
;;
;;       RCV.NXT - receive next
;;       RCV.WND - receive window
;;       RCV.UP  - receive urgent pointer
;;       IRS     - initial receive sequence number
;;
;;   The following diagrams may help to relate some of these variables to
;;   the sequence space.
;;
;;   Send Sequence Space
;;
;;                    1         2          3          4
;;               ----------|----------|----------|----------
;;                      SND.UNA    SND.NXT    SND.UNA
;;                                           +SND.WND
;;
;;         1 - old sequence numbers which have been acknowledged
;;         2 - sequence numbers of unacknowledged data
;;         3 - sequence numbers allowed for new data transmission
;;         4 - future sequence numbers which are not yet allowed
;;
;;                           Send Sequence Space
;;
;;                                Figure 4.
;;
;;   The send window is the portion of the sequence space labeled 3 in
;;   figure 4.
;;
;;   Receive Sequence Space
;;
;;                        1          2          3
;;                    ----------|----------|----------
;;                           RCV.NXT    RCV.NXT
;;                                     +RCV.WND
;;
;;         1 - old sequence numbers which have been acknowledged
;;         2 - sequence numbers allowed for new reception
;;         3 - future sequence numbers which are not yet allowed
;;
;;                          Receive Sequence Space
;;
;;                                Figure 5.
;;
;;   The receive window is the portion of the sequence space labeled 2 in
;;   figure 5.
;;
;;   There are also some variables used frequently in the discussion that
;;   take their values from the fields of the current segment.
;;
;;     Current Segment Variables
;;
;;       SEG.SEQ - segment sequence number
;;       SEG.ACK - segment acknowledgment number
;;       SEG.LEN - segment length
;;       SEG.WND - segment window
;;       SEG.UP  - segment urgent pointer
;;       SEG.PRC - segment precedence value
;;
;;---------------------------------------------------------------------------

(struct buffer (data ;; bit-string
		seqn ;; names leftmost byte in data
		window ;; counts bytes from leftmost byte in data
		finished?) ;; boolean: true after FIN
	#:transparent)

;; Regarding acks:
;;
;; - we send an ack number that is (buffer-seqn (inbound)) plus the
;;   number of buffered bytes.
;;
;; - acks received allow us to advance (buffer-seqn (outbound)) (that
;;   is, SND.UNA) to that point, discarding buffered data to do so.

;; Regarding windows:
;;
;; - (buffer-window (outbound)) is the size of the peer's receive
;;   window. Do not allow more than this many bytes to be
;;   unacknowledged on the wire.
;;
;; - (buffer-window (inbound)) is the size of our receive window. The
;;   peer should not exceed this; we should ignore data received that
;;   extends beyond this. Once we implement flow control locally
;;   (ahem) we should move this around, but at present it is fixed.

;; TODO: Zero receive window probe when we have something to say.

(define (buffer-push b data)
  (struct-copy buffer b [data (bit-string-append (buffer-data b) data)]))

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
  (not (seq>= b a)))

(define (seq>= a b)
  (< (seq- a b) #x80000000))

(define (seq-min a b) (if (seq> a b) b a))
(define (seq-max a b) (if (seq> a b) a b))

(module+ test
  (check-equal? (seq+ 41724780 1) 41724781)
  (check-equal? (seq+ 0 1) 1)
  (check-equal? (seq+ #x80000000 1) #x80000001)
  (check-equal? (seq+ #xffffffff 1) #x00000000)

  (check-equal? (seq> 41724780 41724780) #f)
  (check-equal? (seq> 41724781 41724780) #t)
  (check-equal? (seq> 41724780 41724781) #f)

  (check-equal? (seq> 0 0) #f)
  (check-equal? (seq> 1 0) #t)
  (check-equal? (seq> 0 1) #f)

  (check-equal? (seq> #x80000000 #x80000000) #f)
  (check-equal? (seq> #x80000001 #x80000000) #t)
  (check-equal? (seq> #x80000000 #x80000001) #f)

  (check-equal? (seq> #xffffffff #xffffffff) #f)
  (check-equal? (seq> #x00000000 #xffffffff) #t)
  (check-equal? (seq> #xffffffff #x00000000) #f)

  (check-equal? (seq>= 41724780 41724780) #t)
  (check-equal? (seq>= 41724781 41724780) #t)
  (check-equal? (seq>= 41724780 41724781) #f)

  (check-equal? (seq>= 0 0) #t)
  (check-equal? (seq>= 1 0) #t)
  (check-equal? (seq>= 0 1) #f)

  (check-equal? (seq>= #x80000000 #x80000000) #t)
  (check-equal? (seq>= #x80000001 #x80000000) #t)
  (check-equal? (seq>= #x80000000 #x80000001) #f)

  (check-equal? (seq>= #xffffffff #xffffffff) #t)
  (check-equal? (seq>= #x00000000 #xffffffff) #t)
  (check-equal? (seq>= #xffffffff #x00000000) #f))

(define (spawn-state-vector src-ip src-port dst-ip dst-port)
  (define src (tcp-address (ip-address->hostname src-ip) src-port))
  (define dst (tcp-address (ip-address->hostname dst-ip) dst-port))

  (spawn
   #:name (list 'tcp-state-vector
                (ip-address->hostname src-ip)
                src-port
                (ip-address->hostname dst-ip)
                dst-port)
   ;; Spawn with initial assertions so we are guaranteed to be sent
   ;; the packet that led to our creation (in the case of an accepted
   ;; server connection), and so that we at the same moment gain
   ;; knowledge of whether we were created on a listening port:
   #:assertions* (patch-added
                  (patch-seq (sub (tcp-packet #t src-ip src-port dst-ip dst-port ? ? ? ? ? ?))
                             (sub (observe (advertise (tcp-channel ? (tcp-listener dst-port) ?))))))

   (define root-facet (current-facet-id))

   (define initial-outbound-seqn
     ;; Yuck
     (inexact->exact (truncate (* #x100000000 (random)))))

   (field [outbound (buffer #"!" initial-outbound-seqn 0 #f)] ;; dummy data at SYN position
          [send-next initial-outbound-seqn] ;; SND.NXT
          [high-water-mark initial-outbound-seqn]

          [inbound (buffer #"" #f inbound-buffer-limit #f)]
          [transmission-needed? #f]
          [syn-acked? #f]

          [latest-peer-activity-time (current-inexact-milliseconds)]
          ;; ^ the most recent time we heard from our peer
          [user-timeout-base-time (current-inexact-milliseconds)]
          ;; ^ when the index of the first outbound unacknowledged byte changed

          ;; RFC 6298
          [rtt-estimate #f] ;; milliseconds; "SRTT"
          [rtt-mean-deviation #f] ;; milliseconds; "RTTVAR"
          [retransmission-timeout 1000] ;; milliseconds
          [retransmission-deadline #f]
          [rtt-estimate-seqn-target #f]
          [rtt-estimate-start-time #f]
          )

   (define (next-expected-seqn)
     (define b (inbound))
     (define v (buffer-seqn b))
     (and v (seq+ v (bit-string-byte-count (buffer-data b)))))

   (define (set-inbound-seqn! seqn)
     (inbound (struct-copy buffer (inbound) [seqn seqn])))

   (define (incorporate-segment! data)
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
     (when (not (buffer-finished? b))
       (unless (bit-string-empty? (buffer-data b)) ;; assured by deliver-inbound-locally
         (error 'check-fin "Nonempty inbound buffer"))
       (when (set-member? flags 'fin)
         (log-netstack/tcp-debug "Closing inbound stream.")
         (inbound (struct-copy buffer b
                    [seqn (seq+ (buffer-seqn b) 1)] ;; reliable: count fin as a byte
                    [finished? #t]))
         (transmission-needed? #t)))) ;; we must send an ack

   ;; -> Void
   (define (arm-retransmission-timer!)
     (log-netstack/tcp-debug "Arming retransmission timer (~a ms)" (retransmission-timeout))
     (retransmission-deadline (+ (current-inexact-milliseconds) (retransmission-timeout))))

   ;; Timestamp -> Void
   (define (start-rtt-estimate! now)
     (define target (send-next))
     (when (seq>= target (high-water-mark))
       (log-netstack/tcp-debug "Starting RTT estimation; target seqn is ~a" target)
       (rtt-estimate-start-time now)
       (rtt-estimate-seqn-target target)))

   ;; -> Void
   (define (reset-rtt-estimate!)
     (rtt-estimate-start-time #f)
     (rtt-estimate-seqn-target #f))

   ;; Timestamp -> Void
   (define (finish-rtt-estimate! now)
     (define rtt-measurement (- now (rtt-estimate-start-time)))
     (reset-rtt-estimate!)
     (log-netstack/tcp-debug "RTT measurement: ~a ms" rtt-measurement)
     ;; RFC 6298 Section 2.
     (cond [(rtt-estimate) => ;; we have a previous estimate, RFC 6298 rule (2.3)
            (lambda (prev-estimate)
              (rtt-mean-deviation (+ (* 0.75 (rtt-mean-deviation))
                                     (* 0.25 (abs (- rtt-measurement prev-estimate)))))
              (rtt-estimate (+ (* 0.875 prev-estimate)
                               (* 0.125 rtt-measurement))))]
           [else ;; no previous estimate, RFC 6298 rule (2.2) applies
            (rtt-estimate rtt-measurement)
            (rtt-mean-deviation (/ rtt-measurement 2))])
     (default-retransmission-timeout!)
     (log-netstack/tcp-debug "RTT measurement ~a ms; estimate ~a ms; mean deviation ~a ms; RTO ~a ms"
                             rtt-measurement
                             (rtt-estimate)
                             (rtt-mean-deviation)
                             (retransmission-timeout)))

   (define (default-retransmission-timeout!)
     (retransmission-timeout
      (max 200 ;; RFC 6298 rule (2.4), but cribbing from Linux's 200ms minimum
           (min 60000 ;; (2.5)
                (+ (rtt-estimate) (* 4 (rtt-mean-deviation))))))) ;; (2.2), (2.3)

   ;; Boolean SeqNum -> Void
   (define (discard-acknowledged-outbound! ack? ackn)
     (when ack?
       (let* ((b (outbound))
              (base (buffer-seqn b))
              (ackn (seq-min ackn (high-water-mark)))
              (ackn (seq-max ackn base))
              (dist (seq- ackn base)))
         (user-timeout-base-time (current-inexact-milliseconds))
         (when (positive? dist)
           (when (not (syn-acked?)) (syn-acked? #t))
           (log-netstack/tcp-debug "******** ackn ~a; send-next ~a; high-water-mark ~a"
                                   ackn
                                   (send-next)
                                   (high-water-mark))
           (when (seq> ackn (send-next)) (send-next ackn))
           (when (and (rtt-estimate-seqn-target) (seq>= ackn (rtt-estimate-seqn-target)))
             (finish-rtt-estimate! (current-inexact-milliseconds)))

           (define remaining-data (bit-string-drop (buffer-data b) (* dist 8))) ;; bit offset!
           (outbound (struct-copy buffer b [data remaining-data] [seqn ackn]))

           (default-retransmission-timeout!)
           (log-netstack/tcp-debug "Positive distance moved by ack, RTO now ~a"
                                   (retransmission-timeout))
           (arm-retransmission-timer!)))))

   ;; Nat -> Void
   (define (update-outbound-window! peer-window)
     (log-netstack/tcp-debug "Peer's receive-window is now ~a" peer-window)
     (outbound (struct-copy buffer (outbound) [window peer-window])))

   ;; True iff there is no queued-up data waiting either for
   ;; transmission or (if transmitted already) for acknowledgement.
   (define (all-output-acknowledged?)
     (bit-string-empty? (buffer-data (outbound))))

   (define (close-outbound-stream!)
     (define b (outbound))
     (when (not (buffer-finished? b))
       (outbound (struct-copy buffer (buffer-push b #"!") ;; dummy FIN byte
                   [finished? #t]))
       (transmission-needed? #t))) ;; the FIN machinery is awkwardly
                                   ;; different from the usual
                                   ;; advance-based decision on
                                   ;; whether to send a packet or not

   ;; SeqNum Boolean Boolean Bytes -> TcpPacket
   (define (build-outbound-packet seqn mention-syn? mention-fin? payload)
     (define ackn (next-expected-seqn))
     (define window (min 65535 ;; limit of field width
                         (max 0 ;; can't be negative
                              (- (buffer-window (inbound))
                                 (bit-string-byte-count (buffer-data (inbound)))))))

     (define flags (set))
     (when ackn         (set! flags (set-add flags 'ack)))
     (when mention-syn? (set! flags (set-add flags 'syn)))
     (when mention-fin? (set! flags (set-add flags 'fin)))

     (tcp-packet #f dst-ip dst-port src-ip src-port
                 seqn
                 (or ackn 0)
                 flags
                 window
                 #""
                 payload))

   (define (outbound-data-chunk offset length)
     (bit-string-take (bit-string-drop (buffer-data (outbound)) (* offset 8)) (* length 8)))

   ;; Transmit acknowledgements and outbound data.
   (begin/dataflow
     (define in-flight-count (seq- (send-next) (buffer-seqn (outbound))))

     (define-values (mention-syn? ;; whether to mention SYN
                     payload-size ;; how many bytes of payload data to include
                     mention-fin? ;; whether to mention FIN
                     advance)     ;; how far to advance send-next
       (if (syn-acked?)
           (let* ((effective-window (max 0 (- (buffer-window (outbound)) in-flight-count)))
                  (stream-ended? (buffer-finished? (outbound)))
                  (max-advance (- (bit-string-byte-count (buffer-data (outbound))) in-flight-count))
                  (payload-size (min maximum-segment-size effective-window max-advance)))
             (if (and stream-ended? ;; there's a FIN enqueued,
                      (positive? payload-size) ;; we aren't sending nothing at all,
                      (= payload-size max-advance)) ;; and our payload would cover the FIN
                 (values #f (- payload-size 1) #t payload-size)
                 (values #f payload-size #f payload-size)))
           (cond [(= in-flight-count 0) (values #t 0 #f 1)]
                 [(= in-flight-count 1) (values #t 0 #f 0)]
                 [else (error 'send-outbound!
                              "Invalid state: send-next had advanced too far before SYN")])))

     (when (and (or (next-expected-seqn) (local-peer-seen?))
                ;; ^ Talk only either if: we know the peer's seqn, or
                ;; we don't, but a local peer exists, which means
                ;; we're an outbound connection rather than a
                ;; listener.
                (or (transmission-needed?)
                    (positive? advance))
                ;; ^ ... and we have something to say. Something to
                ;; ack, or something to send.
                )
       (define packet-seqn (if mention-syn? (buffer-seqn (outbound)) (send-next)))
       (define packet (build-outbound-packet packet-seqn
                                             mention-syn?
                                             mention-fin?
                                             (outbound-data-chunk in-flight-count payload-size)))
       (when (positive? advance)
         (define new-send-next (seq+ (send-next) advance))
         (send-next new-send-next)
         (when (seq> new-send-next (high-water-mark))
           (high-water-mark new-send-next)))
       (when (transmission-needed?)
         (transmission-needed? #f))

       ;; (log-netstack/tcp-debug " sending ~v" packet)
       (send! packet)
       ;; (if (> (random) 0.5)
       ;;     (begin (log-netstack/tcp-debug "Send ~a" (summarize-tcp-packet packet))
       ;;            (send! packet))
       ;;     (log-netstack/tcp-debug "Drop ~a" (summarize-tcp-packet packet)))

       (when (or mention-syn? mention-fin? (positive? advance))
         (when (not (retransmission-deadline))
           (arm-retransmission-timer!))
         (when (not (rtt-estimate-start-time))
           (start-rtt-estimate! (current-inexact-milliseconds))))))

   (begin/dataflow
     (when (and (retransmission-deadline) (all-output-acknowledged?))
       (log-netstack/tcp-debug "All output acknowledged; disarming retransmission timer")
       (retransmission-deadline #f)))

   (on #:when (retransmission-deadline) (asserted (later-than (retransmission-deadline)))
     (send-next (buffer-seqn (outbound)))
     (log-netstack/tcp-debug "Retransmission deadline fired, RTO was ~a; reset to ~a"
                             (retransmission-timeout)
                             (send-next))
     (update-outbound-window! maximum-segment-size) ;; temporary. Will reopen on next ack
     (transmission-needed? #t)
     (retransmission-deadline #f)
     (reset-rtt-estimate!) ;; give up on current RTT estimation
     (retransmission-timeout (min 64000 (* 2 (retransmission-timeout))))
     (log-netstack/tcp-debug "  RTO now ~a" (retransmission-timeout)))

   (define (reset! seqn ackn)
     (define reset-packet (tcp-packet #f dst-ip dst-port src-ip src-port
                                      seqn
                                      ackn
                                      (set 'ack 'rst)
                                      0
                                      #""
                                      #""))
     (log-netstack/tcp-warning "Reset ~a" (summarize-tcp-packet reset-packet))
     (stop-facet root-facet)
     (send! reset-packet))

   (assert #:when (and (syn-acked?) (not (buffer-finished? (inbound))))
           (advertise (tcp-channel src dst _)))

   (on-start (log-netstack/tcp-info "Starting state vector ~a-~a" src-port dst-port))
   (on-stop  (log-netstack/tcp-info "Stopping state vector ~a-~a" src-port dst-port))

   (stop-when #:when (and (buffer-finished? (outbound))
                          (buffer-finished? (inbound))
                          (all-output-acknowledged?))
              (asserted (later-than (+ (latest-peer-activity-time)
                                       (* 2 1000 maximum-segment-lifetime-sec))))
    ;; Everything is cleanly shut down, and we just need to wait a while for unexpected
    ;; packets before we release the state vector.
    )

   (stop-when #:when (not (all-output-acknowledged?))
              (asserted (later-than (+ (user-timeout-base-time) user-timeout-msec)))
    ;; We've been plaintively retransmitting for user-timeout-msec without hearing anything
    ;; back; this is a crude approximation of the real condition for TCP_USER_TIMEOUT, but
    ;; it will do for now? TODO
    (log-netstack/tcp-warning "TCP_USER_TIMEOUT fired."))

   (define/query-value local-peer-seen? #f (observe (tcp-channel src dst _)) #t
     #:on-remove (begin
                   (log-netstack/tcp-debug "Closing outbound stream.")
                   (close-outbound-stream!)))

   (define/query-value listener-listening?
     #f
     (observe (advertise (tcp-channel _ (tcp-listener dst-port) _)))
     #t)

   (define (trigger-ack!)
     (transmission-needed? #t))

   (on (message (tcp-packet #t src-ip src-port dst-ip dst-port
                            $seqn $ackn $flags $window $options $data))
       (define expected (next-expected-seqn))
       (define is-syn? (set-member? flags 'syn))
       (define is-fin? (set-member? flags 'fin))
       (cond
         [(set-member? flags 'rst) (stop-facet root-facet)]
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
             (incorporate-segment! data)
             (trigger-ack!)]
            [(= expected seqn)
             (incorporate-segment! data)
             (when (positive? (bit-string-byte-count data)) (trigger-ack!))]
            [else
             (trigger-ack!)])
          (deliver-inbound-locally!)
          (check-fin! flags)
          (discard-acknowledged-outbound! (set-member? flags 'ack) ackn)
          (update-outbound-window! window)
          (latest-peer-activity-time (current-inexact-milliseconds))]))

   (on (message (tcp-channel dst src $bs))
       ;; (log-netstack/tcp-debug "GOT MORE STUFF TO DELIVER ~v" bs)

       (when (all-output-acknowledged?)
         ;; Only move user-timeout-base-time if there wasn't
         ;; already some outstanding output.
         (user-timeout-base-time (current-inexact-milliseconds)))

       (outbound (buffer-push (outbound) bs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-tcp-driver)
