#lang syndicate/actor
;; Ethernet driver

(provide (struct-out ethernet-packet)
	 zero-ethernet-address
	 broadcast-ethernet-address
	 interface-names
	 spawn-ethernet-driver
	 ethernet-packet-pattern
	 lookup-ethernet-hwaddr)

(require/activate syndicate/drivers/timer)
(require racket/set)
(require racket/match)
(require racket/async-channel)

(require packet-socket)
(require bitsyntax)

(require "configuration.rkt")
(require "dump-bytes.rkt")

(struct ethernet-packet (interface from-wire? source destination ethertype body) #:prefab)

(define zero-ethernet-address (bytes 0 0 0 0 0 0))
(define broadcast-ethernet-address (bytes 255 255 255 255 255 255))

(define interface-names (raw-interface-names))
(log-info "Device names: ~a" interface-names)

(define (spawn-ethernet-driver)
  (actor #:name 'ethernet-driver
   (react (during/actor
           (observe (ethernet-packet (ethernet-interface $interface-name _) #t _ _ _ _))
           #:name (list 'ethernet-interface interface-name)

           (define h (raw-interface-open interface-name))
           (when (not h) (error 'ethernet "Couldn't open interface ~v" interface-name))
           (log-info "Opened interface ~a, yielding handle ~v" interface-name h)

           (define interface (ethernet-interface interface-name (raw-interface-hwaddr h)))
           (assert interface)

           (define control-ch (make-async-channel))
           (thread (lambda () (interface-packet-read-loop interface h control-ch)))

           (on-start (flush!) ;; ensure all subscriptions are in place
                     (async-channel-put control-ch 'unblock)
                     (actor #:name (list 'ethernet-interface-quit-monitor interface-name)
                            (react (on (retracted interface)
                                       (async-channel-put control-ch 'quit)))))

           (on (message (inbound ($ p (ethernet-packet interface #t _ _ _ _))))
               ;; (log-info "Interface ~a inbound packet ~a -> ~a (type 0x~a)"
               ;;           (ethernet-interface-name (ethernet-packet-interface p))
               ;;           (pretty-bytes (ethernet-packet-source p))
               ;;           (pretty-bytes (ethernet-packet-destination p))
               ;;           (number->string (ethernet-packet-ethertype p) 16))
               ;; (log-info "~a" (dump-bytes->string (ethernet-packet-body p)))
               (send! p))

           (on (message ($ p (ethernet-packet interface #f _ _ _ _)))
               ;; (log-info "Interface ~a OUTBOUND packet ~a -> ~a (type 0x~a)"
               ;;           (ethernet-interface-name (ethernet-packet-interface p))
               ;;           (pretty-bytes (ethernet-packet-source p))
               ;;           (pretty-bytes (ethernet-packet-destination p))
               ;;           (number->string (ethernet-packet-ethertype p) 16))
               ;; (log-info "~a" (dump-bytes->string (ethernet-packet-body p)))
               (raw-interface-write h (encode-ethernet-packet p)))))))

(define (interface-packet-read-loop interface h control-ch)
  (define (blocked)
    (match (async-channel-get control-ch)
      ['unblock (unblocked)]
      ['quit (void)]))
  (define (unblocked)
    (match (async-channel-try-get control-ch)
      ['unblock (unblocked)]
      ['quit (void)]
      [#f
       (define p (raw-interface-read h))
       (define decoded (decode-ethernet-packet interface p))
       (when decoded (send-ground-message decoded))
       (unblocked)]))
  (blocked)
  (raw-interface-close h))

(define (decode-ethernet-packet interface p)
  (bit-string-case p
    ([ (destination :: binary bytes 6)
       (source :: binary bytes 6)
       (ethertype :: integer bytes 2)
       (body :: binary) ]
     (ethernet-packet interface
		      #t
		      (bit-string->bytes source)
		      (bit-string->bytes destination)
		      ethertype
		      (bit-string->bytes body)))
    (else #f)))

(define (encode-ethernet-packet p)
  (match-define (ethernet-packet _ _ source destination ethertype body) p)
  (bit-string->bytes
   (bit-string (destination :: binary bytes 6)
	       (source :: binary bytes 6)
	       (ethertype :: integer bytes 2)
	       (body :: binary))))

(define (ethernet-packet-pattern interface-name from-wire? ethertype)
  (ethernet-packet (ethernet-interface interface-name ?) from-wire? ? ? ethertype ?))

(define (lookup-ethernet-hwaddr interface-name)
  (define timer-id (gensym 'lookup-ethernet-hwaddr))
  (react/suspend (k)
                 (on-start (send! (set-timer timer-id 5000 'relative)))
                 (stop-when (message (timer-expired timer-id _))
                            (log-info "Lookup of ethernet interface ~v failed" interface-name)
                            (k #f))
                 (stop-when (asserted (ethernet-interface interface-name $hwaddr))
                            (k hwaddr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-ethernet-driver)
