#lang racket/base
;; Ethernet driver

(provide (struct-out ethernet-interface)
	 (struct-out ethernet-packet)
	 zero-ethernet-address
	 broadcast-ethernet-address
	 interface-names
	 spawn-ethernet-driver
	 gestalt->hwaddr
	 ethernet-packet-pattern)

(require racket/set)
(require racket/match)
(require racket/async-channel)

(require minimart)
(require minimart/demand-matcher)

(require packet-socket)
(require bitsyntax)

(require "dump-bytes.rkt")

(struct ethernet-interface (name hwaddr) #:prefab)
(struct ethernet-packet (interface from-wire? source destination ethertype body) #:prefab)

(define zero-ethernet-address (bytes 0 0 0 0 0 0))
(define broadcast-ethernet-address (bytes 255 255 255 255 255 255))

(define interface-names (raw-interface-names))
(log-info "Device names: ~a" interface-names)

(define (spawn-ethernet-driver)
  (spawn-demand-matcher (ethernet-packet (ethernet-interface (?!) ?) #t ? ? ? ?)
			#:demand-is-subscription? #t
			spawn-interface-tap))

(define (spawn-interface-tap interface-name)
  (define h (raw-interface-open interface-name))
  (define interface (ethernet-interface interface-name (raw-interface-hwaddr h)))
  (cond
   [(not h)
    (log-error "ethernet: Couldn't open interface ~v" interface-name)
    '()]
   [else
    (log-info "Opened interface ~a, yielding handle ~v" interface-name h)
    (define control-ch (make-async-channel))
    (thread (lambda () (interface-packet-read-loop interface h control-ch)))
    (spawn (lambda (e h)
	     (match e
	       [(routing-update g)
		(if (gestalt-empty? g)
		    (begin (async-channel-put control-ch 'quit)
			   (transition #f (quit)))
		    (begin (async-channel-put control-ch 'unblock)
			   #f))]
	       [(message (? ethernet-packet? p) 1 #f) ;; from metalevel 1
		;; (log-info "Interface ~a inbound packet ~a -> ~a (type 0x~a)"
		;; 	  (ethernet-interface-name (ethernet-packet-interface p))
		;; 	  (pretty-bytes (ethernet-packet-source p))
		;; 	  (pretty-bytes (ethernet-packet-destination p))
		;; 	  (number->string (ethernet-packet-ethertype p) 16))
		;; (log-info "~a" (dump-bytes->string (ethernet-packet-body p)))
		(transition h (send p))]
	       [(message (? ethernet-packet? p) 0 #f) ;; from metalevel 0
		;; (log-info "Interface ~a OUTBOUND packet ~a -> ~a (type 0x~a)"
		;; 	  (ethernet-interface-name (ethernet-packet-interface p))
		;; 	  (pretty-bytes (ethernet-packet-source p))
		;; 	  (pretty-bytes (ethernet-packet-destination p))
		;; 	  (number->string (ethernet-packet-ethertype p) 16))
		;; (log-info "~a" (dump-bytes->string (ethernet-packet-body p)))
		(raw-interface-write h (encode-ethernet-packet p))
		#f]
	       [_ #f]))
	   h
	   (gestalt-union (pub (ethernet-packet interface #t ? ? ? ?))
			  (pub (ethernet-packet interface #t ? ? ? ?) #:level 1)
			  (sub (ethernet-packet interface #f ? ? ? ?))
			  (sub (ethernet-packet interface #t ? ? ? ?) #:meta-level 1)))]))

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

(define (hwaddr-projection interface-name)
  (compile-gestalt-projection (ethernet-packet (ethernet-interface interface-name (?!)) ? ? ? ? ?)))

(define (gestalt->hwaddr g interface-name)
  (define hwaddrs
    (matcher-key-set/single (gestalt-project g 0 0 #t (hwaddr-projection interface-name))))
  (case (set-count hwaddrs)
    [(0) #f]
    [(1) (set-first hwaddrs)]
    [else
     (log-warning "gestalt->hwaddr: multiple addresses for interface ~a: ~v" interface-name hwaddrs)
     (set-first hwaddrs)]))

(define (ethernet-packet-pattern interface-name from-wire? ethertype)
  (ethernet-packet (ethernet-interface interface-name ?) from-wire? ? ? ethertype ?))
