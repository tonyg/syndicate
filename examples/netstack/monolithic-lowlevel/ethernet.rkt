#lang racket/base
;; Ethernet driver

(provide (struct-out ethernet-packet)
	 zero-ethernet-address
	 broadcast-ethernet-address
	 interface-names
	 spawn-ethernet-driver
	 ethernet-packet-pattern
	 lookup-ethernet-hwaddr)

(require racket/set)
(require racket/match)
(require racket/async-channel)

(require syndicate/monolithic)
(require syndicate/demand-matcher)
(require "on-claim.rkt")

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
  (spawn-demand-matcher (observe (ethernet-packet (ethernet-interface (?!) ?) #t ? ? ? ?))
                        (ethernet-interface (?!) ?)
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
    (actor (lambda (e h)
	     (match e
               [(scn g)
		(if (trie-empty? g)
		    (begin (async-channel-put control-ch 'quit)
                           (quit))
		    (begin (async-channel-put control-ch 'unblock)
			   #f))]
	       [(message (inbound (? ethernet-packet? p)))
		;; (log-info "Interface ~a inbound packet ~a -> ~a (type 0x~a)"
		;; 	  (ethernet-interface-name (ethernet-packet-interface p))
		;; 	  (pretty-bytes (ethernet-packet-source p))
		;; 	  (pretty-bytes (ethernet-packet-destination p))
		;; 	  (number->string (ethernet-packet-ethertype p) 16))
		;; (log-info "~a" (dump-bytes->string (ethernet-packet-body p)))
		(transition h (message p))]
	       [(message (? ethernet-packet? p))
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
           (scn/union (assertion interface)
                      (subscription (ethernet-packet interface #f ? ? ? ?))
                      (subscription (observe (ethernet-packet interface #t ? ? ? ?)))
                      (subscription (inbound (ethernet-packet interface #t ? ? ? ?)))))]))

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

(define (lookup-ethernet-hwaddr base-interests interface-name k)
  (on-claim #:timeout-msec 5000
            #:on-timeout (lambda ()
                           (log-info "Lookup of ethernet interface ~v failed" interface-name)
                           '())
            (lambda (_g hwaddrss)
              (and (not (set-empty? hwaddrss))
                   (let ((hwaddr (car (set-first hwaddrss))))
                     (k hwaddr))))
            base-interests
            (ethernet-interface interface-name (?!))))
