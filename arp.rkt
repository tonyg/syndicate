#lang racket/base
;; ARP protocol, http://tools.ietf.org/html/rfc826
;; Only does ARP-over-ethernet.

(provide (struct-out arp-query)
	 (struct-out arp-assertion)
	 (struct-out arp-interface)
	 spawn-arp-driver)

(require racket/set)
(require racket/match)
(require minimart)
(require minimart/drivers/timer)
(require minimart/demand-matcher)
(require bitsyntax)

(require "dump-bytes.rkt")
(require "configuration.rkt")
(require "ethernet.rkt")

(struct arp-query (protocol protocol-address interface link-address) #:prefab)
(struct arp-assertion (protocol protocol-address interface-name) #:prefab)
(struct arp-interface (interface-name) #:prefab)

(define ARP-ethertype #x0806)
(define cache-entry-lifetime-msec (* 14400 1000))
(define wakeup-interval 5000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spawn-arp-driver)
  (spawn-demand-matcher (arp-interface (?!))
			#:supply-level 1
			spawn-arp-interface))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct cache-key (protocol address) #:transparent)
(struct cache-value (expiry interface address) #:transparent)

(struct state (cache queries assertions) #:transparent)

(define (spawn-arp-interface interface-name)
  (log-info "spawn-arp-interface ~v" interface-name)
  (lookup-ethernet-hwaddr (gestalt-for-supply interface-name)
			  interface-name
			  (lambda (hwaddr) (spawn-arp-interface* interface-name hwaddr))))

(define (gestalt-for-supply interface-name)
  (sub (arp-interface interface-name) #:level 1))

(define (spawn-arp-interface* interface-name hwaddr)
  (log-info "spawn-arp-interface* ~v ~v" interface-name hwaddr)
  (define interface (ethernet-interface interface-name hwaddr))

  (define (expire-cache cache)
    (define now (current-inexact-milliseconds))
    (define (not-expired? v) (< now (cache-value-expiry v)))
    (for/hash [((k v) (in-hash cache)) #:when (not-expired? v)]
      (values k v)))

  (define timer-key (list 'arp interface-name))

  (define (set-wakeup-alarm)
    (send (set-timer timer-key wakeup-interval 'relative)))

  (define (compute-gestalt cache)
    (gestalt-union (sub (timer-expired timer-key ?))
		   (sub (ethernet-packet-pattern interface-name #t ARP-ethertype))
		   (sub (ethernet-packet-pattern interface-name #t ARP-ethertype) #:level 1)
		   (pub (ethernet-packet-pattern interface-name #f ARP-ethertype))
		   (gestalt-for-supply interface-name)
		   (sub (arp-assertion ? ? interface-name) #:level 1)
		   (pub (arp-query ? ? interface ?) #:level 2)
		   (for/fold [(g (gestalt-empty))] [((k v) (in-hash cache))]
		     (gestalt-union g (pub (arp-query (cache-key-protocol k)
						      (cache-key-address k)
						      (cache-value-interface v)
						      (cache-value-address v)))))))

  (define (build-packet s dest-mac ptype oper sender-ha sender-pa target-ha target-pa)
    (define hlen (bytes-length target-ha))
    (define plen (bytes-length target-pa))
    (define packet (bit-string->bytes
		    (bit-string (1 :: integer bytes 2)
				(ptype :: integer bytes 2)
				hlen
				plen
				(oper :: integer bytes 2)
				(sender-ha :: binary bytes hlen)
				(sender-pa :: binary bytes plen)
				(target-ha :: binary bytes hlen)
				(target-pa :: binary bytes plen))))
    (ethernet-packet interface
		     #f
		     hwaddr
		     dest-mac
		     ARP-ethertype
		     packet))

  (define (analyze-incoming-packet source destination body s)
    (bit-string-case body
      ([ (= 1 :: integer bytes 2)
	 (ptype :: integer bytes 2)
	 hlen
	 plen
	 (oper :: integer bytes 2)
	 (sender-hardware-address0 :: binary bytes hlen)
	 (sender-protocol-address0 :: binary bytes plen)
	 (target-hardware-address0 :: binary bytes hlen)
	 (target-protocol-address0 :: binary bytes plen)
	 (:: binary) ;; TODO: are the extra zeros coming from the
		     ;; router real, or an artifact of my
		     ;; packet-capture implementation?
	 ]
       (let ()
	 (define sender-protocol-address (bit-string->bytes sender-protocol-address0))
	 (define sender-hardware-address (bit-string->bytes sender-hardware-address0))
	 (define target-protocol-address (bit-string->bytes target-protocol-address0))
	 ;; (log-info "~a ARP Adding ~a = ~a to cache"
	 ;; 	   interface-name
	 ;; 	   (pretty-bytes sender-protocol-address)
	 ;; 	   (pretty-bytes sender-hardware-address))
	 (define cache (hash-set (expire-cache (state-cache s))
				 (cache-key ptype sender-protocol-address)
				 (cache-value (+ (current-inexact-milliseconds)
						 cache-entry-lifetime-msec)
					      interface
					      sender-hardware-address)))
	 (transition (struct-copy state s
		       [cache cache])
		     (list
		      (case oper
			[(1) ;; request
			 (if (set-member? (state-assertions s)
					  (cache-key ptype target-protocol-address))
			     (send (build-packet s
						 sender-hardware-address
						 ptype
						 2 ;; reply
						 hwaddr
						 target-protocol-address
						 sender-hardware-address
						 sender-protocol-address))
			     '())]
			[(2) '()] ;; reply
			[else '()])
		      (routing-update (compute-gestalt cache))))))
      (else #f)))

  (define queries-projection (project-subs #:level 1 (arp-query (?!) (?!) ? ?)))
  (define (gestalt->queries g)
    (for/set [(e (in-set (gestalt-project/keys g queries-projection)))]
      (match-define (list ptype pa) e)
      (cache-key ptype pa)))

  (define assertions-projection (project-pubs (arp-assertion (?!) (?!) ?)))
  (define (gestalt->assertions g)
    (for/set [(e (in-set (gestalt-project/keys g assertions-projection)))]
      (match-define (list ptype pa) e)
      (cache-key ptype pa)))

  (define (analyze-gestalt g s)
    (define new-queries (gestalt->queries g))
    (define new-assertions (gestalt->assertions g))
    (define added-queries (set-subtract new-queries (state-queries s)))
    (define added-assertions (set-subtract new-assertions (state-assertions s)))
    (define unanswered-queries (set-subtract added-queries (list->set (hash-keys (state-cache s)))))
    (define new-s (struct-copy state s [queries new-queries] [assertions new-assertions]))
    (define (some-asserted-pa ptype)
      (match (filter (lambda (k) (equal? (cache-key-protocol k) ptype))
		     (set->list new-assertions))
	['() #f]
	[(list* k _) (cache-key-address k)]))
    ;; (log-info "analyze-gestalt: g:\n~a" (gestalt->pretty-string g))
    ;; (log-info "analyze-gestalt: new-queries ~v" new-queries)
    ;; (log-info "analyze-gestalt: new-assertions ~v" new-assertions)
    ;; (log-info "analyze-gestalt: added-queries ~v" added-queries)
    ;; (log-info "analyze-gestalt: added-assertions ~v" added-assertions)
    ;; (log-info "analyze-gestalt: unanswered-queries ~v" unanswered-queries)
    ;; (log-info "analyze-gestalt: new-s ~v" new-s)
    (transition new-s
		(list
		 (when (gestalt-empty? (gestalt-filter g (gestalt-for-supply interface-name)))
		   (quit))
		 (for/list [(q (in-set unanswered-queries))]
		   (define pa (some-asserted-pa (cache-key-protocol q)))
		   (log-info "~a ARP Asking for ~a from ~a"
			     interface-name
			     (pretty-bytes (cache-key-address q))
			     (and pa (pretty-bytes pa)))
		   (if pa
		       (send (build-packet new-s
					   broadcast-ethernet-address
					   (cache-key-protocol q)
					   1 ;; request
					   hwaddr
					   pa
					   zero-ethernet-address
					   (cache-key-address q)))
		       '()))
		 (for/list [(a (in-set added-assertions))]
		   (log-info "~a ARP Announcing ~a as ~a"
			     interface-name
			     (pretty-bytes (cache-key-address a))
			     (pretty-bytes hwaddr))
		   (send (build-packet new-s
				       broadcast-ethernet-address
				       (cache-key-protocol a)
				       2 ;; reply -- gratuitous announcement
				       hwaddr
				       (cache-key-address a)
				       hwaddr
				       (cache-key-address a)))))))

  (list (set-wakeup-alarm)
	(spawn (lambda (e s)
		 ;; (log-info "ARP ~a ~a: ~v // ~v" interface-name (pretty-bytes hwaddr) e s)
		 (match e
		   [(routing-update g)
		    (analyze-gestalt g s)]
		   [(message (ethernet-packet _ _ source destination _ body) _ _)
		    (analyze-incoming-packet source destination body s)]
		   [(message (timer-expired _ _) _ _)
		    (define new-s (struct-copy state s
				    [cache (expire-cache (state-cache s))]))
		    (transition new-s
				(list (set-wakeup-alarm)
				      (routing-update (compute-gestalt (state-cache new-s)))))]
		   [_ #f]))
	       (state (hash) (set) (set))
	       (compute-gestalt (hash)))))
