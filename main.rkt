#lang minimart

(require minimart/demand-matcher)
(require minimart/drivers/timer)
(require (only-in mzlib/os gethostname))
(require "configuration.rkt")
(require "ethernet.rkt")
(require "arp.rkt")
(require "ip.rkt")
(require "tcp.rkt")
(require "udp.rkt")

;;(log-events-and-actions? #t)

(spawn-timer-driver)
(spawn-ethernet-driver)
(spawn-arp-driver)
(spawn-ip-driver)
(spawn-tcp-driver)
(spawn-udp-driver)

(spawn (lambda (e s) #f)
       (void)
       (match (gethostname)
	 ["skip"
	  (gestalt-union (pub (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) "en0"))
			 (pub (host-route (bytes 192 168 1 222) 24 "en0")))]
	 ["hop"
	  (gestalt-union (pub (gateway-route (bytes 0 0 0 0) 0 (bytes 192 168 1 1) "wlan0"))
			 (pub (host-route (bytes 192 168 1 222) 24 "wlan0")))]
	 ["stockholm.ccs.neu.edu"
	  (gestalt-union (pub (host-route (bytes 129 10 115 94) 24 "eth0"))
			 (pub (host-route (bytes 192 168 56 222) 24 "vboxnet0"))
			 (pub (gateway-route (bytes 0 0 0 0) 0 (bytes 129 10 115 1) "eth0")))]
	 [else
	  (error 'stack-configuration "No setup for hostname ~a" (gethostname))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (local-require racket/set racket/string)

  (define (spawn-session them us)
    (define user (gensym 'user))
    (define remote-detector (project-pubs #:meta-level 1 (?!)))
    (define peer-detector (project-pubs `(,(?!) says ,?)))
    (define (send-to-remote fmt . vs)
      (send #:meta-level 1 (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs)))))
    (define (say who fmt . vs)
      (unless (equal? who user) (send-to-remote "~a ~a\n" who (apply format fmt vs))))
    (list (send-to-remote "Welcome, ~a.\n" user)
	  (spawn (lambda (e old-peers)
		   (log-info "~a: ~v --> ~v" user e old-peers)
		   (match e
		     [(message (tcp-channel _ _ bs) 1 #f)
		      (transition old-peers
				  (send `(,user says ,(string-trim (bytes->string/utf-8 bs)))))]
		     [(message `(,who says ,what) 0 #f)
		      (transition old-peers (say who "says: ~a" what))]
		     [(routing-update g)
		      (define new-peers (gestalt-project/single g peer-detector))
		      (transition
		       new-peers
		       (list (when (matcher-empty? (gestalt-project g remote-detector)) (quit))
			     (for/list [(who (set-subtract new-peers old-peers))]
			       (say who "arrived."))
			     (for/list [(who (set-subtract old-peers new-peers))]
			       (say who "departed."))))]
		     [#f #f]))
		 (set)
		 (gestalt-union (sub `(,? says ,?))
				(sub `(,? says ,?) #:level 1)
				(pub `(,user says ,?))
				(sub (tcp-channel them us ?) #:meta-level 1)
				(sub (tcp-channel them us ?) #:meta-level 1 #:level 1)
				(pub (tcp-channel us them ?) #:meta-level 1)))))

  (spawn-world
   (spawn-demand-matcher (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 6667)) ?)
			 #:meta-level 1
			 spawn-session))

  )

(let ()
  (spawn (lambda (e s)
	   (match e
	     [(message (udp-packet src dst body) _ _)
	      (log-info "Got packet from ~v: ~v" src body)
	      (transition s (send (udp-packet dst
					      src
					      (string->bytes/utf-8 (format "You said: ~a" body)))))]
	     [_ #f]))
	 (void)
	 (gestalt-union (sub (udp-packet ? (udp-listener 6667) ?)))))

(spawn (lambda (e s)
	 (local-require racket/pretty)
	 (match e
	   [(message m _ _)
	    ;; (pretty-write `(MAIN ,m))
	    (void)]
	   [(routing-update g)
	    ;; (printf "MAIN gestalt:\n")
	    ;; (pretty-print-gestalt g)
	    (void)]
	   [_ (void)])
	 (flush-output)
	 #f)
       (void)
       (gestalt-union
	(sub ? #:level 5)
	(pub ? #:level 5)
	;;(sub (tcp-channel ? ? ?) #:level 5)
	))
