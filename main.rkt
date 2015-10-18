#lang minimart

(require minimart/demand-matcher)
(require minimart/drivers/timer)
(require "demo-config.rkt")
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
(spawn-demo-config)

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

(let ()
  (define (spawn-session them us)
    (list
     (send 'bump)
     (spawn (lambda (e s)
	      (match e
		[(message `(counter ,counter) _ _)
		 (define response
		   (string->bytes/utf-8
		    (format (string-append
			     "HTTP/1.0 200 OK\r\n\r\n"
			     "<h1>Hello world from minimart-netstack!</h1>\n"
			     "<p>This is running on minimart's own\n"
			     "<a href='https://github.com/tonyg/minimart-netstack/'>\n"
			     "TCP/IP stack</a>.</p>\n"
			     "<p>There have been ~a requests prior to this one.</p>")
			    counter)))
		 (transition s (list (send #:meta-level 1 (tcp-channel us them response))
				     (quit)))]
		[_ #f]))
	    (void)
	    (gestalt-union (sub `(counter ,?))
			   (sub (tcp-channel them us ?) #:meta-level 1)
			   (sub (tcp-channel them us ?) #:meta-level 1 #:level 1)
			   (pub (tcp-channel us them ?) #:meta-level 1)))))

  (spawn-world
   (spawn (lambda (e counter)
	    (match e
	      [(message 'bump _ _)
	       (transition (+ counter 1) (send `(counter ,counter)))]
	      [_ #f]))
	  0
	  (gestalt-union (sub 'bump)
			 (pub `(counter ,?))))
   (spawn-demand-matcher (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 80)) ?)
			 #:meta-level 1
			 spawn-session))

  )

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
