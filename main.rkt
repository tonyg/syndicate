#lang minimart

(require minimart/demand-matcher)
(require minimart/drivers/timer)
(require "ethernet.rkt")
(require "arp.rkt")
(require "ip.rkt")
(require "tcp.rkt")

(define interface "vboxnet0")

;;(log-events-and-actions? #t)

(spawn-timer-driver)
(spawn-ethernet-driver)
(spawn-arp-driver interface)
(spawn-ip-driver interface (bytes 192 168 56 222))
(spawn-tcp-driver)

(let ()
  (local-require racket/set racket/string)

  (define (spawn-session them us)
    (define user (gensym 'user))
    (define remote-detector (compile-gestalt-projection (?!)))
    (define peer-detector (compile-gestalt-projection `(,(?!) says ,?)))
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
		      (define new-peers
			(matcher-key-set/single (gestalt-project g 0 0 #t peer-detector)))
		      (transition
		       new-peers
		       (list (when (matcher-empty? (gestalt-project g 1 0 #t remote-detector))
			       (quit))
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
   (spawn-demand-matcher (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 5999)) ?)
			 #:meta-level 1
			 spawn-session))

  )

(spawn (lambda (e s)
	 (local-require racket/pretty)
	 (match e
	   [(message m _ _)
	    (pretty-write `(MAIN ,m))]
	   [(routing-update g)
	    (printf "MAIN gestalt:\n")
	    (pretty-print-gestalt g)]
	   [_ (void)])
	 (flush-output)
	 #f)
       (void)
       (gestalt-union
	;;(sub ? #:level 5)
	(sub (tcp-channel ? ? ?) #:level 5)
	))
