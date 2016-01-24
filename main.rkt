#lang prospect-monolithic

(require prospect-monolithic/demand-matcher)
(require prospect-monolithic/drivers/timer)
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
    (define remote-detector (compile-projection (at-meta (?!))))
    (define peer-detector (compile-projection (advertise `(,(?!) says ,?))))
    (define (send-to-remote fmt . vs)
      (message (at-meta (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))
    (define (say who fmt . vs)
      (unless (equal? who user) (send-to-remote "~a ~a\n" who (apply format fmt vs))))
    (list (send-to-remote "Welcome, ~a.\n" user)
          (spawn
           (lambda (e peers)
             (match e
               [(message (at-meta (tcp-channel _ _ bs)))
                (transition peers (message `(,user says ,(string-trim (bytes->string/utf-8 bs)))))]
               [(message `(,who says ,what))
                (transition peers (say who "says: ~a" what))]
               [(scn assertions)
                (if (trie-empty? (trie-project assertions remote-detector))
                    (quit (send-to-remote "Goodbye!\n"))
                    (let ((new-peers (trie-project/set/single assertions peer-detector)))
                      (define arrived (set-subtract new-peers peers))
                      (define departed (set-subtract peers new-peers))
                      (transition new-peers
                                  (list (for/list [(who arrived)] (say who "arrived."))
                                        (for/list [(who departed)] (say who "departed."))))))]
               [#f #f]))
           (set)
           (scn/union
            (subscription `(,? says ,?)) ;; read actual chat messages
            (subscription (advertise `(,? says ,?))) ;; observe peer presence
            (advertisement `(,user says ,?)) ;; advertise our presence
            (subscription (tcp-channel them us ?) #:meta-level 1) ;; read from remote client
            (subscription (advertise (tcp-channel them us ?)) #:meta-level 1) ;; monitor remote client
            (advertisement (tcp-channel us them ?) #:meta-level 1) ;; we will write to remote client
            ))))

  (spawn-network
   (spawn-demand-matcher (advertise (tcp-channel (?!) (?! (tcp-listener 5999)) ?))
                         (observe (tcp-channel (?!) (?! (tcp-listener 5999)) ?))
                         #:meta-level 1
                         spawn-session))
  )

(let ()
  (spawn (lambda (e s)
	   (match e
	     [(message (udp-packet src dst body))
	      (log-info "Got packet from ~v: ~v" src body)
	      (transition s (message
                             (udp-packet dst
                                         src
                                         (string->bytes/utf-8 (format "You said: ~a" body)))))]
	     [_ #f]))
	 (void)
	 (scn (subscription (udp-packet ? (udp-listener 6667) ?)))))

#;(let ()
  (define (spawn-session them us)
    (list
     (send 'bump)
     (spawn (lambda (e s)
	      (match e
		[(message `(counter ,counter))
		 (define response
		   (string->bytes/utf-8
		    (format (string-append
			     "HTTP/1.0 200 OK\r\n\r\n"
			     "<h1>Hello world from prospect-monolithic-netstack!</h1>\n"
			     "<p>This is running on prospect-monolithic's own\n"
			     "<a href='https://github.com/tonyg/prospect-netstack/'>\n"
			     "TCP/IP stack</a>.</p>\n"
			     "<p>There have been ~a requests prior to this one.</p>")
			    counter)))
		 (transition s (list (message #:meta-level 1 (tcp-channel us them response))
				     (quit)))]
		[_ #f]))
	    (void)
	    (scn/union (subscription `(counter ,?))
                       (subscription (tcp-channel them us ?) #:meta-level 1)
                       (subscription (advertise (tcp-channel them us ?)) #:meta-level 1)
                       (advertisement (tcp-channel us them ?) #:meta-level 1)))))

  (spawn-world
   (spawn (lambda (e counter)
	    (match e
	      [(message 'bump _ _)
	       (transition (+ counter 1) (send `(counter ,counter)))]
	      [_ #f]))
	  0
	  (scn (subscription 'bump)))
   (spawn-demand-matcher (advertise (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 80)) ?))
                         (observe (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 80)) ?))
			 #:meta-level 1
			 spawn-session))

  )
