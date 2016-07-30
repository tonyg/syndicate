#lang syndicate/monolithic

(require syndicate/demand-matcher)
(require syndicate/drivers/timer)
(require syndicate/protocol/advertise)
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
    (define remote-detector (inbound (?!)))
    (define peer-detector (advertise `(,(?!) says ,?)))
    (define (send-to-remote fmt . vs)
      (message (outbound (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))
    (define (say who fmt . vs)
      (unless (equal? who user) (send-to-remote "~a ~a\n" who (apply format fmt vs))))
    (list (send-to-remote "Welcome, ~a.\n" user)
          (spawn
           (lambda (e peers)
             (match e
               [(message (inbound (tcp-channel _ _ bs)))
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
            (subscription (inbound (tcp-channel them us ?))) ;; read from remote client
            (subscription (inbound (advertise (tcp-channel them us ?)))) ;; monitor remote client
            (advertisement (inbound (tcp-channel us them ?))) ;; we will write to remote client
            ))))

  (spawn-dataspace
   (spawn-demand-matcher (inbound (advertise (tcp-channel (?!) (?! (tcp-listener 5999)) ?)))
                         (inbound (observe (tcp-channel (?!) (?! (tcp-listener 5999)) ?)))
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

(let ()
  (define (spawn-session them us)
    (list
     (message 'bump)
     (spawn (lambda (e s)
	      (match e
		[(message `(counter ,counter))
		 (define response
		   (string->bytes/utf-8
		    (format (string-append
			     "HTTP/1.0 200 OK\r\n\r\n"
			     "<h1>Hello world from syndicate-monolithic-netstack!</h1>\n"
			     "<p>This is running on syndicate-monolithic's own\n"
			     "<a href='https://github.com/tonyg/syndicate/'>\n"
			     "TCP/IP stack</a>.</p>\n"
			     "<p>There have been ~a requests prior to this one.</p>")
			    counter)))
		 (quit (message (outbound (tcp-channel us them response))))]
		[_ #f]))
	    (void)
	    (scn/union (subscription `(counter ,?))
                       (subscription (inbound (tcp-channel them us ?)))
                       (subscription (inbound (advertise (tcp-channel them us ?))))
                       (advertisement (inbound (tcp-channel us them ?)))))))

  (spawn-dataspace
   (spawn (lambda (e counter)
	    (match e
	      [(message 'bump)
	       (transition (+ counter 1) (message `(counter ,counter)))]
	      [_ #f]))
	  0
	  (scn (subscription 'bump)))
   (spawn-demand-matcher
    (inbound (advertise (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 80)) ?)))
    (inbound (observe (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 80)) ?)))
    spawn-session))

  )
