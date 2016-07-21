#lang syndicate/actor


;;(log-events-and-actions? #t)

(require/activate syndicate/drivers/timer)
(require/activate "ethernet.rkt")
(require/activate "arp.rkt")
(require/activate "ip.rkt")
(require/activate "tcp.rkt")
(require/activate "udp.rkt")
(require/activate "demo-config.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (local-require (only-in racket/string string-trim))

  (struct says (who what) #:prefab)
  (struct present (who) #:prefab)

  (define (spawn-session them us)
    (actor (define (send-to-remote fmt . vs)
             (send! (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs)))
                    #:meta-level 1))

           (define (say who fmt . vs)
             (unless (equal? who user)
               (send-to-remote "~a ~a\n" who (apply format fmt vs))))

           (define user (gensym 'user))
           (send-to-remote "Welcome, ~a.\n" user)

           (until (retracted (advertise (tcp-channel them us _)) #:meta-level 1)
                  (assert (present user))
                  (on (asserted (present $who)) (say who "arrived."))
                  (on (retracted (present $who)) (say who "departed."))

                  (on (message (says $who $what)) (say who "says: ~a" what))

                  (assert (advertise (tcp-channel us them _)) #:meta-level 1)
                  (on (message (tcp-channel them us $bs) #:meta-level 1)
                      (send! (says user (string-trim (bytes->string/utf-8 bs))))))))

  (dataspace (define us (tcp-listener 5999))
             (forever (assert (advertise (observe (tcp-channel _ us _))) #:meta-level 1)
                      (on (asserted (advertise (tcp-channel $them us _)) #:meta-level 1)
                          (spawn-session them us)))))

(let ((dst (udp-listener 6667)))
  (actor (react
          (on (message (udp-packet $src dst $body))
	      (log-info "Got packet from ~v: ~v" src body)
              (send! (udp-packet dst src (string->bytes/utf-8 (format "You said: ~a" body))))))))

(let ()
  (dataspace
   (actor (react (field [counter 0])
                 (on (message 'bump)
                     (send! `(counter ,(counter)))
                     (counter (+ (counter) 1)))))

   (forever (define us (tcp-listener 80))
            (assert (advertise (observe (tcp-channel _ us _))) #:meta-level 1)
            (during/actor (advertise (tcp-channel ($ them (tcp-address _ _)) us _)) #:meta-level 1
                          (log-info "Got connection from ~v" them)
                          (field [done? #f])
                          (stop-when (rising-edge (done?)))
                          (assert (advertise (tcp-channel us them _)) #:meta-level 1)
                          (on (message (tcp-channel them us _) #:meta-level 1)) ;; ignore input

                          (on-start (send! 'bump))
                          (on (message `(counter ,$counter))
                              (define response
                                (string->bytes/utf-8
                                 (format (string-append
                                          "HTTP/1.0 200 OK\r\n\r\n"
                                          "<h1>Hello world from syndicate-netstack!</h1>\n"
                                          "<p>This is running on syndicate's own\n"
                                          "<a href='https://github.com/tonyg/syndicate/'>\n"
                                          "TCP/IP stack</a>.</p>\n"
                                          "<p>There have been ~a requests prior to this one.</p>\n")
                                         counter)))
                              (send! (tcp-channel us them response) #:meta-level 1)
                              (done? #t))))))
