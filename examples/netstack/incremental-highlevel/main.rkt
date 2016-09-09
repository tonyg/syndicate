#lang syndicate/actor

(require syndicate/protocol/advertise)

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
             (send! (outbound (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))

           (define (say who fmt . vs)
             (unless (equal? who user)
               (send-to-remote "~a ~a\n" who (apply format fmt vs))))

           (define user (gensym 'user))
           (on-start (send-to-remote "Welcome, ~a.\n" user))

           (stop-when (retracted (inbound (advertise (tcp-channel them us _)))))

           (assert (present user))
           (on (asserted (present $who)) (say who "arrived."))
           (on (retracted (present $who)) (say who "departed."))

           (on (message (says $who $what)) (say who "says: ~a" what))

           (assert (outbound (advertise (tcp-channel us them _))))
           (on (message (inbound (tcp-channel them us $bs)))
               (send! (says user (string-trim (bytes->string/utf-8 bs)))))))

  (dataspace #:name 'chat-dataspace
             (define us (tcp-listener 5999))
             (forever (assert (outbound (advertise (observe (tcp-channel _ us _)))))
                      (on (asserted (inbound (advertise (tcp-channel $them us _))))
                          (spawn-session them us)))))

(let ((dst (udp-listener 6667)))
  (actor #:name 'udp-echo-program
         (on (message (udp-packet $src dst $body))
             (log-info "Got packet from ~v: ~v" src body)
             (send! (udp-packet dst src (string->bytes/utf-8 (format "You said: ~a" body)))))))

(let ()
  (dataspace #:name 'webserver-dataspace
   (actor #:name 'webserver-counter
          (field [counter 0])
          (on (message 'bump)
              (send! `(counter ,(counter)))
              (counter (+ (counter) 1))))

   (forever (define us (tcp-listener 80))
            (assert (outbound (advertise (observe (tcp-channel _ us _)))))
            (during/actor (inbound (advertise (tcp-channel ($ them (tcp-address _ _)) us _)))
                          #:name (list 'webserver-session them)
                          (log-info "Got connection from ~v" them)
                          (field [done? #f])
                          (stop-when (rising-edge (done?)))
                          (assert (outbound (advertise (tcp-channel us them _))))
                          (on (message (inbound (tcp-channel them us _)))) ;; ignore input

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
                              (send! (outbound (tcp-channel us them response)))
                              (done? #t))))))
