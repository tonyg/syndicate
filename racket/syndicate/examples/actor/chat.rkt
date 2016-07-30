#lang syndicate/actor

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require (only-in racket/string string-trim))

(struct says (who what) #:prefab)
(struct present (who) #:prefab)
(struct shutdown () #:prefab)

(define (spawn-session them us)
  (actor (define (send-to-remote fmt . vs)
           (send! (outbound (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))

         (define (say who fmt . vs)
           (unless (equal? who user)
             (send-to-remote "~a ~a\n" who (apply format fmt vs))))

         (define user (gensym 'user))
         (send-to-remote "Welcome, ~a.\n" user)

         (until (retracted (inbound (advertise (tcp-channel them us _))))
                (assert (present user))
                (on (asserted (present $who)) (say who "arrived."))
                (on (retracted (present $who)) (say who "departed."))

                (on (message (says $who $what)) (say who "says: ~a" what))

                (assert (outbound (advertise (tcp-channel us them _))))
                (on (message (inbound (tcp-channel them us $bs)))
                    (define input-string (string-trim (bytes->string/utf-8 bs)))
                    (if (equal? input-string "quit-dataspace")
                        (send! (shutdown))
                        (send! (says user input-string)))))))

(dataspace (define us (tcp-listener 5999))
           (until (message (shutdown))
                  (assert (outbound (advertise (observe (tcp-channel _ us _)))))
                  (on (asserted (inbound (advertise (tcp-channel $them us _))))
                      (spawn-session them us))))
