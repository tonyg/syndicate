#lang syndicate/actor

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require (only-in racket/string string-trim))

(struct says (who what) #:prefab)
(struct present (who) #:prefab)

(define (spawn-session them us)
  (actor (define (send-to-remote fmt . vs)
           (send! (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs)))))

         (define (say who fmt . vs)
           (unless (equal? who user)
             (send-to-remote "~a ~a\n" who (apply format fmt vs))))

         (define user (gensym 'user))
         (on-start (send-to-remote "Welcome, ~a.\n" user))

         (stop-when (retracted (advertise (tcp-channel them us _))))

         (assert (present user))
         (on (asserted (present $who)) (say who "arrived."))
         (on (retracted (present $who)) (say who "departed."))

         (on (message (says $who $what)) (say who "says: ~a" what))

         (assert (advertise (tcp-channel us them _)))
         (on (message (tcp-channel them us $bs))
             (send! (says user (string-trim (bytes->string/utf-8 bs)))))))

(define us (tcp-listener 5999))
(actor (assert (advertise (observe (tcp-channel _ us _))))
       (on (asserted (advertise (tcp-channel $them us _)))
           (spawn-session them us)))
