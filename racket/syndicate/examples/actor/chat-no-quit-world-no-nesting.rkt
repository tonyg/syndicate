#lang syndicate

(require syndicate/actor)
(require syndicate/drivers/tcp)
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
         (send-to-remote "Welcome, ~a.\n" user)

         (until (retracted (advertise (tcp-channel them us _)))
                (assert (present user))
                (on (asserted (present $who)) (say who "arrived."))
                (on (retracted (present $who)) (say who "departed."))

                (on (message (says $who $what)) (say who "says: ~a" what))

                (assert (advertise (tcp-channel us them _)))
                (on (message (tcp-channel them us $bs))
                    (send! (says user (string-trim (bytes->string/utf-8 bs))))))))

(spawn-tcp-driver)
(define us (tcp-listener 5999))
(actor
 (forever (assert (advertise (observe (tcp-channel _ us _))))
          (on (asserted (advertise (tcp-channel $them us _)))
              (spawn-session them us))))
