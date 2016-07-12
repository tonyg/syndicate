#lang syndicate/actor

(require/activate syndicate/drivers/tcp)
(require (only-in racket/string string-trim))

(struct says (who what) #:prefab)
(struct present (who) #:prefab)
(struct shutdown () #:prefab)

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
                    (define input-string (string-trim (bytes->string/utf-8 bs)))
                    (if (equal? input-string "quit-dataspace")
                        (send! (shutdown))
                        (send! (says user input-string)))))))

(dataspace (define us (tcp-listener 5999))
           (until (message (shutdown))
                  (assert (advertise (observe (tcp-channel _ us _))) #:meta-level 1)
                  (on (asserted (advertise (tcp-channel $them us _)) #:meta-level 1)
                      (spawn-session them us))))
