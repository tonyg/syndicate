#lang syndicate/actor

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require (only-in racket/string string-trim))

(struct says (who what) #:prefab)
(struct present (who) #:prefab)
(struct shutdown () #:prefab)

(dataspace (define us (tcp-listener 5999))
           (until (message (shutdown))
             (assert (outbound (advertise (observe (tcp-channel _ us _)))))
             (during/spawn (inbound (advertise (tcp-channel $them us _)))
               (assert (outbound (advertise (tcp-channel us them _))))

               (define (send-to-remote fmt . vs)
                 (send! (outbound (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))

               (define user (gensym 'user))

               (on-start (send-to-remote "Welcome, ~a.\n" user))
               (assert (present user))
               (on (message (inbound (tcp-channel them us $bs)))
                   (define input-string (string-trim (bytes->string/utf-8 bs)))
                   (if (equal? input-string "quit-dataspace")
                       (send! (shutdown))
                       (send! (says user input-string))))

               (during (present $who)
                 (unless (equal? who user)
                   (on-start (send-to-remote "~a arrived.\n" who))
                   (on-stop (send-to-remote "~a departed.\n" who))
                   (on (message (says who $what)) (send-to-remote "~a says: ~a\n" who what)))))))
