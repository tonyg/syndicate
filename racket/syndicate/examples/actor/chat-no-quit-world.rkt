#lang syndicate

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require (only-in racket/string string-trim))

(struct says (who what) #:prefab)
(struct present (who) #:prefab)

(define us (tcp-listener 5999))
(dataspace (spawn (assert (outbound (advertise (observe (tcp-channel _ us _)))))
                  (during/spawn (inbound (advertise (tcp-channel $them us _)))
                    (assert (outbound (advertise (tcp-channel us them _))))

                    (define (send-to-remote fmt . vs)
                      (send! (outbound (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))

                    (define user (gensym 'user))

                    (on-start (send-to-remote "Welcome, ~a.\n" user))
                    (assert (present user))
                    (on (message (inbound (tcp-channel them us $bs)))
                        (send! (says user (string-trim (bytes->string/utf-8 bs)))))

                    (during (present $who)
                      (unless (equal? who user)
                        (on-start (send-to-remote "~a arrived.\n" who))
                        (on-stop (send-to-remote "~a departed.\n" who))
                        (on (message (says who $what)) (send-to-remote "~a says: ~a\n" who what)))))))
