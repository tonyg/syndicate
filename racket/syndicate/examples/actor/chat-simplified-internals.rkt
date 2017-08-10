#lang syndicate

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require (only-in racket/string string-trim))

(struct tcp-remote-open (id) #:prefab)
(struct tcp-local-open (id) #:prefab)
(struct tcp-incoming-data (id bytes) #:prefab)
(struct tcp-outgoing-data (id bytes) #:prefab)

(define us (tcp-listener 5999))
(spawn (assert (advertise (observe (tcp-channel _ us _))))
       (on (asserted (advertise (tcp-channel $them us _)))
           (define id (seal (list them us)))
           (spawn (stop-when (retracted (advertise (tcp-channel them us _))))
                  (stop-when (retracted (tcp-local-open id)))
                  (assert (tcp-remote-open id))
                  (on (message (tcp-channel them us $bs))
                      (send! (tcp-incoming-data id bs)))
                  (on (message (tcp-outgoing-data id $bs))
                      (send! (tcp-channel us them bs))))))

(struct says (who what) #:prefab)
(struct present (who) #:prefab)

(spawn (during/spawn (tcp-remote-open $id)
         (assert (tcp-local-open id))

         (define (send-to-remote fmt . vs)
           (send! (tcp-outgoing-data id (string->bytes/utf-8 (apply format fmt vs)))))

         (define user (gensym 'user))
         (on-start (send-to-remote "Welcome, ~a.\n" user))
         (assert (present user))
         (on (message (tcp-incoming-data id $bs))
             (send! (says user (string-trim (bytes->string/utf-8 bs)))))

         (during (present $who)
           (unless (equal? who user)
             (on-start (send-to-remote "~a arrived.\n" who))
             (on-stop (send-to-remote "~a departed.\n" who))
             (on (message (says who $what)) (send-to-remote "~a says: ~a\n" who what))))))
