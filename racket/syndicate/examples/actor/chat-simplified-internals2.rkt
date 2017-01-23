#lang syndicate/actor

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require (only-in racket/string string-trim))
(require racket/format)

;;---------------------------------------------------------------------------
;; Sketch of a translation from the existing TCP protocol into a simpler one

(struct tcp-connection (id spec) #:prefab)
(struct tcp-out (id text) #:prefab)
(struct tcp-in (id text) #:prefab)

(actor #:name 'translate-tcp-protocol-into-simpler-sketch
 (during/actor (observe (tcp-connection _ $us))
   (assert (advertise (observe (tcp-channel _ us _))))
   (on (asserted (advertise (tcp-channel $them us _)))
       (define id (seal (list them us)))
       (actor (stop-when (retracted (advertise (tcp-channel them us _))))
              (stop-when (retracted (observe (tcp-connection id us))))
              (assert (tcp-connection id us))
              (on (message (tcp-channel them us $bs))
                  (send! (tcp-in id (string-trim (bytes->string/utf-8 bs)))))
              (on (message (tcp-out id $text))
                  (send! (tcp-channel us them (string->bytes/utf-8 (~a text "\n")))))))))

;;---------------------------------------------------------------------------
;; The actual chat part

(struct speak (who what) #:prefab)
(struct present (who) #:prefab)

(actor #:name 'chat-server
 (during/actor (tcp-connection $id (tcp-listener 5999))
   (define me (gensym 'user))  ;; a random user name
   (assert (present me))
   (during (present $user)
     (on-start (send! (tcp-out id (~a user " arrived"))))
     (on-stop  (send! (tcp-out id (~a user " left")))))
   (on (message (speak $user $text))
       (send! (tcp-out id (~a user " says '" text "'"))))
   (on (message (tcp-in id $line))
       (send! (speak me line)))))

