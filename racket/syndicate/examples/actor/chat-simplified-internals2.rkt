#lang syndicate

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)
(require (only-in racket/string string-trim))
(require racket/format)

;;---------------------------------------------------------------------------
;; Sketch of a translation from the existing TCP protocol into a simpler one

(struct tcp-connection (id spec) #:prefab)
(struct tcp-accepted (id) #:prefab)
(struct tcp-out (id text) #:prefab)
(struct tcp-in (id text) #:prefab)

(spawn #:name 'translate-tcp-protocol-into-simpler-sketch
 (during/spawn (observe (tcp-connection _ $us))
   (assert (advertise (observe (tcp-channel _ us _))))
   (on (asserted (advertise (tcp-channel $them us _)))
       (define id (seal (list them us)))
       (spawn (stop-when (retracted (advertise (tcp-channel them us _))))
              (stop-when (retracted (tcp-accepted id)))
              (assert (tcp-connection id us))
              (on (message (tcp-channel them us $bs))
                  (send! (tcp-in id (string-trim (bytes->string/utf-8 bs)))))
              (on (message (tcp-out id $text))
                  (send! (tcp-channel us them (string->bytes/utf-8 (~a text "\n")))))))))

;;---------------------------------------------------------------------------
;; The actual chat part

(struct speak (who what) #:prefab)
(struct present (who) #:prefab)

(spawn #:name 'chat-server
 (during/spawn (tcp-connection $id (tcp-listener 5999))
   (assert (tcp-accepted id))
   (let ((me (gensym 'user)))
     (assert (present me))
     (on (message (tcp-in id $line))
         (send! (speak me line))))
   (during (present $user)
     (on-start (send! (tcp-out id (~a user " arrived"))))
     (on-stop  (send! (tcp-out id (~a user " left"))))
     (on (message (speak user $text))
         (send! (tcp-out id (~a user " says '" text "'")))))))
