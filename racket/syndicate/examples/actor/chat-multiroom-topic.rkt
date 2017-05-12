#lang syndicate/actor
;; Evolution of chat-multiroom.rkt to support room topics.

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

(struct speak (room who what) #:prefab)
(struct present (room who) #:prefab)
(struct topic (room what) #:prefab)

(spawn #:name 'topic-factory
 (during/spawn (present $room _)
   (field [t #f])
   (assert #:when (t) (topic room (t)))
   (on (message (speak room _ $line))
       (match line
         [(regexp #px"^topic (.*)$" (list _ new-topic)) (t new-topic)]
         [_ (void)]))))

(spawn #:name 'chat-server
 (during/spawn (tcp-connection $id (tcp-listener 5999))
   (assert (tcp-accepted id))
   (on-start
    (for [(room (immediate-query (query-set (present $r _) r)))]
      (send! (tcp-out id (~a "Room: " room))))
    (send! (tcp-out id "Enter a room name."))
    (let-event [(message (tcp-in id $room))]
      (react
       (let ((me (gensym 'user)))
         (assert (present room me))
         (on (message (tcp-in id $line))
             (send! (speak room me line))))
       (on (asserted (topic room $t))
           (send! (tcp-out id (~a "Room topic is: " t))))
       (during (present room $user)
         (on-start (send! (tcp-out id (~a user " arrived"))))
         (on-stop  (send! (tcp-out id (~a user " left"))))
         (on (message (speak room user $text))
             (send! (tcp-out id (~a user " says '" text "'"))))))))))
