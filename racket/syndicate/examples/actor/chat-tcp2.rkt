#lang syndicate
;; A version of chat-simplified-internals2.rkt that has the simplified TCP
;; driver split out into syndicate/drivers/tcp2.

(require/activate syndicate/drivers/tcp2)
(require racket/format)

(message-struct speak (who what))
(assertion-struct present (who))

(spawn #:name 'chat-server
 (during/spawn (tcp-connection $id (tcp-listener 5999))
   (assert (tcp-accepted id))
   (let ((me (gensym 'user)))
     (assert (present me))
     (on (message (tcp-in-line id $bs))
         (send! (speak me (bytes->string/utf-8 bs)))))
   (during (present $user)
     (on-start (send! (tcp-out id (string->bytes/utf-8 (~a user " arrived\n")))))
     (on-stop  (send! (tcp-out id (string->bytes/utf-8 (~a user " left\n")))))
     (on (message (speak user $text))
         (send! (tcp-out id (string->bytes/utf-8 (~a user " says '" text "'\n"))))))))
