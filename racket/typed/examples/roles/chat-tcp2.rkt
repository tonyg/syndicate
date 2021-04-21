#lang typed/syndicate/roles

(require typed/syndicate/drivers/tcp)

;; message
(define-constructor (speak who what)
  #:type-constructor SpeakT
  #:with Speak (SpeakT Symbol String))

(define-constructor (present who)
  #:type-constructor PresentT
  #:with Present (PresentT Symbol))

(define-type-alias chat-comm
  (U Present
     (Message Speak)
     (Observe (PresentT ★/t))
     (Observe (SpeakT Symbol ★/t))))

(define-type-alias chat-ds
  (U chat-comm
     Tcp2Driver))

(run-ground-dataspace chat-ds
  (activate!)

  (spawn chat-ds
   (start-facet chat-server
     ;; TODO - should be during/spawn
     (during (tcp-connection (bind id Symbol) (tcp-listener 5999))
             (assert (tcp-accepted id))
             (let ([me (gensym 'user)])
               (assert (present me))
               (on (message (tcp-in-line id (bind bs ByteString)))
                   (send! (speak me (bytes->string/utf-8 bs))))
               (during (present (bind user Symbol))
                 (on start
                     (send! (tcp-out id (string->bytes/utf-8 (~a user " arrived\n")))))
                 (on stop
                     (send! (tcp-out id (string->bytes/utf-8 (~a user " left\n")))))
                 (on (message (speak user (bind text String)))
                     (send! (tcp-out id (string->bytes/utf-8 (~a user " says '" text "'\n")))))))))))
