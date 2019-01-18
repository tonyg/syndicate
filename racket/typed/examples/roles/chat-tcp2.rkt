#lang typed/syndicate/roles

(require "../../drivers/tcp.rkt")

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

;; without this line, there's no check on what type of actors `activate!`
;; spawns. However, putting these actors in a dataspace interferes with the
;; ground activity performed by the tcp driver (by making all activity one level
;; removed)
;; (dataspace chat-ds
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
                     (send! (tcp-out id (string->bytes/utf-8 (~a user " says '" text "'\n"))))))))))
