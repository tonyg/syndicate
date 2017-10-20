#lang syndicate

(require/activate syndicate/drivers/tcp2)
(require/activate syndicate/drivers/timestate)

(define (random-selection items)
  (list-ref items (random (length items))))

(define id (make-parameter #f))

(define (random-sleep secs)
  (define duration (/ (random (* secs 1000)) 1000.0))
  (printf "Bot ~a sleeping for ~a seconds\n" (id) duration)
  (sleep duration))

(define *utterances* (list "Lovely weather we're having!\n"
                           "It's a bit cold today\n"
                           "I've got a lovely bunch of coconuts\n"))

(assertion-struct bot-running (id))

(define (bot)
  (parameterize ((id (gensym 'chatbot)))
    (spawn (assert (tcp-connection (id) (tcp-address "localhost" 5999)))
           (stop-when (retracted (tcp-accepted (id))))

           (on-start (printf "Bot ~a starting\n" (id)))
           (assert (bot-running (id)))
           (on-stop (printf "Bot ~a stopping\n" (id)))

           (on-start (random-sleep 30)
                     (stop-current-facet))

           (on-start (let loop ()
                       (random-sleep 10)
                       (send! (tcp-out (id) (random-selection *utterances*)))
                       (loop))))))

(parameterize ((id 'monitor))
  (spawn (on (retracted (bot-running $id))
             (random-sleep 5)
             (bot))))

(bot)
(bot)
(bot)
