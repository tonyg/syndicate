#lang syndicate

(require syndicate/protocol/advertise)
(require (only-in racket/string string-trim))
(require/activate "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(define (spawn-session them us)
  (define user (gensym 'user))
  (define remote-detector (inbound (?!)))
  (define peer-detector (advertise `(,(?!) says ,?)))
  (define (send-to-remote fmt . vs)
    (message (outbound (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))
  (define (say who fmt . vs)
    (unless (equal? who user) (send-to-remote "~a ~a\n" who (apply format fmt vs))))
  (list (send-to-remote "Welcome, ~a.\n" user)
	(actor/stateless
         (lambda (e)
           (match e
             [(message (inbound (tcp-channel _ _ bs)))
              (define input-string (string-trim (bytes->string/utf-8 bs)))
              (if (equal? input-string "quit-dataspace")
                  (quit-dataspace)
                  (message `(,user says ,input-string)))]
             [(message `(,who says ,what))
              (say who "says: ~a" what)]
             [(? patch? p)
              (if (patch/removed? (patch-project p remote-detector))
                  (quit (send-to-remote "Goodbye!\n"))
                  (let-values (((arrived departed) (patch-project/set/single p peer-detector)))
                    (list (for/list [(who arrived)] (say who "arrived."))
                          (for/list [(who departed)] (say who "departed.")))))]
             [#f #f]))
         (patch-seq
          (sub `(,? says ,?)) ;; read actual chat messages
          (sub (advertise `(,? says ,?))) ;; observe peer presence
          (pub `(,user says ,?)) ;; advertise our presence
          (sub (inbound (tcp-channel them us ?))) ;; read from remote client
          (sub (inbound (advertise (tcp-channel them us ?)))) ;; monitor remote client
          (pub (inbound (tcp-channel us them ?))) ;; we will write to remote client
          ))))

(spawn-dataspace
 (spawn-demand-matcher (inbound (advertise (tcp-channel (?!) (?! (tcp-listener 5999)) ?)))
                       (inbound (observe (tcp-channel (?!) (?! (tcp-listener 5999)) ?)))
		       spawn-session))
