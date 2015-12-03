#lang prospect

(require (only-in racket/string string-trim))
(require "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(define (spawn-session them us)
  (define user (gensym 'user))
  (define remote-detector (compile-projection (at-meta (?!))))
  (define peer-detector (compile-projection (advertise `(,(?!) says ,?))))
  (define (send-to-remote fmt . vs)
    (message (at-meta (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))
  (define (say who fmt . vs)
    (unless (equal? who user) (send-to-remote "~a ~a\n" who (apply format fmt vs))))
  (list (send-to-remote "Welcome, ~a.\n" user)
	(spawn/stateless
         (lambda (e)
           (match e
             [(message (at-meta (tcp-channel _ _ bs)))
              (define input-string (string-trim (bytes->string/utf-8 bs)))
              (if (equal? input-string "quit-world")
                  (quit-world)
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
          (sub (tcp-channel them us ?) #:meta-level 1) ;; read from remote client
          (sub (advertise (tcp-channel them us ?)) #:meta-level 1) ;; monitor remote client
          (pub (tcp-channel us them ?) #:meta-level 1) ;; we will write to remote client
          ))))

(spawn-tcp-driver)
(spawn-world
 (spawn-demand-matcher (advertise (tcp-channel (?!) (?! (tcp-listener 5999)) ?))
                       (observe (tcp-channel (?!) (?! (tcp-listener 5999)) ?))
		       #:meta-level 1
		       spawn-session))
