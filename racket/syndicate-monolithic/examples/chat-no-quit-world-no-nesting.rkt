#lang syndicate-monolithic

(require racket/set)
(require (only-in racket/string string-trim))
(require "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(define (spawn-session them us)
  (define user (gensym 'user))
  (define remote-detector (advertise (?! (tcp-channel ? ? ?))))
  (define peer-detector (advertise `(,(?!) says ,?)))
  (define (send-to-remote fmt . vs)
    (message (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs)))))
  (define (say who fmt . vs)
    (unless (equal? who user) (send-to-remote "~a ~a\n" who (apply format fmt vs))))
  (list (send-to-remote "Welcome, ~a.\n" user)
	(spawn
         (lambda (e peers)
           (match e
             [(message (tcp-channel _ _ bs))
              (transition peers (message `(,user says ,(string-trim (bytes->string/utf-8 bs)))))]
             [(message `(,who says ,what))
              (transition peers (say who "says: ~a" what))]
             [(scn assertions)
              (if (trie-empty? (trie-project assertions remote-detector))
                  (quit (send-to-remote "Goodbye!\n"))
                  (let ((new-peers (trie-project/set/single assertions peer-detector)))
                    (define arrived (set-subtract new-peers peers))
                    (define departed (set-subtract peers new-peers))
                    (transition new-peers
                                (list (for/list [(who arrived)] (say who "arrived."))
                                      (for/list [(who departed)] (say who "departed."))))))]
             [#f #f]))
         (set)
         (scn/union
          (subscription `(,? says ,?)) ;; read actual chat messages
          (subscription (advertise `(,? says ,?))) ;; observe peer presence
          (advertisement `(,user says ,?)) ;; advertise our presence
          (subscription (tcp-channel them us ?)) ;; read from remote client
          (subscription (advertise (tcp-channel them us ?))) ;; monitor remote client
          (advertisement (tcp-channel us them ?)) ;; we will write to remote client
          ))))

(spawn-tcp-driver)
(spawn-demand-matcher (advertise (tcp-channel (?!) (?! (tcp-listener 5999)) ?))
                      (observe (tcp-channel (?!) (?! (tcp-listener 5999)) ?))
                      spawn-session)
