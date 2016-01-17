#lang prospect
;; The chat server, using a proxy abstracting over details of the TCP
;; driver's protocol.

(require (only-in racket/string string-trim))
(require "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(struct tcp-remote-open (id) #:prefab)
(struct tcp-local-open (id) #:prefab)
(struct tcp-incoming-data (id bytes) #:prefab)
(struct tcp-outgoing-data (id bytes) #:prefab)

(struct says (who what) #:prefab)
(struct present (who) #:prefab)

(define (tcp-proxy-process them us)
  (define id (seal (list them us)))
  (spawn (lambda (e s)
           (match e
             [(message (tcp-channel _ _ bs))
              (transition s (message (tcp-incoming-data id bs)))]
             [(message (tcp-outgoing-data _ bs))
              (transition s (message (tcp-channel us them bs)))]
             [(? patch/removed?)
              (quit)]
             [_ #f]))
         (void)
         (patch-seq (sub (tcp-channel them us ?))
                    (sub (advertise (tcp-channel them us ?)))
                    (pub (tcp-channel us them ?))
                    (sub (tcp-outgoing-data id ?))
                    (assert (tcp-remote-open id))
                    (sub (tcp-local-open id)))))

(define (spawn-session id)
  (define user (gensym 'user))
  (define (send-to-remote fmt . vs)
    (message (tcp-outgoing-data id (string->bytes/utf-8 (apply format fmt vs)))))
  (define (say who fmt . vs)
    (unless (equal? who user) (send-to-remote "~a ~a\n" who (apply format fmt vs))))
  (list (send-to-remote "Welcome, ~a.\n" user)
	(spawn/stateless
         (lambda (e)
           (match e
             [(message (tcp-incoming-data _ bs))
              (message (says user (string-trim (bytes->string/utf-8 bs))))]
             [(message (says who what))
              (say who "says: ~a" what)]
             [(? patch? p)
              (if (patch/removed? (patch-project p (compile-projection (tcp-remote-open id))))
                  (quit)
                  (let-values (((arrived departed)
                                (patch-project/set/single p (compile-projection (present (?!))))))
                    (list (for/list [(who arrived)] (say who "arrived."))
                          (for/list [(who departed)] (say who "departed.")))))]
             [#f #f]))
         (patch-seq
          (sub (says ? ?)) ;; read actual chat messages
          (sub (present ?)) ;; observe peer presence
          (assert (present user)) ;; advertise our presence
          (sub (tcp-incoming-data id ?)) ;; read from remote client
          (sub (tcp-remote-open id)) ;; monitor remote client
          (assert (tcp-local-open id)) ;; indicate our end of the connection is up
          ))))

(spawn-tcp-driver)

(spawn-demand-matcher (advertise (tcp-channel (?!) (?! (tcp-listener 5999)) ?))
                      (observe (tcp-channel (?!) (?! (tcp-listener 5999)) ?))
                      tcp-proxy-process)

(spawn (lambda (e s)
         (if (patch? e)
             (transition s
                         (for/list [(id (matcher-project/set/single
                                         (patch-added e)
                                         (compile-projection (tcp-remote-open (?!)))))]
                           (spawn-session id)))
             #f))
       (void)
       (sub (tcp-remote-open ?)))
