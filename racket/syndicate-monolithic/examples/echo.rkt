#lang syndicate-monolithic

(require "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(define server-id (tcp-listener 5999))

(spawn-tcp-driver)
(spawn-demand-matcher (advertise (tcp-channel (?!) server-id ?))
                      (observe (tcp-channel (?!) server-id ?))
                      (lambda (c)
                        (printf "Accepted connection from ~v\n" c)
                        (spawn (lambda (e state)
                                 (match e
                                   [(scn (? trie-empty?))
                                    (printf "Closed connection ~v\n" c)
                                    (quit)]
                                   [(message (tcp-channel src dst bs))
                                    (transition state (message (tcp-channel dst src bs)))]
                                   [_ #f]))
                               (void)
                               (scn/union (subscription (advertise (tcp-channel c server-id ?)))
                                          (subscription (tcp-channel c server-id ?))
                                          (advertisement (tcp-channel server-id c ?))))))
