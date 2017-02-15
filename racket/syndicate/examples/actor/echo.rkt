#lang syndicate/actor

(require syndicate/protocol/advertise)
(require/activate syndicate/drivers/tcp)

(define server-id (tcp-listener 5999))
(spawn (assert (advertise (observe (tcp-channel _ server-id _))))
       (during/spawn (advertise (tcp-channel $c server-id _))
                     (on-start (printf "Accepted connection from ~v\n" c))
                     (assert (advertise (tcp-channel server-id c _)))
                     (on (message (tcp-channel c server-id $bs))
                         (send! (tcp-channel server-id c bs)))
                     (on-stop (printf "Closed connection ~v\n" c))))
