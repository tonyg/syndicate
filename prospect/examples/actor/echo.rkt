#lang prospect

(require prospect/actor)
(require prospect/drivers/tcp)

(spawn-tcp-driver)

(define server-id (tcp-listener 5999))
(forever (assert (advertise (observe (tcp-channel _ server-id _))))
         (on (asserted (advertise (tcp-channel $c server-id _)))
             (printf "Accepted connection from ~v\n" c)
             (actor (until (retracted (advertise (tcp-channel c server-id _)))
                           (assert (advertise (tcp-channel server-id c _)))
                           (on (message (tcp-channel c server-id $bs))
                               (send! (tcp-channel server-id c bs))))
                    (printf "Closed connection ~v\n" c))))
