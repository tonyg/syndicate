#lang prospect

(require prospect/actor)

(struct echo-req (body) #:prefab)
(struct echo-resp (body) #:prefab)

(actor (forever #:collect [(count 0)]
                (on (message (echo-req $body))
                    (send! (echo-resp body))
                    (+ count 1))))

(actor (forever (on (message (echo-resp $body))
                    (printf "Received: ~v\n" body))))

(actor (until (asserted (observe (echo-req _))))
       (until (asserted (observe (echo-resp _))))
       (send! (echo-req 0))
       (send! (echo-req 1))
       (send! (echo-req 2)))
