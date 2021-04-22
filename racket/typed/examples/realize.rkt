#lang typed/syndicate

;; Expected Output:
#|
received message bad
realized good
|#

(message-struct ping : Ping (v))

(define-type-alias τc
  (U (Message (Ping Symbol))
     (Observe ★/t)))

(run-ground-dataspace τc
(spawn
 (start-facet _
  (on (realize (ping $v:Symbol))
      (printf "realized ~a\n" v))
  (on (message (ping $v))
      (printf "received message ~a\n" v)
      (realize! (ping 'good)))))

(spawn
 (start-facet _
  (on start (send! (ping 'bad)))))
)
