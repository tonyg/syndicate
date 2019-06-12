#lang syndicate

;; Expected Output:
#|
received message bad
realized good
|#

(message-struct ping (v))

(spawn
 (on (realize (ping $v))
     (printf "realized ~a\n" v))
 (on (message (ping $v))
     (printf "received message ~a\n" v)
     (realize! (ping 'good))))

(spawn*
 (send! (ping 'bad)))
