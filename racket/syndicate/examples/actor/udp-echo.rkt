#lang syndicate

(require/activate syndicate/drivers/udp)

(spawn (on (message (udp-packet $peer (udp-listener 5999) $body))
           (printf "Received from ~a: ~v\n" peer body)
           (send! (udp-packet (udp-listener 5999) peer body))))
