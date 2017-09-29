#lang syndicate

(require/activate syndicate/drivers/udp)
(require syndicate/protocol/advertise)

(spawn (on (message (udp-packet $peer (udp-listener 5999) $body))
           (printf "Received from ~a: ~v\n" peer body)
           (send! (udp-packet (udp-listener 5999) peer body)))
       (on (asserted (advertise (udp-packet _ (udp-listener 5999) _)))
           (printf "Socket is ready and will forward datagrams.\n")))
