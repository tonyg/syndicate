#lang syndicate/test

(spawn (on (message "ping")
           (printf "ping\n")
           (send! "pong")))

(spawn (on (message "pong")
           (printf "pong\n")
           (send! "ping"))
       (on-start (send! "ping")))

(trace (message "ping")
       (message "pong")
       (message "ping")
       (message "pong")
       (message "ping")
       (message "pong")
       (message "ping")
       (message "pong"))