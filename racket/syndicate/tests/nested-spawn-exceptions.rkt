#lang syndicate/test

(spawn (on (message "go")
           (spawn (on-start (/ 1 0)))
           (send! "lovely happiness")))

(spawn (on-start (send! "go")))

(trace (message "lovely happiness"))