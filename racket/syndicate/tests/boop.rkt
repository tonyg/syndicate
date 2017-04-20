#lang syndicate/actor

(dataspace

 (spawn (on (message "hello")
            (printf "got hello\n")))
 
 (spawn (assert "I am here")
        (on-start (printf "hello\n")
                  (send! "hello"))))

#;(dataspace
 (send! 5))