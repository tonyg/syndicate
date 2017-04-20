#lang syndicate/test

(spawn (on (asserted "hello")
           (react (on (asserted "hello")
                      (printf "do I run?\n")
                      (send! "yes indeed")))))

(spawn (assert "hello"))

(trace (message "yes indeed"))