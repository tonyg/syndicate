#lang syndicate/test

(spawn (on (asserted 5)
           (printf "wat\n")))

(spawn (assert 5))

(trace (assertion-added (observe 5))
       (assertion-added 5))