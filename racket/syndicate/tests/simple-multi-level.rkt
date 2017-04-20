#lang syndicate/test

(spawn (on (asserted "hello")
           (printf "goodbye\n")))

(dataspace (spawn (assert (outbound "hello"))))

(trace (assertion-added "hello"))