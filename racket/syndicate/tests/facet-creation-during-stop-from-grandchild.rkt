#lang syndicate/test

;; currently FAILS

;; dubious behavior by little implementation;
;; create new facets from more nested facets

(spawn (on-start
        (react (on-stop
                (react (assert (outbound "inner"))))))
       (stop-when (message "stop")
                  (react (assert (outbound "outer")))))

(spawn (on-start (send! "stop")))

(trace (assertion-added (outbound "inner")))
