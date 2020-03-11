#lang syndicate/test

;; currently FAILS

;; dubious behavior by little implementation;
;; create new facets from more nested facets

;; The facet in the on-stop should immediately die and its assertion should never be visible.
;; Pretty sure the little implementation gets that wrong.
;; the trace does not have a way of saying there should never be an "inner" assertion

(spawn (on-start
        (react (on-stop
                (react (assert (outbound "inner"))))))
       (stop-when (message "stop")
                  (react (assert (outbound "outer")))))

(spawn (on-start (send! "stop")))

(trace (message "stop"))
