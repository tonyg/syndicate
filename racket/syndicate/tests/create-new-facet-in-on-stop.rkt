#lang syndicate/test

;; The facet in the on-stop should immediately die and its assertion should never be visible.
;; Pretty sure the little implementation gets that wrong.
;; the trace does not have a way of saying there should never be a "here" assertion

(spawn
 (on-stop (react (assert (outbound "here"))))
 (stop-when (message "stop")))

(spawn (on-start (send! "stop")))

(trace (message "stop"))
