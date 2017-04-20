#lang syndicate/test

;; Reflects the current behavior of the little implementation,
;; but quite possibly *not* what should happen

(spawn
 (on-stop (react (assert (outbound "here"))))
 (stop-when (message "stop")))

(spawn (on-start (send! "stop")))

(trace (assertion-added (outbound "here")))