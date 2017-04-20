#lang syndicate/test

;; test that terminating facets can create new facets (at the parent level)

(spawn (stop-when (message "stop")
                  (react (on (message "poodle")
                             (send! "success")
                             (printf "woohoo\n")))))

(spawn (on-start (send! "stop"))
       (on (asserted (observe "poodle"))
           (send! "poodle")))

(trace (message "success"))
