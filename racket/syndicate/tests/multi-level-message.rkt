#lang syndicate/test

;; currently failing, seemingly due to a bug in the big implementation; see
;; https://github.com/tonyg/syndicate/issues/20

(dataspace
 (spawn (on (message "hello")
            (printf "got hello\n")))
 
 (spawn (assert 12)
        (on-start (printf "hello\n")
                  (send! (outbound "hello")))))

(trace (message "hello"))