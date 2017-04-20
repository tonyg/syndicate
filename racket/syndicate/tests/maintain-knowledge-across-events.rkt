#lang syndicate/test

(spawn (on (asserted "outer")
           (react (on (message "bam")
                      (react (on (asserted "outer")
                                 (send! "icu")
                                 (printf "icu\n")))))))

(spawn (assert "outer")
       (on (asserted (observe "bam"))
           (send! "bam")))

(trace (message "icu"))