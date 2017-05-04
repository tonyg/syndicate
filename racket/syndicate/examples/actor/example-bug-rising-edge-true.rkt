#lang syndicate/actor
;; Demonstrates a bug: rising-edge of a predicate that starts off true
;; yields a crash.

(spawn (field [f #t])
       (stop-when (rising-edge (f))
                  (printf "Stopping (via field).\n")))

(spawn (stop-when (rising-edge #t)
                  (printf "Stopping (direct).\n")))
