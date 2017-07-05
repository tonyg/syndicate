#lang syndicate/actor
;; Demonstrates a bug: rising-edge of a predicate that starts off true
;; yields a crash.
;;
;; Fixed by commit 1fdd62d: Now both processes print their message and
;; terminate normally, as expected.

(spawn (field [f #t])
       (stop-when (rising-edge (f))
                  (printf "Stopping (via field).\n")))

(spawn (stop-when (rising-edge #t)
                  (printf "Stopping (direct).\n")))
