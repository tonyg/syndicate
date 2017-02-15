#lang syndicate/actor
;; Demonstrates that fields at actor scope are visible to facets.

(spawn* (field [x 123])
        (react
         (on (message `(read-from ,$this-field))
             (log-info "Trying to read from ~a" this-field)
             (log-info "Read: ~a" (this-field))
             (send! `(read-successfully ,this-field))))
        (react
         (on-start
          (log-info "x in second facet: ~v (should be 123)" (x))
          (send! `(read-from ,x))
          (until (message `(read-successfully ,x)))
          (log-info "Done."))))
