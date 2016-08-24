#lang syndicate/actor
;; Demonstrates that fields may not be passed between sibling facets.

(actor (on (message `(read-from ,$this-field))
           (log-info "Trying to read from ~a" this-field)
           (log-info "Read: ~a" (this-field))
           (send! `(read-successfully ,this-field)))
       (on-start
        (react
         (field [a 123])
         (on-start
          (send! `(read-from ,a))
          (until (message `(read-successfully ,a)))
          (log-info "Done.")))))
