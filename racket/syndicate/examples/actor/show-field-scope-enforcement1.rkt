#lang syndicate
;; Demonstrates that fields may not be passed between actors.

(require syndicate/actor)

(actor #:name 'reading-actor
       (react
        (on (message `(read-from ,$this-field))
            (log-info "Trying to read from ~a" this-field)
            (log-info "Read: ~a" (this-field))
            (send! `(read-successfully ,this-field)))))

(actor #:name 'requesting-actor
       (field [a 123])
       (send! `(read-from ,a))
       (until (message `(read-successfully ,a)))
       (log-info "Done."))
