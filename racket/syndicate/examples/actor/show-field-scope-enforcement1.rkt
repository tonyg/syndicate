#lang syndicate
;; Demonstrates that fields may not be passed between actors.

(spawn #:name 'reading-actor
       (on (message `(read-from ,$this-field))
           (log-info "Trying to read from ~a" this-field)
           (log-info "Read: ~a" (this-field))
           (send! `(read-successfully ,this-field))))

(spawn #:name 'requesting-actor
       (field [a 123])
       (on-start (send! `(read-from ,a)))
       (stop-when (message `(read-successfully ,a)))
       (on-stop (log-info "Done.")))
