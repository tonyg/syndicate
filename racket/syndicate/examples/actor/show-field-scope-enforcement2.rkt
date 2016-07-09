#lang syndicate
;; Demonstrates that fields may used in a child facet of a declaring
;; facet, but not the other way around.

(require syndicate/actor)

(actor #:name 'reading-actor
       (react
        (field [top 123])
        (on (message `(read-from ,$this-field))
            (log-info "Trying to read from ~a" this-field)
            (log-info "Read: ~a" (this-field))
            (send! `(read-successfully ,this-field)))
        (on-start
         (react (field [inner 234])
                (on-start
                 (log-info "Inner access to ~a: ~a" top (top)) ;; OK
                 (log-info "Inner access to ~a: ~a" inner (inner)) ;; OK
                 (send! `(read-from ,top)) ;; OK
                 (until (message `(read-successfully ,top)))
                 (send! `(read-from ,inner)) ;; Will cause a failure.
                 (until (message `(read-successfully ,inner))) ;; Will never happen.
                 (log-info "Done."))))))
