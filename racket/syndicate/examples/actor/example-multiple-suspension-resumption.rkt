#lang syndicate/actor
;; Shows that the checks enforcing single-use suspension-resumption are working.

(spawn #:name 'shouldnt-work
       (field [k #f])
       (on-start
        (flush!)
        (log-info "Result from suspension: ~v"
                  (react/suspend (actual-k)
                                 (on-start (k actual-k)
                                           ((k) 'first-result))))
        (flush!)
        ((k) 'second-result)))
