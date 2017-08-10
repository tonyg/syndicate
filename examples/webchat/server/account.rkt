#lang syndicate

(require racket/set)

(require/activate syndicate/reload)
(require/activate syndicate/supervise)

(require "protocol.rkt")
(require "duplicate.rkt")

(spawn #:name 'account-manager
       (stop-when-reloaded)
       (define/query-set accounts (account $e) e)
       (on (asserted (session $email _))
           (when (not (set-member? (accounts) email))
             (send! (create-resource (account email))))))

(spawn #:name 'account-factory
       (stop-when-reloaded)
       (on (message (create-resource ($ a (account $email))))
           (spawn #:name (list 'account email)
                  (on-start (log-info "Account ~s created." email))
                  (on-stop (log-info "Account ~s deleted." email))
                  (assert a)
                  (assert (grant email email email (p:follow email) #t))
                  (stop-when-duplicate a)
                  (stop-when (message (delete-resource a))))))
