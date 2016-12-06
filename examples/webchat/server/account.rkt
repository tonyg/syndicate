#lang syndicate/actor

(require racket/set)

(require/activate syndicate/reload)

(require "protocol.rkt")

(actor #:name 'account-manager
       (stop-when-reloaded)
       (define/query-set accounts (account $e) e)
       (on (message (create-resource (account $e)))
           (when (not (set-member? (accounts) e))
             (spawn-account e)))
       (on (asserted (session $email _))
           (when (not (set-member? (accounts) email))
             (spawn-account email))))

(define (spawn-account email)
  (actor #:name (list 'account email)
         (stop-when-reloaded)
         (on-start (log-info "Account ~s created." email))
         (on-stop (log-info "Account ~s deleted." email))
         (assert (account email))
         (assert (grant email email email (p:follow email) #t))
         (stop-when (message (delete-account email)))))
