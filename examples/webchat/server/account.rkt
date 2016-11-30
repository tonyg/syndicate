#lang syndicate/actor

(require racket/set)

(require/activate syndicate/reload)

(require "protocol.rkt")

(actor #:name 'account-manager
       (stop-when-reloaded)
       (define/query-set accounts (account $e) e)
       (on (asserted (session $email _))
           (when (not (set-member? (accounts) email))
             (spawn-account email))))

(define (spawn-account email)
  (actor #:name (list 'account email)
         (stop-when-reloaded)
         (on-start (log-info "Account ~s created." email))
         (on-stop (log-info "Account ~s deleted." email))
         (assert (account email))
         (assert (issuer email (p:follow email)))
         ;; (assert (issuer email (p:invite email)))
         ;; (assert (issuer email (p:see-presence email)))
         (stop-when (message (delete-account email)))))
