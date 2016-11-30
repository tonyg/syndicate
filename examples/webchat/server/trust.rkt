#lang syndicate/actor

(require racket/set)
(require syndicate/protocol/instance)

(require/activate syndicate/reload)
(require "protocol.rkt")
(require "util.rkt")

(actor #:name 'trust-inference
       (stop-when-reloaded)

       (during (issuer $who $permission)
         (assert (permitted who who permission #t)))

       (during (grant $issuer $grantor $grantee $permission $delegable?)
         (during (permitted issuer grantor permission #t)
           (assert (permitted issuer grantee permission delegable?)))))

(define (stop-when-duplicate spec)
  (define id (random-hex-string 16))
  (field [duplicate? #f])
  (stop-when (rising-edge (duplicate?)))
  (assert (instance id spec))
  (on (asserted (instance $id2 spec))
      (when (string<? id id2)
        (log-info "Duplicate instance of ~v detected; terminating" spec)
        (duplicate? #t)))
  id)

(actor #:name 'grant-factory
       (stop-when-reloaded)
       (on (message (create-resource
                     ($ g (grant $issuer $grantor $grantee $permission $delegable?))))
           (actor #:name g
                  (on-start (log-info "~s grants ~s ~v~a"
                                      grantor grantee permission (if delegable? ", delegably" "")))
                  (on-stop (log-info "~s revokes~a grant of ~v to ~s"
                                     grantor (if delegable? " delegable" "") permission grantee))
                  (assert g)
                  (stop-when-duplicate g)
                  (stop-when (message (delete-resource g)))
                  (stop-when (message
                              (delete-resource (permitted issuer grantee permission delegable?))))
                  (stop-when (message (delete-account grantor)))
                  (stop-when (message (delete-account grantee))))))

(actor #:name 'request-factory
       (stop-when-reloaded)
       (on (message (create-resource ($ r (permission-request $the-issuer $grantee $permission))))
           (actor #:name r
                  (on-start (log-info "~s requests ~s from ~s" grantee permission the-issuer))
                  (assert r)
                  (stop-when-duplicate r)
                  (stop-when (message (delete-resource r))
                             (log-info "~s's request of ~s from ~s was cancelled or denied"
                                       grantee permission the-issuer))
                  (stop-when (asserted (issuer grantee permission))
                             (log-info "~s's request of ~s from ~s is axiomatically granted"
                                       grantee permission the-issuer))
                  (stop-when (asserted (grant the-issuer $grantor grantee permission $delegable?))
                             (log-info "~s's request of ~s from ~s was approved~a by ~s"
                                       grantee
                                       permission
                                       the-issuer
                                       (if delegable? ", delegably," "")
                                       grantor)))))
