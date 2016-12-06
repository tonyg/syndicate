#lang syndicate/actor

(require/activate syndicate/reload)
(require/activate syndicate/supervise)
(require/activate "trust.rkt")
(require/activate "qa.rkt")

(require "protocol.rkt")
(require "duplicate.rkt")

(struct online () #:prefab)
(struct present (email) #:prefab)

(supervise
 (actor #:name 'reflect-contacts
        (stop-when-reloaded)
        (during (api (session $who _) (online))
          (during (permitted who $grantee (p:follow #;p:see-presence who) _)
            ;; `who` allows `grantee` to follow them
            (assert (api (session grantee _) (present who)))))))

(actor #:name 'contact-list-factory
       (stop-when-reloaded)
       (on (message (create-resource ($ e (contact-list-entry $owner $member))))
           (actor #:name e
                  (on-start (log-info "~s adds ~s to their contact list" owner member))
                  (on-stop (log-info "~s removes ~s from their contact list" owner member))
                  (assert e)
                  (stop-when-duplicate e)
                  (stop-when (message (delete-resource e)))
                  (stop-when (asserted (delete-account owner)))
                  (stop-when (asserted (delete-account member))))))

(supervise
 (actor #:name 'contacts:questions
        (stop-when-reloaded)
        ;; TODO: NOTE: When the `permission-request` vanishes (due to
        ;; satisfaction or rejection), this should remove the question
        ;; from all eligible answerers at once
        (during (permission-request $who $grantee ($ p (p:follow _)))
          (when (equal? who (p:follow-email p))
            ;; `grantee` wants to follow `who`
            (during (permitted who $grantor p #t)
              ;; `grantor` can make that decision
              (on-start
               (define-values (title blurb)
                 (if (equal? who grantor)
                     (values (format "Follow request from ~a" grantee)
                             `(p "User " (b ,grantee) " wants to be able to invite you "
                                 "to conversations and see when you are online."))
                     (values (format "Request from ~a to follow ~a" grantee who)
                             `(p "User " (b ,grantee) " wants to be able to invite "
                                 (b ,who) " to conversations and see when they are online."))))
               (define base-options
                 (list (list "deny" "Reject")
                       (list "ignore" "Ignore")))
               (match (ask-question #:title title #:blurb blurb #:target grantor #:class "q-follow"
                                    (option-question
                                     ;; If who == grantor, then the grantor is directly
                                     ;; the person being followed, and should be offered
                                     ;; the option to follow back, unless they've already
                                     ;; taken that option, which can be deduced if BOTH
                                     ;; the grantee has declared that the grantor may
                                     ;; follow the grantee AND the grantor has declared
                                     ;; that the grantee is a member of their contact
                                     ;; list.
                                     (if (and (equal? who grantor)
                                              (not (and
                                                    (immediate-query [query-value #f (permitted grantee grantor (p:follow grantee) _) #t])
                                                    (immediate-query [query-value #f (contact-list-entry grantor grantee) #t]))))
                                         (list* (list "allow-and-return" "Accept and follow back")
                                                (list "allow" "Accept, but do not follow back")
                                                base-options)
                                         (cons (list "allow" "Accept")
                                               base-options))))
                 ["allow-and-return"
                  (send! (create-resource (grant who grantor grantee p #f)))
                  (send! (create-resource (contact-list-entry grantor grantee)))
                  (send! (create-resource (permission-request grantee grantor (p:follow grantee))))]
                 ["allow" (send! (create-resource (grant who grantor grantee p #f)))]
                 ["deny" (send! (delete-resource (permission-request who grantee p)))]
                 ["ignore" (void)])))))))

