#lang syndicate/actor

(require/activate syndicate/reload)
(require/activate syndicate/supervise)
(require/activate "trust.rkt")
(require/activate "qa.rkt")

(require "protocol.rkt")
(require "duplicate.rkt")

;; TODO: Move to protocol.rkt
(struct online () #:prefab)
(struct present (email) #:prefab)

(supervise
 (actor #:name 'reflect-presence
        (stop-when-reloaded)
        (during (api (session $who _) (online))
          (during (permitted who $grantee (p:follow who) _)
            ;; `who` allows `grantee` to follow them
            (assert (api (session grantee _) (present who)))))))

(supervise
 (actor #:name 'ensure-p:follow-symmetric
        (stop-when-reloaded)
        (on (asserted (permitted $A $B (p:follow $maybe-A) _))
            (when (equal? A maybe-A)
              (send! (create-resource (permission-request B A (p:follow B))))))
        (on (retracted (permitted $A $B (p:follow $maybe-A) _))
            (when (equal? A maybe-A)
              (send! (delete-resource (permission-request B A (p:follow B))))
              (send! (delete-resource (permitted B A (p:follow B) ?)))))
        (on (retracted (permission-request $A $B (p:follow $maybe-A)))
            (when (equal? A maybe-A)
              (when (not (immediate-query [query-value #f (permitted A B (p:follow A) _) #t]))
                (send! (delete-resource (permitted B A (p:follow B) ?))))))))

(supervise
 (actor #:name 'contact-list-factory
        (stop-when-reloaded)
        (during (permission-request $A $B (p:follow $maybe-A))
          (when (equal? A maybe-A)
            (assert (contact-list-entry B A))))
        (during (permitted $A $B (p:follow $maybe-A) _)
          (when (equal? A maybe-A)
            (when (string<? A B)
              (during (permitted B A (p:follow B) _)
                (assert (contact-list-entry A B))
                (assert (contact-list-entry B A))))))))

(supervise
 (actor #:name 'contact-list-change-log
        (stop-when-reloaded)
        (on (asserted (contact-list-entry $owner $member))
            (log-info "~s adds ~s to their contact list" owner member))
        (on (retracted (contact-list-entry $owner $member))
            (log-info "~s removes ~s from their contact list" owner member))))

(supervise
 (actor #:name 'contacts:questions
        (stop-when-reloaded)
        ;; TODO: CHECK THE FOLLOWING: When the `permission-request` vanishes (due to
        ;; satisfaction or rejection), this should remove the question from all eligible
        ;; answerers at once
        (during (permission-request $who $grantee ($ p (p:follow _)))
          (when (equal? who (p:follow-email p))
            ;; `grantee` wants to follow `who`
            (during (permitted who $grantor p #t)
              ;; `grantor` can make that decision
              (define-values (title blurb)
                (if (equal? who grantor)
                    (values (format "Contact request from ~a" grantee)
                            `(p "User " (b ,grantee) " wants to be able to invite you "
                                "to conversations and see when you are online."))
                    (values (format "Contact request from ~a to ~a" grantee who)
                            `(p "User " (b ,grantee) " wants to be able to invite "
                                (b ,who) " to conversations and see when they are online."))))
              (define qid
                (ask-question! #:title title #:blurb blurb #:target grantor #:class "q-follow"
                               (option-question (list (list "allow" "Accept")
                                                      (list "deny" "Reject")
                                                      (list "ignore" "Ignore")))))
              (stop-when (asserted (answer qid $v))
                         (match v
                           ["allow" (send! (create-resource (grant who grantor grantee p #f)))]
                           ["deny" (send! (delete-resource (permission-request who grantee p)))]
                           ["ignore" (void)])))))))
