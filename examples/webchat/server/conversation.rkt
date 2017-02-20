#lang syndicate/actor

(require racket/port)
(require markdown)

(require/activate syndicate/reload)
(require/activate syndicate/supervise)
(require/activate "trust.rkt")

(require "protocol.rkt")
(require "duplicate.rkt")
(require "util.rkt")

(define (user-in-conversation? who cid)
  (immediate-query [query-value #f (in-conversation cid who) #t]))

(supervise
 (spawn #:name 'take-conversation-instructions
        (stop-when-reloaded)

        (on (message (api (session $creator _) (create-resource (? conversation? $c))))
            (when (equal? creator (conversation-creator c))
              (send! (create-resource c))))
        (on (message (api (session $updater _) (update-resource (? conversation? $c))))
            (when (user-in-conversation? updater (conversation-id c))
              (send! (update-resource c))))
        (on (message (api (session $creator _) (delete-resource (? conversation? $c))))
            (when (equal? creator (conversation-creator c))
              (send! (delete-resource c))))

        (on (message (api (session $joiner _) (create-resource (? in-conversation? $i))))
            (when (equal? joiner (in-conversation-member i))
              (send! (create-resource i))))
        (on (message (api (session $leaver _) (delete-resource (? in-conversation? $i))))
            (when (equal? leaver (in-conversation-member i))
              (send! (delete-resource i))))

        (on (message (api (session $inviter _) (create-resource (? invitation? $i))))
            (when (equal? inviter (invitation-inviter i))
              (send! (create-resource i))))
        (on (message (api (session $who _) (delete-resource (? invitation? $i))))
            (when (or (equal? who (invitation-inviter i))
                      (equal? who (invitation-invitee i)))
              (send! (delete-resource i))))

        (on (message (api (session $who _) (create-resource (? post? $p))))
            (when (and (user-in-conversation? who (post-conversation-id p))
                       (equal? who (post-author p)))
              (send! (create-resource p))))
        (on (message (api (session $who _) (update-resource (? post? $p))))
            (when (equal? who (post-author p))
              (send! (update-resource p))))
        (on (message (api (session $who _) (delete-resource (? post? $p))))
            (when (equal? who (post-author p))
              (send! (delete-resource p))))))

(supervise
 (spawn #:name 'relay-conversation-state
        (stop-when-reloaded)

        (during (invitation $cid $inviter $invitee)
          (assert (api (session invitee _) (invitation cid inviter invitee)))
          (during ($ c (conversation cid _ _ _))
            (assert (api (session invitee _) c))))

        (during (in-conversation $cid $who)
          (during ($ i (invitation cid _ _))
            (assert (api (session who _) i)))
          (during ($ i (in-conversation cid _))
            (assert (api (session who _) i)))
          (during ($ c (conversation cid _ _ _))
            (assert (api (session who _) c)))
          (during ($ p (post _ _ cid _ _))
            (assert (api (session who _) p))))))

(supervise
 (spawn #:name 'conversation-factory
        (stop-when-reloaded)
        (on (message (create-resource ($ c0 (conversation $cid $title0 $creator $blurb0))))
            (spawn #:name c0
                   (field [title title0]
                          [blurb blurb0])
                   (define/dataflow c (conversation cid (title) creator (blurb)))
                   (on-start (log-info "~v created" (c)))
                   (on-stop (log-info "~v deleted" (c)))
                   (assert (c))
                   (stop-when-duplicate (list 'conversation cid))
                   (stop-when (message (delete-resource (conversation cid _ _ _))))
                   (on (message (update-resource (conversation cid $newtitle _ $newblurb)))
                       (title newtitle)
                       (blurb newblurb))))))

(supervise
 (spawn #:name 'in-conversation-factory
        (stop-when-reloaded)
        (on (message (create-resource ($ i (in-conversation $cid $who))))
            (spawn #:name i
                   (on-start (log-info "~s joins conversation ~a" who cid))
                   (on-stop (log-info "~s leaves conversation ~a" who cid))
                   (assert i)
                   (stop-when-duplicate i)
                   (stop-when (message (delete-resource i)))
                   (stop-when (message (delete-resource (conversation cid _ _ _))))))))

(supervise
 (spawn #:name 'invitation-factory
        (stop-when-reloaded)
        (on (message (create-resource ($ i (invitation $cid $inviter $invitee))))
            (spawn #:name i
                   (on-start (log-info "~s invited to conversation ~a by ~s" invitee cid inviter))
                   (on-stop (log-info "invitation of ~s to conversation ~a by ~s retracted"
                                      invitee cid inviter))
                   (assert i)
                   (stop-when-duplicate i)
                   (stop-when (message (delete-resource i)))
                   (stop-when (message (delete-resource (conversation cid _ _ _))))
                   (stop-when (asserted (in-conversation cid invitee)))))))

(supervise
 (spawn #:name 'post-factory
        (stop-when-reloaded)
        (on (message (create-resource
                      ($ p0 (post $pid $timestamp $cid $author $items0))))
            (spawn #:name p0
                   (field [items items0])
                   (define/dataflow p (post pid timestamp cid author (items)))
                   (assert (p))
                   (stop-when-duplicate (list 'post cid pid))
                   (stop-when (message (delete-resource (post pid _ cid _ _))))
                   (stop-when (message (delete-resource (conversation cid _ _ _))))
                   (on (message (update-resource (post pid _ cid _ $newitems)))
                       (items newitems))))))

(supervise
 (spawn #:name 'conversation:questions
        (stop-when-reloaded)
        ;; TODO: CHECK THE FOLLOWING: When the `invitation` vanishes (due to satisfaction
        ;; or rejection), this should remove the question from all eligible answerers at once
        (during (invitation $cid $inviter $invitee)
          ;; `inviter` has invited `invitee` to conversation `cid`...
          (define qid (random-hex-string 32)) ;; Fix qid and timestamp even as title/creator vary
          (define timestamp (current-seconds))
          (during (conversation cid $title $creator _)
            ;; ...and it exists...
            (during (permitted invitee inviter (p:follow invitee) _)
              ;; ...and they are permitted to do so
              (assert (question qid timestamp "q-invitation" invitee
                                (format "Invitation from ~a" inviter)
                                (with-output-to-string
                                  (lambda ()
                                    (display-xexpr
                                     `(div
                                       (p "You have been invited by " (b ,inviter)
                                          " to join a conversation started by " (b ,creator) ".")
                                       (p "The conversation is titled "
                                          (i "\"" ,title "\"") ".")))))
                                (option-question (list (list "join" "Join conversation")
                                                       (list "decline" "Decline invitation")))))
              (stop-when (asserted (answer qid $v))
                         (match v
                           ["join"
                            (send! (create-resource (in-conversation cid invitee)))]
                           ["decline"
                            (send! (delete-resource (invitation cid inviter invitee)))])))))))
