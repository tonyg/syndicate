#lang racket/base

(provide (all-defined-out)) ;; TODO

;;---------------------------------------------------------------------------
;; Server State

;; (server-baseurl URLString)
(struct server-baseurl (string) #:prefab) ;; ASSERTION

;;---------------------------------------------------------------------------
;; Session and Account Management

;; (session EmailString String)
;; Represents a live session. Retracted when the session ends.
(struct session (email token) #:prefab) ;; ASSERTION

;; (login-link EmailString String)
;; Represents the availability of a non-expired login link. Retracted when the link expires.
(struct login-link (email token) #:prefab) ;; ASSERTION

;; (end-session String)
;; Instructs any matching session to terminate.
(struct end-session (token) #:prefab) ;; MESSAGE

;; (account EmailString)
;; Represents an extant account.
(struct account (email) #:prefab) ;; ASSERTION

;; (delete-account EmailString)
;; Deletes the account named by the given email.
;; As a side effect, should delete all sessions associated with the account.
(struct delete-account (email) #:prefab) ;; MESSAGE

;;---------------------------------------------------------------------------
;; API requests and assertions

;; (api Session Any)
;; Represents some value asserted or transmitted on behalf of the
;; given user session. Values of this type cannot be trusted, since
;; they originate with the user's client, which may be the browser or
;; may be some other client.
(struct api (session value) #:prefab) ;; ASSERTION AND MESSAGE

;;---------------------------------------------------------------------------
;; Create, Update and Delete

;; (create-resource Any)
;; Request creation of the given resource as described.
(struct create-resource (description) #:prefab) ;; MESSAGE

;; (update-resource Any)
;; Request update of the given resource as described.
(struct update-resource (description) #:prefab) ;; MESSAGE

;; (delete-resource Any)
;; Request deletion of the given resource as described.
(struct delete-resource (description) #:prefab) ;; MESSAGE

;;---------------------------------------------------------------------------
;; Capability Management

;; A Principal is an EmailString

;; TODO: Action: report a cap request as spam or some other kind of nuisance

;; (issuer Principal Any)
(struct issuer (email permission) #:prefab) ;; ASSERTION

;; (grant Principal Principal Principal Any Boolean)
;; Links in a grant chain.
(struct grant (issuer grantor grantee permission delegable?) #:prefab) ;; ASSERTION

;; (permitted Principal Principal Any Boolean)
;; Net results of processing grant chains. Query these.
(struct permitted (issuer email permission delegable?) #:prefab) ;; ASSERTION

;; (permission-request Principal Principal Any)
;; Represents an outstanding request for a permission.
;; Satisfied by either - appearance of a matching Grant
;;                     - receipt of a matching Revoke
;;                     - receipt of a CancelRequest
(struct permission-request (issuer grantee permission) #:prefab) ;; ASSERTION

;;---------------------------------------------------------------------------
;; Contact List Management

;; M Capability to invite X to a conversation
;; W Capability to see onlineness of X
;; W Capability to silently block X from contacting one in any way
;; W Capability to visibly block X from contacting one in any way
;; W Capability to mute an individual outside the context of any particular conversation for a certain length of time

(struct p:follow (email) #:prefab)
;; (struct p:invite (email) #:prefab)
;; (struct p:see-presence (email) #:prefab)

;;---------------------------------------------------------------------------
;; Conversation Management

;; M Capability to destroy a conversation
;; M Capability to invite someone inviteable to a conversation
;; M Capability to cancel an open invitation
;; M Capability to boot someone from a conversation
;; M Capability to leave a conversation
;; M Capability to reject an invitation to a conversation
;; M Capability to accept an invitation to a conversation
;; M Capability to see the list of participants in a conversation
;; M Capability to publish posts to a conversation
;; S Capability to remove or edit one's own posts
;; S Capability to remove or edit other people's posts
;; C Capability to clear conversation history
;; C Capability to react to a post on a conversation
;; W Capability to delegate capabilities to others
;; W Capability to mute a conversation for a certain length of time
;; W Capability to mute an individual within the context of a particular conversation for a certain length of time
;; W Capability to have a conversation joinable by ID, without an invitation
;; W Capability to have a conversation be publicly viewable
;; W Capability to draft posts before publication
;; W Capability to approve draft posts

;; TODO: For now, all members will have all conversation control
;; abilities. Later, these can be split out into separate permissions.

;; Attribute: conversation title
;; Attribute: conversation creator
;; Attribute: conversation blurb
;; Attribute: conversation members

;; Simple posting is a combination of draft+approve.
;; Flagging a post for moderator attention is a kind of reaction.

;; (conversation String String Principal Markdown Boolean
(struct conversation (id title creator blurb) #:prefab) ;; ASSERTION

;; (delete-conversation String)
;; TODO: use resource management messages instead
(struct delete-conversation (id) #:prefab) ;; MESSAGE

;; (invitation String Principal)
(struct invitation (conversation-id invitee) #:prefab) ;; ASSERTION

;; (join-conversation String Principal)
;; Used to accept an invitation
;; TODO: use resource management messages instead
(struct join-conversation (id member) #:prefab) ;; MESSAGE

;; (leave-conversation String Principal)
;; Used to - cancel open invitations (by issuer)
;;         - reject open invitations (by invitee)
;;         - eject a member from a conversation (by a moderator)
;;         - leave a conversation (by a member)
;; TODO: use resource management messages instead
(struct leave-conversation (id member) #:prefab) ;; MESSAGE

;; (in-conversation String Principal)
;; Records conversation membership.
(struct in-conversation (conversation-id member) #:prefab) ;; ASSERTION

(struct post (id ;; String
              timestamp ;; Seconds
              conversation-id ;; String
              content-type ;; MimeTypeString
              content ;; Any
              ) #:prefab) ;; ASSERTION

;;---------------------------------------------------------------------------
;; User Interaction

;; (ui-template String String)
;; A fragment of HTML for use in the web client.
(struct ui-template (name data) #:prefab) ;; ASSERTION

;; (question String String Principal String Markdown QuestionType)
(struct question (id class target title blurb type) #:prefab) ;; ASSERTION

;; (answer String Any)
(struct answer (id value) #:prefab) ;; MESSAGE

;; A QuestionType is one of
;; - (yes/no-question Markdown Markdown)
;; - (option-question (Listof (Cons Any Markdown)))
;; - (text-question Boolean)
(struct yes/no-question (false-value true-value) #:prefab)
(struct option-question (options) #:prefab)
(struct text-question (multiline?) #:prefab)
