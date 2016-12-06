#lang syndicate/actor

(require "protocol.rkt")

(send! (create-resource (account "tonyg@ccs.neu.edu")))
(send! (create-resource (account "me@here")))
(send! (create-resource (account "also@here")))

(define (follow! A B)
  (send! (create-resource (grant A A B (p:follow A) #f)))
  (send! (create-resource (grant B B A (p:follow B) #f))))

(follow! "tonyg@ccs.neu.edu" "me@here")
(follow! "also@here" "me@here")
(follow! "tonyg@ccs.neu.edu" "also@here")

(define (make-conversation! cid title creator . other-members)
  (send! (create-resource (conversation cid title creator "")))
  (for [(who (in-list (cons creator other-members)))]
    (send! (create-resource (in-conversation cid who)))))

(make-conversation! "test" "Test Conversation" "tonyg@ccs.neu.edu" "me@here")
(make-conversation! "grouptest" "Group Conversation" "also@here" "me@here" "tonyg@ccs.neu.edu")
