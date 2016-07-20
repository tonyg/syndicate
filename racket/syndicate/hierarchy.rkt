#lang racket/base
;; Keep track of actor hierarchy, e.g. for
;;  - naming specific actors in traces, or
;;  - injecting events from the outside world to specific locations in the tree

(provide (struct-out targeted-event)
         target-event
         current-actor-path-rev
         current-actor-path
         call/extended-actor-path)

;; An event destined for a particular node in the actor hierarchy.
;; Used to inject events from the outside world.
(struct targeted-event (relative-path event) #:prefab)

;; If a non-null path is provided, wraps event in a targeted-event
;; struct.
(define (target-event relative-path event)
  (if (pair? relative-path)
      (targeted-event relative-path event)
      event))

;; Parameterof (Listof Any)
;; Path to the active leaf in the process tree. The car end is the
;; leaf; the cdr end, the root.
(define current-actor-path-rev (make-parameter '()))

;; Retrieves current-actor-path-rev, but reversed, for use with
;; target-event.
(define (current-actor-path) (reverse (current-actor-path-rev)))

;; Any (-> Any) -> Any
;; Pushes pid on current-actor-path for the duration of the call to thunk.
(define (call/extended-actor-path pid thunk)
  (parameterize ((current-actor-path-rev (cons pid (current-actor-path-rev))))
    (thunk)))
