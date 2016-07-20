#lang racket/base
;; Keep track of actor hierarchy, e.g. for
;;  - naming specific actors in traces, or
;;  - injecting events from the outside world to specific locations in the tree

(provide current-actor-path-rev
         current-actor-path
         call/extended-actor-path
         gen:actor-hierarchy-node
         actor-hierarchy-node-deliver)

(require racket/generic)

;; Parameterof (Listof Any)
;; Path to the active leaf in the process tree. The car end is the
;; leaf; the cdr end, the root.
(define current-actor-path-rev (make-parameter '()))

;; Retrieves current-actor-path-rev, but reversed, for use with
;; actor-hierarchy-node-deliver.
(define (current-actor-path) (reverse (current-actor-path-rev)))

;; Any (-> Any) -> Any
;; Pushes pid on current-actor-path for the duration of the call to thunk.
(define (call/extended-actor-path pid thunk)
  (parameterize ((current-actor-path-rev (cons pid (current-actor-path-rev))))
    (thunk)))

;; Generic interface for non-leaf nodes in the hierarchy.
(define-generics actor-hierarchy-node
  ;; Deliver an event to a specific node in the hierarchy.
  (actor-hierarchy-node-deliver actor-hierarchy-node
                                relative-path
                                event))
