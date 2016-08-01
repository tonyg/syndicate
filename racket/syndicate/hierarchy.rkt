#lang racket/base
;; Keep track of actor hierarchy, e.g. for
;;  - naming specific actors in traces, or
;;  - injecting events from the outside world to specific locations in the tree

(provide (struct-out targeted-event)
         target-event
         current-actor-path-rev
         current-actor-path
         call/extended-actor-path
         level-anchor
         level-anchor->meta-level)

(require "store.rkt")

;; An event destined for a particular node in the actor hierarchy.
;; Used to inject events from the outside world.
(struct targeted-event (relative-path event) #:prefab)

;; If a non-null path is provided, wraps event in a targeted-event
;; struct.
(define (target-event relative-path event)
  (if (pair? relative-path)
      (targeted-event relative-path event)
      event))

;; Storeof (Listof Any)
;; Path to the active leaf in the process tree. The car end is the
;; leaf; the cdr end, the root.
(define current-actor-path-rev (make-store #:default-box (box '())))

;; Retrieves current-actor-path-rev, but reversed, for use with
;; target-event.
(define (current-actor-path) (reverse (current-actor-path-rev)))

;; Any (-> Any) -> Any
;; Pushes pid on current-actor-path for the duration of the call to thunk.
(define (call/extended-actor-path pid thunk)
  (with-store ((current-actor-path-rev (cons pid (current-actor-path-rev))))
    (thunk)))

;; Retrieves an abstract value to be used with level-anchor->meta-level to compute a
;; relative meta-level number. Concretely, is the actor path to the current actor's
;; dataspace.
;;
;; TODO: Once dataspaces are split into mux and relay, this will need to change to count
;; just relay steps.
(define (level-anchor)
  (if (null? (current-actor-path-rev)) ;; outside even ground
      '()
      (reverse (cdr (current-actor-path-rev)))))

;; Computes the number of nesting levels between the current actor's dataspace and the
;; dataspace path passed in.
(define (level-anchor->meta-level anchor)
  (define ds-path (level-anchor))
  (let loop ((outer anchor) (inner ds-path))
    (cond
      [(null? outer) (length inner)]
      [(and (pair? inner)
            (equal? (car outer) (car inner)))
       (loop (cdr outer) (cdr inner))]
      [else (error 'level-anchor->meta-level
                   "Attempt to access dataspace ~a from non-contained dataspace ~a"
                   anchor
                   ds-path)])))
