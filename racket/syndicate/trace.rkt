#lang racket/base

(provide current-trace-procedures
         trace-turn-begin
         trace-turn-end
         trace-actor-spawn
         trace-actor-exit
         trace-action-interpreted
         trace-actions-produced
         trace-event-consumed
         trace-causal-influence

         (struct-out trace-notification))

(require "hierarchy.rkt")
(require "pretty.rkt")
(require "store.rkt")

;; A NotificationType is one of
;; -- 'turn-begin
;; -- 'turn-end
;; -- 'spawn
;; -- 'exit
;; -- 'action-interpreted
;; -- 'actions-produced
;; -- 'event
;; -- 'influence
;;
;; The trace-notification-detail field is used differently for each
;; NotificationType:
;; -- 'turn-begin and 'turn-end --> Process
;; -- 'spawn --> (list PID Process), the parent's PID and the process' initial state
;; -- 'exit --> Option Exception
;; -- 'action-interpreted --> (U Event 'quit) (notably, spawns are handled otherwise)
;; -- 'actions-produced --> (Listof (U Action 'quit)) (spawns included!)
;; -- 'event --> Event
;; -- 'influence --> Event
;;
;; The source and sink fields both hold values of type ActorPath. They
;; are, again, used differently for each NotificationType:
;; -- 'turn-begin --> source is dataspace; sink the process whose turn it is
;; -- 'turn-end --> source is dataspace; sink the process whose turn it was
;; -- 'spawn --> source is dataspace; sink the new process
;; -- 'exit --> source is dataspace; sink the exiting process
;; -- 'action-interpreted --> source is acting process; sink is dataspace (NB: Flipped!)
;; -- 'actions-produced --> source is acting process; sink is dataspace (NB: Flipped!)
;; -- 'event --> source is dataspace; sink is receiving process
;; -- 'influence --> source is acting process; sink is receiving process
;;
;; For 'influence, when the detail event is a patch, the source field
;; is not always the true influencing party. In the case where a
;; process adds new observe assertions to a dataspace where matching
;; assertions already exist, it will appear to "influence itself".
;; Really, with patches, it's the PIDs at the leaves of each patch's
;; tries that are the influencers.

(struct trace-notification (source sink type detail) #:prefab)

(define current-trace-procedures (make-store #:default-box (box '())))

(define-syntax-rule (notify! src snk typ det)
  (let ((trace-procedures (current-trace-procedures)))
    (when (pair? trace-procedures)
      (define n (trace-notification src snk typ det))
      (for-each (lambda (procedure) (procedure n)) trace-procedures))))

(define (cons-pid pid)
  (if pid
      (cons pid (current-actor-path-rev))
      (current-actor-path-rev)))

;; PID Process
(define (trace-turn-begin pid p)
  (notify! (current-actor-path-rev) (cons-pid pid) 'turn-begin p))

;; PID Process
(define (trace-turn-end pid p)
  (notify! (current-actor-path-rev) (cons-pid pid) 'turn-end p))

;; PID PID Process
(define (trace-actor-spawn parent-pid pid p)
  (notify! (current-actor-path-rev) (cons-pid pid) 'spawn (list (cons-pid parent-pid) p)))

;; PID (Option Exception)
(define (trace-actor-exit pid maybe-exn)
  (notify! (current-actor-path-rev) (cons-pid pid) 'exit maybe-exn))

;; PID Event
(define (trace-action-interpreted pid e)
  (notify! (cons-pid pid) (current-actor-path-rev) 'action-interpreted e))

;; PID (Listof Event)
(define (trace-actions-produced pid es)
  (notify! (cons-pid pid) (current-actor-path-rev) 'actions-produced es))

;; PID Event
(define (trace-event-consumed pid e)
  (notify! (current-actor-path-rev) (cons-pid pid) 'event e))

;; PID PID Event
(define (trace-causal-influence src-pid snk-pid e)
  (notify! (cons-pid src-pid) (cons-pid snk-pid) 'influence e))
