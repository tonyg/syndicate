#lang racket/base

(provide current-trace-procedures
         trace-turn-begin
         trace-turn-end
         trace-actor-spawn
         trace-actor-exit
         trace-action-interpreted
         trace-actions-produced
         trace-event-consumed

         trace-timestamp!
         (struct-out spacetime)
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

;; A Moment is a Natural representing an abstract increasing counter,
;; unique for a Racket VM instance. It names a specific moment in the
;; interpretation of a Syndicate configuration.

;; A SpaceTime is either a (spacetime ActorPath Moment) or #f. When
;; non-#f, it names a specific point in the actor hierarchy ("space")
;; along with a point in time ("time"). When #f, it signifies
;; "unknown".
(struct spacetime (space time) #:prefab)

;; A TraceNotification is a (trace-notification SpaceTime SpaceTime NotificationType TraceDetail).
;; It represents an event in a Syndicate hierarchy.
(struct trace-notification (source sink type detail) #:prefab)
;;
;; A TraceDetail represents information about a specific
;; NotificationType, and so depends on the particular NotificationType
;; being used:
;; -- 'turn-begin and 'turn-end --> Process
;; -- 'spawn --> Process, the new process' initial state
;; -- 'exit --> Option Exception
;; -- 'action-interpreted --> (U Event 'quit) (notably, spawns are handled otherwise)
;; -- 'actions-produced --> (Listof (U Action 'quit)) (spawns included!)
;; -- 'event --> (list SpaceTime Event) ;; point describes action that led to this event,
;;                                      ;; thus capturing the information of the former
;;                                      ;; "causal influence" NotificationType.
;;
;; The source and sink fields both hold values of type SpaceTime. They
;; are, again, used differently for each NotificationType:
;; -- 'turn-begin --> source is dataspace; sink the process whose turn it is
;; -- 'turn-end --> source is dataspace; sink the process whose turn it was
;; -- 'spawn --> source is parent process; sink the new process
;; -- 'exit --> source is dataspace; sink the exiting process
;; -- 'action-interpreted --> source is acting process; sink is dataspace (NB: Flipped!)
;; -- 'actions-produced --> source and sink are both acting process; source = turn-end or spawn
;; -- 'event --> source is dataspace; sink is receiving process

(define current-trace-procedures (make-store #:default-box (box '())))

(define *current-moment* 0)
(define (moment!)
  (local-require ffi/unsafe/atomic)
  (call-as-atomic (lambda ()
                    (begin0 *current-moment*
                      (set! *current-moment* (+ *current-moment* 1))))))

(define (trace-timestamp! actor-path)
  (spacetime actor-path (moment!)))

(define-syntax-rule (notify! SRC SNK TYP DET)
  (let ((trace-procedures (current-trace-procedures)))
    (cond [(pair? trace-procedures)
           (define snk SNK)
           (define n (trace-notification SRC snk TYP DET))
           (for-each (lambda (procedure) (procedure n)) trace-procedures)
           snk]
          [else 'trace-collection-disabled])))

(define (cons-pid pid)
  (if pid
      (cons pid (current-actor-path-rev))
      (current-actor-path-rev)))

(define (trace-turn-begin source pid p)
  (notify! source (trace-timestamp! (cons-pid pid)) 'turn-begin p))

(define (trace-turn-end source pid p)
  (notify! source (trace-timestamp! (cons-pid pid)) 'turn-end p))

(define (trace-actor-spawn source pid p)
  (notify! source (trace-timestamp! (cons-pid pid)) 'spawn p))

(define (trace-actor-exit source pid maybe-exn)
  (notify! source (trace-timestamp! (cons-pid pid)) 'exit maybe-exn))

(define (trace-action-interpreted source _pid e)
  (notify! source (trace-timestamp! (current-actor-path-rev)) 'action-interpreted e))

(define (trace-actions-produced source pid es)
  (notify! source (trace-timestamp! (cons-pid pid)) 'actions-produced es))

(define (trace-event-consumed interpreted-point ;; direct cause
                              produced-point    ;; one-step indirect cause
                              pid               ;; recipient
                              e)
  (notify! interpreted-point (trace-timestamp! (cons-pid pid)) 'event (list produced-point e)))
