#lang racket/base
;; Breaking the infinite tower of nested Dataspaces, connecting to the "real world" at the fracture line.

(require racket/async-channel)
(require racket/set)
(require racket/match)
(require racket/list)
(require "core.rkt")
(require "dataspace.rkt")
(require "hierarchy.rkt")
(require "trace.rkt")
(require "trace/stderr.rkt")
(require "tset.rkt")
(require "protocol/standard-relay.rkt")
(require "trie.rkt")

(provide (struct-out external-event)
         current-ground-event-async-channel
         send-ground-message
         send-ground-patch
         send-ground-event
         signal-background-activity!
	 run-ground)

(define-logger syndicate/ground)

;;---------------------------------------------------------------------------
;; Communication via regular subscription and messages from other threads

;; (Parameterof (Option AsyncChannel))
;; Communication channel from auxiliary (usually driver) threads to
;; the currently-active ground VM.
(define current-ground-event-async-channel (make-parameter (make-async-channel)))

;; Any -> Void
;; Sends a message at the ground-VM metalevel.
(define (send-ground-message body #:path [path '()])
  (async-channel-put (current-ground-event-async-channel) (target-event path (message body))))

;; Patch -> Void
;; Injects a patch into the ground-VM metalevel. It will appear to be
;; asserted by the environment in general. The obligation is on the caller
;; to ensure that patches do not interfere between drivers.
(define (send-ground-patch p #:path [path '()])
  (async-channel-put (current-ground-event-async-channel) (target-event path p)))

;; Event -> Void
(define (send-ground-event e #:path [path '()])
  (async-channel-put (current-ground-event-async-channel) (target-event path e)))

;;---------------------------------------------------------------------------

(struct background-activity-signal (delta) #:prefab)

(define (signal-background-activity! active?)
  (async-channel-put (current-ground-event-async-channel)
                     (background-activity-signal (if active? 1 -1))))

;;---------------------------------------------------------------------------
;; Communication via RacketEvents

;; A GroundEvent is a pair of a Racket (evt?) event and its yielded
;; results.
;;  - (external-event RacketEvent (Listof Any))
(struct external-event (descriptor values) #:prefab)

;; RacketEvent -> RacketEvent
;; Wraps a CML-style Racket event with a handler that sends the event
;; results via the ground VM.
(define (event-handler descriptor)
  (handle-evt descriptor (lambda vs (message (external-event descriptor vs)))))

;; Projection
;; Used to extract event descriptors and results from subscriptions
;; from the ground VM's contained Dataspace.
(define event-projection (observe (external-event (?!) ?)))

;; Interests -> (Listof RacketEvent)
;; Projects out the active event subscriptions from the given interests.
(define (extract-active-events interests)
  (define es (trie-project/set/single interests event-projection))
  ;; TODO: how should the following error be handled, ideally?
  ;; In principle, security restrictions should make it impossible.
  ;; But absent those, what should be done? Should an offending
  ;; process be identified and terminated?
  (unless es (error 'extract-active-events "User program subscribed to wildcard event"))
  (for/list [(e (in-set es))] (event-handler e)))

;;---------------------------------------------------------------------------

;; RacketEvent
;; Used only when the system is not provably inert, in order to let it
;; take further internal reductions.
(define idle-handler
  (handle-evt (system-idle-evt) (lambda _ #f)))

;; RacketEvent
;; Used only when the system is locally not inert, but background
;; activity is continuing.
(define poll-handler
  (handle-evt always-evt (lambda _ #f)))

;; Boolean Process AssertionSet Natural -> AssertionSet
;; Returns the final set of active assertions at groundmost level.
(define (await-interrupt inert? proc interests background-activity-count)
  ;; (log-info "~a ~a GROUND INTERESTS:\n~a"
  ;;           inert?
  ;;           background-activity-count
  ;;           (trie->pretty-string interests))
  (define active-events (extract-active-events interests))
  ;; use the presence of subscriptions to indicate whether the program is
  ;; waiting on a driver
  (define ground-subscriptions (trie-step interests observe-parenthesis))
  (if (and inert? (zero? background-activity-count) (trie-empty? ground-subscriptions))
      (begin (log-syndicate/ground-debug "run-ground: Terminating because inert")
             interests)
      (match (apply sync
                    (current-ground-event-async-channel)
                    (cond
                      [inert? never-evt]
                      [(zero? background-activity-count) idle-handler]
                      [else poll-handler])
                    active-events)
        [(background-activity-signal delta)
         ;; (log-info "background-activity-count ~v" (+ background-activity-count delta))
         (await-interrupt inert? proc interests (+ background-activity-count delta))]
        [e
         (inject-event e proc interests background-activity-count)])))

;; Event Process AssertionSet Natural -> AssertionSet
;; Returns the final set of active assertions at groundmost level.
(define (inject-event e proc interests background-activity-count)
  (trace-event-consumed #f e)
  (trace-turn-begin #f proc)
  (define resulting-transition (clean-transition ((process-behavior proc) e (process-state proc))))
  (process-transition resulting-transition proc interests background-activity-count))

;; (Listof Action) AssertionSet -> AssertionSet
;; Incorporates patches into the given assertion set.
(define (process-actions actions interests)
  (match actions
    ['() interests]
    [(cons a actions)
     (match a
       [(? patch? p)
        (process-actions actions (apply-patch interests (label-patch p (datum-tset 'root))))]
       [_
        (log-syndicate/ground-warning "run-ground: ignoring useless meta-action ~v" a)
        (process-actions actions interests)])]))

;; Transition Process AssertionSet Natural -> AssertionSet
;; Returns the final set of active assertions at groundmost level.
(define (process-transition resulting-transition proc interests background-activity-count)
  (match resulting-transition
    [#f ;; inert
     (trace-turn-end #f proc)
     (await-interrupt #t proc interests background-activity-count)]
    [(<quit> exn actions)
     (trace-turn-end #f proc)
     (trace-actor-exit #f exn)
     (log-syndicate/ground-debug "run-ground: Terminating by request")
     (process-actions actions interests)]
    [(transition new-state actions)
     (trace-turn-end #f (process (process-name proc) (process-behavior proc) new-state))
     (let ((proc (update-process-state proc new-state))
           (new-interests (process-actions actions interests)))
       (await-interrupt #f proc new-interests background-activity-count))]))

;; Action* -> AssertionSet
;; Runs a ground VM, booting the outermost Dataspace with the given Actions.
;; Returns the final set of active assertions at groundmost level.
(define (run-ground . boot-actions)
  (run-ground* (dataspace-actor #:name 'ground boot-actions)))

;; actor -> AssertionSet
;; Returns the final set of active assertions at groundmost level.
(define (run-ground* s)
  (define-values (proc t) (actor->process+transition s))
  (process-transition t proc trie-empty 0))
