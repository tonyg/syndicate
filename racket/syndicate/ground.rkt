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

(provide (struct-out external-event)
         send-ground-message
         send-ground-patch
         send-ground-event
         signal-background-activity!
	 run-ground)

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

;; Boolean Behavior State AssertionSet Natural -> Void
(define (await-interrupt inert? beh st interests background-activity-count)
  ;; (log-info "~a ~a GROUND INTERESTS:\n~a"
  ;;           inert?
  ;;           background-activity-count
  ;;           (trie->pretty-string interests))
  (if (and inert? (zero? background-activity-count) (trie-empty? interests))
      (begin (log-info "run-ground: Terminating because inert")
             (void))
      (match (apply sync
                    (current-ground-event-async-channel)
                    (cond
                      [inert? never-evt]
                      [(zero? background-activity-count) idle-handler]
                      [else poll-handler])
                    (extract-active-events interests))
        [(background-activity-signal delta)
         ;; (log-info "background-activity-count ~v" (+ background-activity-count delta))
         (await-interrupt inert? beh st interests (+ background-activity-count delta))]
        [e
         (inject-event e beh st interests background-activity-count)])))

;; Event Behavior State AssertionSet Natural -> Void
(define (inject-event e beh st interests background-activity-count)
  (trace-process-step e #f beh st)
  (define resulting-transition (clean-transition (beh e st)))
  (trace-process-step-result e #f beh st #f resulting-transition)
  (process-transition resulting-transition beh st interests background-activity-count))

;; Transition Behavior State AssertionSet Natural -> Void
(define (process-transition resulting-transition beh st interests background-activity-count)
  (match resulting-transition
    [#f ;; inert
     (await-interrupt #t beh st interests background-activity-count)]
    [(<quit> _ _)
     (log-info "run-ground: Terminating by request")
     (void)]
    [(transition st actions)
     (let process-actions ((actions actions) (interests interests))
       (match actions
         ['() (await-interrupt #f beh st interests background-activity-count)]
         [(cons a actions)
          (match a
            [(? patch? p)
             (process-actions actions (apply-patch interests (label-patch p (datum-tset 'root))))]
            [_
             (log-warning "run-ground: ignoring useless meta-action ~v" a)
             (process-actions actions interests)])]))]))

;; Action* -> Void
;; Runs a ground VM, booting the outermost Dataspace with the given Actions.
(define (run-ground . boot-actions)
  (run-ground* (spawn-dataspace #:name 'ground boot-actions)))

;; Spawn -> Void
(define (run-ground* s)
  (match-define (list beh t _name) ((spawn-boot s)))
  (process-transition t beh 'undefined-initial-ground-state trie-empty 0))
