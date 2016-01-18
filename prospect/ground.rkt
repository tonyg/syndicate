#lang racket/base
;; Breaking the infinite tower of nested Networks, connecting to the "real world" at the fracture line.

(require racket/async-channel)
(require racket/set)
(require racket/match)
(require racket/list)
(require "core.rkt")
(require "trace.rkt")
(require "trace/stderr.rkt")
(require "tset.rkt")

(provide (struct-out external-event)
         send-ground-message
	 run-ground)

;;---------------------------------------------------------------------------
;; Communication via regular subscription and messages from other threads

;; (Parameterof (Option AsyncChannel))
;; Communication channel from auxiliary (usually driver) threads to
;; the currently-active ground VM.
(define current-ground-event-async-channel (make-parameter (make-async-channel)))

;; Any -> Void
;; Sends a message at the ground-VM metalevel.
(define (send-ground-message body)
  (async-channel-put (current-ground-event-async-channel) (message body)))

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
;; from the ground VM's contained Network.
(define event-projection (compile-projection (observe (external-event (?!) ?))))

;; Interests -> (Listof RacketEvent)
;; Projects out the active event subscriptions from the given interests.
(define (extract-active-events interests)
  (define es (matcher-project/set/single interests event-projection))
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

;; Action* -> Void
;; Runs a ground VM, booting the outermost Network with the given Actions.
(define (run-ground . boot-actions)
  (let await-interrupt ((inert? #f)
                        (w (make-network boot-actions))
                        (interests (matcher-empty)))
    ;; (log-info "GROUND INTERESTS:\n~a" (matcher->pretty-string interests))
    (if (and inert? (matcher-empty? interests))
	(begin (log-info "run-ground: Terminating because inert")
	       (void))
	(let ((e (apply sync
                        (current-ground-event-async-channel)
                        (if inert? never-evt idle-handler)
                        (extract-active-events interests))))
          (trace-process-step e #f network-handle-event w)
          (define resulting-transition (clean-transition (network-handle-event e w)))
          (trace-process-step-result e #f network-handle-event w #f resulting-transition)
	  (match resulting-transition
	    [#f ;; inert
	     (await-interrupt #t w interests)]
	    [(transition w actions)
	     (let process-actions ((actions actions) (interests interests))
	       (match actions
		 ['() (await-interrupt #f w interests)]
		 [(cons a actions)
		  (match a
                    [(? patch? p)
                     (process-actions actions (apply-patch interests (label-patch p (datum-tset 'root))))]
		    [_
		     (log-warning "run-ground: ignoring useless meta-action ~v" a)
		     (process-actions actions interests)])]))])))))
