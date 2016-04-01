#lang racket/base

(provide trace-logger

	 trace-pid-stack
	 call-in-trace-context

	 trace-process-step
	 trace-process-step-result
         trace-internal-action
	 trace-internal-action-result)

(require "pretty.rkt")

(define trace-logger (make-logger 'minimart-trace))

;; (Parameterof (Listof PID))
;; Path to the active leaf in the process tree. The car end is the
;; leaf; the cdr end, the root. Used for debugging and tracing purposes.
(define trace-pid-stack (make-parameter '()))

;; PID (-> Any) -> Any
;; Pushes pid on trace-pid-stack for the duration of the call to thunk.
(define (call-in-trace-context pid thunk)
  (parameterize ((trace-pid-stack (cons pid (trace-pid-stack))))
    (thunk)))

(define-syntax-rule (record-trace-event name r)
  (when (log-level? trace-logger 'info)
    (log-message trace-logger 'info name "" r #f)))

(define (cons-pid pid)
  (if pid
      (cons pid (trace-pid-stack))
      (trace-pid-stack)))

;; Event (Option PID) Process -> Void
(define (trace-process-step e pid beh st)
  (record-trace-event 'process-step (list (cons-pid pid) e beh st)))

;; Event (Option PID) Process (Option Exception) (Option Transition) -> Void
(define (trace-process-step-result e pid beh st exn t)
  (when exn
    (log-error "Process ~a died with exception:\n~a"
	       (cons-pid pid)
	       (exn->string exn)))
  (record-trace-event 'process-step-result (list (cons-pid pid) e beh st exn t)))

;; (Option PID) Action Network -> Void
(define (trace-internal-action pid a w)
  (record-trace-event 'internal-action (list (cons-pid pid) a w)))

;; (Option PID) Action Network Transition -> Void
(define (trace-internal-action-result pid a w t)
  (record-trace-event 'internal-action-result (list (cons-pid pid) a w t)))
