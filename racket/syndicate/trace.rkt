#lang racket/base

(provide trace-logger
	 trace-process-step
	 trace-process-step-result
         trace-internal-action
	 trace-internal-action-result)

(require "hierarchy.rkt")
(require "pretty.rkt")

(define trace-logger (make-logger 'minimart-trace))

(define-syntax-rule (record-trace-event name r)
  (when (log-level? trace-logger 'info)
    (log-message trace-logger 'info name "" r #f)))

(define (cons-pid pid)
  (if pid
      (cons pid (current-actor-path-rev))
      (current-actor-path-rev)))

;; Event (Option PID) Process -> Void
(define (trace-process-step e pid beh st)
  (record-trace-event 'process-step (list (cons-pid pid) e beh st)))

;; Event (Option PID) Process (Option Exception) (Option Transition) -> Void
(define (trace-process-step-result e pid beh st exn t)
  (record-trace-event 'process-step-result (list (cons-pid pid) e beh st exn t)))

;; (Option PID) Action Dataspace -> Void
(define (trace-internal-action pid a w)
  (record-trace-event 'internal-action (list (cons-pid pid) a w)))

;; (Option PID) Action Dataspace Transition -> Void
(define (trace-internal-action-result pid a w t)
  (record-trace-event 'internal-action-result (list (cons-pid pid) a w t)))
