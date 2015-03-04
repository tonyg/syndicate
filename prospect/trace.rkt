#lang racket/base

(provide trace-logger

	 trace-pid-stack
	 call-in-trace-context

	 trace-process-step
	 trace-internal-step

         exn->string) ;; required from web-server/private/util

(require (only-in web-server/private/util exn->string))

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

;; Event PID Process (Option Exception) (Option Transition) -> Void
(define (trace-process-step e pid p exn t)
  (when exn
    (log-error "Process ~a died with exception:\n~a"
	       (cons pid (trace-pid-stack))
	       (exn->string exn)))
  (record-trace-event 'process-step (list (cons pid (trace-pid-stack)) e p exn t)))

;; PID Action World Transition -> Void
(define (trace-internal-step pid a w t)
  (record-trace-event 'internal-step (list (cons pid (trace-pid-stack)) a w t)))
