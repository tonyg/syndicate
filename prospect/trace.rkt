#lang racket/base

(provide trace-logger

	 trace-pid-stack
	 call-in-trace-context

	 trace-process-step
	 trace-process-step-result
         trace-internal-action
	 trace-internal-action-result

         exn->string ;; required from web-server/private/util
         string-indent
         indented-port-output)

(require (only-in web-server/private/util exn->string))
(require (only-in racket/string string-join string-split))

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

;; Event PID Process -> Void
(define (trace-process-step e pid beh st)
  (record-trace-event 'process-step (list (cons pid (trace-pid-stack)) e beh st)))

;; Event PID Process (Option Exception) (Option Transition) -> Void
(define (trace-process-step-result e pid beh st exn t)
  (when exn
    (log-error "Process ~a died with exception:\n~a"
	       (cons pid (trace-pid-stack))
	       (exn->string exn)))
  (record-trace-event 'process-step-result (list (cons pid (trace-pid-stack)) e beh st exn t)))

;; PID Action World -> Void
(define (trace-internal-action pid a w)
  (record-trace-event 'internal-action (list (cons pid (trace-pid-stack)) a w)))

;; PID Action World Transition -> Void
(define (trace-internal-action-result pid a w t)
  (record-trace-event 'internal-action-result (list (cons pid (trace-pid-stack)) a w t)))

(define (string-indent amount s)
  (define pad (make-string amount #\space))
  (string-join (for/list [(line (string-split s "\n"))] (string-append pad line)) "\n"))

(define (indented-port-output amount f)
  (define p (open-output-string))
  (f p)
  (string-indent amount (get-output-string p)))
