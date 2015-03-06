#lang racket/base

(provide set-stderr-trace-flags!)

(require racket/set)
(require racket/match)
(require racket/pretty)
(require (only-in racket/string string-join))
(require (only-in web-server/private/util exn->string))
(require "../core.rkt")
(require "../trace.rkt")

(define (env-aref varname default alist)
  (define key (or (getenv varname) default))
  (cond [(assoc key alist) => cadr]
	[else (error 'env-aref
		     "Expected environment variable ~a to contain one of ~v; got ~v"
		     (map car alist)
		     key)]))

(define colored-output? (env-aref "MINIMART_COLOR" "true" '(("true" #t) ("false" #f))))

(define flags (set))
(define show-exceptions? #f)
(define show-patch-events? #f)
(define show-message-events? #f)
(define show-events? #f)
(define show-process-states-pre? #f)
(define show-process-states-post? #f)
(define show-process-lifecycle? #f)
(define show-patch-actions? #f)
(define show-message-actions? #f)
(define show-actions? #f)
(define show-routing-table? #f)

(define (set-stderr-trace-flags! flags-string)
  (set! flags (for/set [(c flags-string)] (string->symbol (string c))))
  (set! show-exceptions? (set-member? flags 'x))
  (set! show-patch-events? (set-member? flags 'r))
  (set! show-message-events? (set-member? flags 'm))
  (set! show-events? (set-member? flags 'e))
  (set! show-process-states-pre? (set-member? flags 's))
  (set! show-process-states-post? (set-member? flags 't))
  (set! show-process-lifecycle? (set-member? flags 'p))
  (set! show-patch-actions? (set-member? flags 'R))
  (set! show-message-actions? (set-member? flags 'M))
  (set! show-actions? (set-member? flags 'a))
  (set! show-routing-table? (set-member? flags 'g)))

(set-stderr-trace-flags! (or (getenv "MINIMART_TRACE") ""))

(define YELLOW-ON-RED ";1;33;41")
(define WHITE-ON-RED ";1;37;41")
(define WHITE-ON-GREEN ";1;37;42")
(define GREY-ON-RED ";37;41")
(define GREY-ON-GREEN ";37;42")
(define RED ";31")
(define BRIGHT-RED ";1;31")
(define GREEN ";32")
(define BRIGHT-GREEN ";1;32")
(define YELLOW ";33")
(define BLUE ";34")
(define BRIGHT-BLUE ";1;34")
(define NORMAL "")

(define (format-pids pids)
  (match pids
    ['() "ground"]
    [(cons 'meta rest) (format "boot action of ~a" (format-pids rest))]
    [_ (string-join (map number->string (reverse pids)) ":")]))

(define (output fmt . args)
  (apply fprintf (current-error-port) fmt args))

(define (output-state state)
  (cond
   [(world? state) (output "#<world>\n")]
   [else (pretty-write state (current-error-port))]))

(define (boring-state? state)
  (or (world? state)
      (void? state)))

(define (set-color! c) (when colored-output? (output "\e[0~am" c)))
(define (reset-color!) (when colored-output? (output "\e[0m")))

(define-syntax-rule (with-color c expr ...)
  (begin (set-color! c)
	 (begin0 (begin expr ...)
	   (reset-color!))))

(define (display-trace)
  (define receiver (make-log-receiver trace-logger 'info))
  (parameterize ((pretty-print-columns 100))
    (let loop ()
      (match-define (vector level message-string data event-name) (sync receiver))
      (match* (event-name data)
	[('process-step (list pids e p exn t))
	 (define pidstr (format-pids pids))
	 (define relevant-exn? (and show-exceptions? exn))
	 (match e
	   [#f
	    (when (or relevant-exn? show-events?)
	      (with-color YELLOW (output "~a was polled for changes.\n" pidstr)))]
	   [(? patch? p)
	    (when (or relevant-exn? show-events? show-patch-events?)
	      (with-color YELLOW
			  (output "~a received a patch:\n" pidstr)
			  (pretty-print-patch p (current-error-port))))]
	   [(message body)
	    (when (or relevant-exn? show-events? show-message-events?)
	      (with-color YELLOW
			  (output "~a received a message:\n" pidstr)
			  (pretty-write body (current-error-port))))])
	 (when (or relevant-exn? show-process-states-pre?)
	   (when (or relevant-exn? (not (boring-state? (process-state p))))
	     (with-color YELLOW
			 (output "~a's state just before the event:\n" pidstr)
			 (output-state (process-state p)))))
	 (when relevant-exn?
	   (with-color WHITE-ON-RED
		       (output "Process ~a died with exception:\n~a\n"
			       pidstr
			       (exn->string exn))))
         (when (quit? t)
           (with-color BRIGHT-RED
             (output "Process ~a exited normally.\n" pidstr)))
	 (when (or relevant-exn? show-process-states-post?)
	   (when (transition? t)
	     (unless (boring-state? (transition-state t))
	       (when (not (equal? (process-state p) (transition-state t)))
		 (with-color YELLOW
			     (output "~a's state just after the event:\n" pidstr)
			     (output-state (transition-state t)))))))]
	[('internal-step (list pids a old-w t))
	 (when t ;; inert worlds don't change interestingly
	   (define pidstr (format-pids pids))
	   (define new-w (if (transition? t) (transition-state t) old-w))
	   (define old-processes (world-process-table old-w))
	   (define new-processes (world-process-table new-w))
	   (define newcount (hash-count new-processes))
	   (match a
	     [(? spawn?)
	      (when (or show-process-lifecycle? show-actions?)
		(define newpid (set-first (set-subtract (hash-keys new-processes)
							(hash-keys old-processes))))
		(define newpidstr (format-pids (cons newpid (cdr pids)))) ;; replace parent pid
                (match-define (process interests behavior state) (hash-ref new-processes newpid))
		(with-color BRIGHT-GREEN
			    (output "~a ~v spawned from ~a (~a total processes now)\n"
				    newpidstr
				    behavior
				    pidstr
				    newcount))
		(unless (boring-state? state)
		  (output "~a's initial state:\n" newpidstr)
		  (output-state state))
		(unless (matcher-empty? interests)
		  (output "~a's initial interests:\n" newpidstr)
		  (pretty-print-matcher interests (current-error-port))))]
	     ['quit
	      (when (or show-process-lifecycle? show-actions?)
		(match (hash-ref old-processes (car pids) (lambda () #f))
		  [#f (void)]
		  [(process interests behavior state)
		   (with-color BRIGHT-RED
			       (output "~a ~v exited (~a total processes now)\n"
				       pidstr
                                       behavior
				       newcount))
		   (unless (boring-state? state)
		     (output "~a's final state:\n" pidstr)
		     (output-state state))
		   (unless (matcher-empty? interests)
		     (output "~a's final interests:\n" pidstr)
		     (pretty-print-matcher interests (current-error-port)))]))]
	     [(? patch? p)
	      (when (or show-actions? show-patch-actions?)
		(output "~a performed a patch:\n" pidstr)
		(pretty-print-patch p (current-error-port)))]
	     [(message body)
	      (when (or show-actions? show-message-actions?)
		(output "~a sent a message:\n" pidstr)
		(pretty-write body (current-error-port)))])
	   (when show-routing-table?
	     (when (not (equal? (world-routing-table old-w) (world-routing-table new-w)))
	       (with-color BRIGHT-BLUE
			   (output "~a's routing table:\n" (format-pids (cdr pids)))
			   (pretty-print-matcher (world-routing-table new-w)
						 (current-error-port))))))])
      (loop))))

(void (when (not (set-empty? flags))
	(thread display-trace)))
