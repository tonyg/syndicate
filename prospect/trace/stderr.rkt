#lang racket/base

(provide set-stderr-trace-flags!)

(require racket/set)
(require racket/match)
(require racket/pretty)
(require (only-in racket/string string-join))
(require "../exn-util.rkt")
(require "../core.rkt")
(require "../trace.rkt")
(require "../mux.rkt")
(require "../endpoint.rkt")
(require "../pretty.rkt")

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
(define show-boot-events? #f)
(define show-events? #f)
(define show-process-states-pre? #f)
(define show-process-states-post? #f)
(define show-process-lifecycle? #f)
(define show-patch-actions? #f)
(define show-message-actions? #f)
(define show-actions? #f)
(define show-routing-table? #f)
(define network-is-boring? #t)

(define (set-stderr-trace-flags! flags-string)
  (set! flags (for/set [(c flags-string)] (string->symbol (string c))))
  (define-syntax-rule (set-flag! symbol variable)
    (set! variable (set-member? flags 'symbol)))
  (set-flag! x show-exceptions?)
  (set-flag! r show-patch-events?)
  (set-flag! m show-message-events?)
  (set-flag! b show-boot-events?)
  (set-flag! e show-events?)
  (set-flag! s show-process-states-pre?)
  (set-flag! t show-process-states-post?)
  (set-flag! p show-process-lifecycle?)
  (set-flag! R show-patch-actions?)
  (set-flag! M show-message-actions?)
  (set-flag! a show-actions?)
  (set-flag! g show-routing-table?)
  (set! network-is-boring? (not (set-member? flags 'N))))

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
    [(cons 'meta rest) (format "context of ~a" (format-pids rest))]
    [_ (string-join (map number->string (reverse pids)) ":")]))

(define (output fmt . args)
  (apply fprintf (current-error-port) fmt args))

(define (boring-state? state)
  (or (and (network? state) network-is-boring?)
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
	[('process-step (list pids e beh st))
	 (define pidstr (format-pids pids))
	 (match e
	   [#f
	    (when show-events?
	      (with-color YELLOW (output "~a is being polled for changes.\n" pidstr)))]
	   [(? patch? p)
	    (when (or show-events? show-patch-events?)
	      (with-color YELLOW
                (output "~a is receiving a patch:\n" pidstr)
                (pretty-print-patch p (current-error-port))))]
	   [(message body)
	    (when (or show-events? show-message-events?)
	      (with-color YELLOW
                (output "~a is receiving a message:\n" pidstr)
                (pretty-write body (current-error-port))))])
	 (when show-process-states-pre?
	   (when (not (boring-state? st))
	     (with-color YELLOW
               (output "~a's state just before the event:\n" pidstr)
               (prospect-pretty-print st (current-error-port)))))]
        [('process-step-result (list pids e beh st exn t))
	 (define pidstr (format-pids pids))
	 (define relevant-exn? (and show-exceptions? exn))
         (define (exn-and-not b) (and relevant-exn? (not b)))
	 (match e
	   [#f
	    (when (exn-and-not show-events?)
	      (with-color YELLOW (output "~a was polled for changes.\n" pidstr)))]
	   ['boot
	    (when (or show-events? show-boot-events?)
	      (with-color YELLOW (output "~a was booted.\n" pidstr)))]
	   [(? patch? p)
	    (when (exn-and-not (or show-events? show-patch-events?))
	      (with-color YELLOW
                (output "~a received a patch:\n" pidstr)
                (pretty-print-patch p (current-error-port))))]
	   [(message body)
	    (when (exn-and-not (or show-events? show-message-events?))
	      (with-color YELLOW
                (output "~a received a message:\n" pidstr)
                (pretty-write body (current-error-port))))])
	 (when (exn-and-not (and show-process-states-pre? (not (boring-state? st))))
           (with-color YELLOW
             (output "~a's state just before the event:\n" pidstr)
             (prospect-pretty-print st (current-error-port))))
	 (when relevant-exn?
	   (with-color WHITE-ON-RED
             (output "Process ~a ~v died with exception:\n~a\n"
                     pidstr
                     beh
                     (exn->string exn))))
         (when (quit? t)
           (with-color BRIGHT-RED
             (output "Process ~a ~v exited normally.\n" pidstr beh)))
	 (when (or relevant-exn? show-process-states-post?)
	   (when (transition? t)
	     (unless (boring-state? (transition-state t))
	       (when (not (equal? st (transition-state t)))
		 (with-color YELLOW
                   (output "~a's state just after the event:\n" pidstr)
                   (prospect-pretty-print (transition-state t) (current-error-port)))))))]
	[('internal-action (list pids a old-w))
         (define pidstr (format-pids pids))
         (define oldcount (hash-count (network-behaviors old-w)))
         (match a
           [(? spawn?)
            ;; Handle this in internal-action-result
            (void)]
           ['quit
            (when (or show-process-lifecycle? show-actions?)
              (define interests (mux-interests-of (network-mux old-w) (car pids)))
              (with-color BRIGHT-RED
                (output "~a exiting (~a total processes remain)\n"
                        pidstr
                        (- oldcount 1)))
              (unless (matcher-empty? interests)
                (output "~a's final interests:\n" pidstr)
                (pretty-print-matcher interests (current-error-port))))]
           [(quit-network)
            (with-color BRIGHT-RED
              (output "Process ~a performed a quit-network.\n" pidstr))]
           [(? patch? p)
            (when (or show-actions? show-patch-actions?)
              (output "~a performing a patch:\n" pidstr)
              (pretty-print-patch p (current-error-port)))]
           [(message body)
            (when (or show-actions? show-message-actions?)
              (output "~a sending a message:\n" pidstr)
              (pretty-write body (current-error-port)))])]
        [('internal-action-result (list pids a old-w t))
         (when (transition? t)
           (define new-w (transition-state t))
           (define pidstr (format-pids pids))
           (define newcount (hash-count (network-behaviors new-w)))
           (match a
             [(? spawn?)
              (when (or show-process-lifecycle? show-actions?)
                (define newpid (mux-next-pid (network-mux old-w)))
                (define newpidstr (format-pids (cons newpid (cdr pids)))) ;; replace parent pid
                (define interests (mux-interests-of (network-mux new-w) newpid))
                (define behavior (hash-ref (network-behaviors new-w) newpid '#:missing-behavior))
                (define state (hash-ref (network-states new-w) newpid '#:missing-state))
                (with-color BRIGHT-GREEN
                  (output "~a ~v spawned from ~a (~a total processes now)\n"
                          newpidstr
                          behavior
                          pidstr
                          newcount))
                (unless (boring-state? state)
                  (output "~a's initial state:\n" newpidstr)
                  (prospect-pretty-print state (current-error-port)))
                (unless (matcher-empty? interests)
                  (output "~a's initial interests:\n" newpidstr)
                  (pretty-print-matcher interests (current-error-port))))]
             [_
              ;; other cases handled in internal-action
              (void)])
           (when show-routing-table?
             (define old-table (mux-routing-table (network-mux old-w)))
             (define new-table (mux-routing-table (network-mux new-w)))
             (when (not (equal? old-table new-table))
               (with-color BRIGHT-BLUE
                 (output "~a's routing table:\n" (format-pids (cdr pids)))
                 (pretty-print-matcher new-table (current-error-port))))))])
      (loop))))

(void (when (not (set-empty? flags))
	(thread display-trace)))
