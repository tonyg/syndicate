#lang racket/base

(provide set-stderr-trace-flags!)

(require racket/set)
(require racket/match)
(require racket/pretty)
(require racket/exn)
(require (only-in racket/string string-join string-split))
(require "../core.rkt")
(require "../dataspace.rkt")
(require "../hierarchy.rkt")
(require "../trace.rkt")
(require "../mux.rkt")
(require "../pretty.rkt")
(require "../trie.rkt")
(require "../tset.rkt")

(define (env-aref varname default alist)
  (define key (or (getenv varname) default))
  (cond [(assoc key alist) => cadr]
	[else (error 'env-aref
		     "Expected environment variable ~a to contain one of ~v; got ~v"
		     (map car alist)
		     key)]))

(define colored-output? (env-aref "SYNDICATE_COLOR" "true" '(("true" #t) ("false" #f))))

(define flags (set))
(define show-exceptions? #f)
(define show-turns? #f)
(define show-lifecycle? #f)
(define show-actions? #f)
(define show-events? #f)
(define show-influence? #f)

(define (set-stderr-trace-flags! flags+module-string)
  (define-values (flags-string module-string)
    (match flags+module-string
      [(regexp #px"^([^:]*):(.*)$" (list _ fs m)) (values fs m)]
      [_ (values flags+module-string "")]))

  (define A-flags (set 'x 'i 'p))
  (set! flags (for/set [(c flags-string)] (string->symbol (string c))))
  (define-syntax-rule (set-flag! symbol variable)
    (set! variable (or (and (set-member? flags 'A) (set-member? A-flags 'symbol))
                       (set-member? flags 'symbol))))
  (set-flag! x show-exceptions?)
  (set-flag! t show-turns?)
  (set-flag! p show-lifecycle?)
  (set-flag! a show-actions?)
  (set-flag! e show-events?)
  (set-flag! i show-influence?)

  (let ((port (open-input-string module-string)))
    (let loop ()
      (match (read port)
        [(? eof-object?) (void)]
        [v (begin (dynamic-require v 0)
                  (loop))]))))

(set-stderr-trace-flags! (or (getenv "SYNDICATE_TRACE") ""))

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

(define (format-pids pids [name #f])
  (define pidstr
    (match pids
      ['() "ground"]
      [(cons 'meta rest) (format "context of ~a" (format-pids rest))]
      [_ (string-join (map number->string (reverse pids)) ":")]))
  (if name
      (format "~a a.k.a ~v" pidstr name)
      pidstr))

(define (output fmt . args)
  (apply fprintf (current-error-port) fmt args))

(define (set-color! c) (when colored-output? (output "\e[0~am" c)))
(define (reset-color!) (when colored-output? (output "\e[0m")))

(define-syntax-rule (with-color c expr ...)
  (begin (set-color! c)
	 (begin0 (begin expr ...)
	   (reset-color!))))

(define (extract-leaf-pids sink p)
  (for/list [(pid (in-set (extract-patch-pids p)))]
    (cons pid (cdr sink))))

(define (display-notification the-notification)
  (match-define (trace-notification source sink type detail) the-notification)
  (match* (type detail)
    [('turn-begin (process name _beh state))
     (when show-turns?
       (with-color BLUE
         (output "~a turn begins\n" (format-pids sink name))))]
    [('turn-end (process name _beh state))
     (when show-turns?
       (with-color BLUE
         (output "~a turn ends\n" (format-pids sink name))
         (syndicate-pretty-print state (current-error-port))))]
    [('spawn (list parent (process name _beh state)))
     (when show-lifecycle?
       (with-color BRIGHT-GREEN
         (output "~a spawned by ~a\n" (format-pids sink name) (format-pids parent))))]
    [('exit #f)
     (when show-lifecycle?
       (with-color BRIGHT-RED
         (output "~a schedules an exit\n" (format-pids sink))))]
    [('exit exn)
     (when (or show-lifecycle? show-exceptions?)
       (with-color WHITE-ON-RED
         (output "~a raises an exception:\n~a\n" (format-pids sink) (exn->string exn))))]
    [('action (? patch? p))
     (when show-actions?
       (output "~a performs a patch:\n~a\n" (format-pids source) (patch->pretty-string p)))]
    [('action (message body))
     (when show-actions?
       (output "~a broadcasts a message:\n~a\n" (format-pids source) (pretty-format body)))]
    [('action 'quit)
     (when show-lifecycle?
       (with-color BRIGHT-RED
         (output "~a exits\n" (format-pids source))))]
    [('event (? patch? p))
     (when show-events?
       (with-color YELLOW
         (output "~a receives an event:\n~a\n" (format-pids sink) (patch->pretty-string p))))]
    [('event (message body))
     (when show-events?
       (with-color YELLOW
         (output "~a receives a message:\n~a\n" (format-pids sink) (pretty-format body))))]
    [('event #f)
     (when show-events?
       (with-color YELLOW
         (output "~a is polled\n" (format-pids sink))))]
    [('influence (? patch? p))
     (when show-influence?
       (output "~a influenced by ~a via a patch:\n~a\n"
               (format-pids sink)
               (string-join (map format-pids (extract-leaf-pids sink p)) ", ")
               (patch->pretty-string p)))]
    [('influence (message body))
     (when show-influence?
       (output "~a influences ~a with a message:\n~a\n"
               (format-pids source)
               (format-pids sink)
               (pretty-format body)))]))

(define (install-trace-procedure!)
  (define logger (make-logger 'syndicate-trace))
  (define (trace-via-logger n)
    (log-message logger 'info (trace-notification-type n) "" n #f))
  (current-trace-procedures (cons trace-via-logger (current-trace-procedures)))
  logger)

(define ((display-trace logger))
  (define receiver (make-log-receiver logger 'info))
  (parameterize ((pretty-print-columns 100))
    (let loop ()
      (match-define (vector level message-string data event-name) (sync receiver))
      (display-notification data)
      (loop))))

(void (when (not (set-empty? flags))
        (thread (display-trace (install-trace-procedure!)))))
