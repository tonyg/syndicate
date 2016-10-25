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

(define (format-pids process-names pids)
  (define pidstr
    (match pids
      ['() "ground"]
      [(cons 'meta rest) (format "context of ~a" (format-pids process-names rest))]
      [_ (string-join (map number->string (reverse pids)) ":")]))
  (match (hash-ref process-names pids #f)
    [#f pidstr]
    [name (format "~a a.k.a ~v" pidstr name)]))

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

(define (ensure-process-named! process-names pids expected-name)
  (define current-name (hash-ref process-names pids #f))
  (when (not (equal? current-name expected-name))
    (with-color WHITE-ON-RED
      (output "Weird: ~a should be named ~v, but is named ~v\n"
              pids
              expected-name
              current-name))))

(define (name-process! process-names pids name)
  (hash-set! process-names pids name))

(define (forget-process! process-names pids)
  (hash-remove! process-names pids))

(define (display-notification the-notification process-names)
  (match-define (trace-notification source sink type detail) the-notification)
  (match* (type detail)
    [('turn-begin (process name _beh state))
     (ensure-process-named! process-names sink name)
     (when show-turns?
       (with-color BLUE
         (output "~a turn begins\n" (format-pids process-names sink))))]
    [('turn-end (process name _beh state))
     (ensure-process-named! process-names sink name)
     (when show-turns?
       (with-color BLUE
         (output "~a turn ends\n" (format-pids process-names sink))
         (syndicate-pretty-print state (current-error-port))))]
    [('spawn (list parent (process name _beh state)))
     (name-process! process-names sink name)
     (when show-lifecycle?
       (with-color BRIGHT-GREEN
         (output "~a spawned by ~a\n"
                 (format-pids process-names sink)
                 (format-pids process-names parent))))]
    [('exit #f)
     (when show-lifecycle?
       (with-color BRIGHT-RED
         (output "~a schedules an exit\n" (format-pids process-names sink))))]
    [('exit exn)
     (when (or show-lifecycle? show-exceptions?)
       (with-color WHITE-ON-RED
         (output "~a raises an exception:\n~a\n"
                 (format-pids process-names sink)
                 (exn->string exn))))]
    [('action (? patch? p))
     (when show-actions?
       (output "~a performs a patch:\n~a\n"
               (format-pids process-names source)
               (patch->pretty-string p)))]
    [('action (message body))
     (when show-actions?
       (output "~a broadcasts a message:\n~a\n"
               (format-pids process-names source)
               (pretty-format body)))]
    [('action 'quit)
     (when show-lifecycle?
       (with-color BRIGHT-RED
         (output "~a exits\n" (format-pids process-names source))))
     (forget-process! process-names source)]
    [('event (? patch? p))
     (when show-events?
       (with-color YELLOW
         (output "~a receives an event:\n~a\n"
                 (format-pids process-names sink)
                 (patch->pretty-string p))))]
    [('event (message body))
     (when show-events?
       (with-color YELLOW
         (output "~a receives a message:\n~a\n"
                 (format-pids process-names sink)
                 (pretty-format body))))]
    [('event #f)
     (when show-events?
       (with-color YELLOW
         (output "~a is polled\n" (format-pids process-names sink))))]
    [('influence (? patch? p))
     (when show-influence?
       (output "~a influenced by ~a via a patch:\n~a\n"
               (format-pids process-names sink)
               (string-join (map (lambda (p) (format-pids process-names p))
                                 (extract-leaf-pids sink p))
                            ", ")
               (patch->pretty-string p)))]
    [('influence (message body))
     (when show-influence?
       (output "~a influences ~a with a message:\n~a\n"
               (format-pids process-names source)
               (format-pids process-names sink)
               (pretty-format body)))]))

(define (install-trace-procedure!)
  (define logger (make-logger 'syndicate-trace))
  (define (trace-via-logger n)
    (log-message logger 'info (trace-notification-type n) "" n #f))
  (current-trace-procedures (cons trace-via-logger (current-trace-procedures)))
  logger)

(define (check-for-unix-signals-support!)
  (define capture-signal! (with-handlers [(void (lambda _ #f))]
                            (dynamic-require 'unix-signals 'capture-signal!)))
  (and capture-signal!
       (begin (capture-signal! 'SIGUSR1)
              (dynamic-require 'unix-signals 'next-signal-evt))))

(define ((display-trace logger))
  (define receiver (make-log-receiver logger 'info))
  (define process-names (make-hash))
  (name-process! process-names '() 'ground) ;; by convention
  (define next-signal-evt (check-for-unix-signals-support!))
  (parameterize ((pretty-print-columns 100))
    (let loop ()
      (sync (handle-evt receiver
                        (lambda (v)
                          (match-define (vector level message-string data event-name) v)
                          (display-notification data process-names)
                          (loop)))
            (if next-signal-evt
                (handle-evt next-signal-evt
                            (lambda (_signum)
                              (with-color WHITE-ON-GREEN
                                (output "\e[2J\e[HProcess name table:\n")
                                (for [((pid name) (in-hash process-names))]
                                  (output "\t~v\t--> ~v\n" pid name)))
                              (loop)))
                never-evt)))))

(void (when (not (set-empty? flags))
        (thread (display-trace (install-trace-procedure!)))))

(when (getenv "SYNDICATE_STDOUT_TO_STDERR")
  (current-output-port (current-error-port)))
