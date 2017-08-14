#lang racket/base

(provide set-stderr-trace-flags!)

(require data/order)
(require racket/set)
(require racket/match)
(require racket/pretty)
(require (only-in racket/string string-join string-split))
(require "util.rkt")
(require "../core.rkt")
(require "../trace.rkt")
(require "../pretty.rkt")

(define colored-output? (env-aref "SYNDICATE_COLOR" "true" '(("true" #t) ("false" #f))))

(define flags (set))
(define show-exceptions? #f)
(define show-turns? #f)
(define show-turns/state? #f)
(define show-lifecycle? #f)
(define show-actions? #f)
(define show-events? #f)
(define show-events/polls? #f)

(define (set-stderr-trace-flags! flags+module-string)
  (define-values (flags-string module-string)
    (match flags+module-string
      [(regexp #px"^([^:]*):(.*)$" (list _ fs m)) (values fs m)]
      [_ (values flags+module-string "")]))

  (define A-flags (set 'x 'e 'p))
  (set! flags (for/set [(c flags-string)] (string->symbol (string c))))
  (define-syntax-rule (set-flag! symbol variable)
    (set! variable (or (and (set-member? flags 'A) (set-member? A-flags 'symbol))
                       (set-member? flags 'symbol))))
  (set-flag! x show-exceptions?)
  (set-flag! t show-turns?)
  (set-flag! T show-turns/state?)
  (set-flag! p show-lifecycle?)
  (set-flag! a show-actions?)
  (set-flag! e show-events?)
  (set-flag! E show-events/polls?)

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
(define WHITE-ON-BLUE ";1;37;44")
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

(define (output fmt . args)
  (apply fprintf (current-error-port) fmt args))

(define (set-color! c) (when colored-output? (output "\e[0~am" c)))
(define (reset-color!) (when colored-output? (output "\e[0m")))

(define-syntax-rule (with-color c expr ...)
  (begin (set-color! c)
	 (begin0 (begin expr ...)
	   (reset-color!))))

(define (ensure-process-named! process-names point expected-name)
  (define pids (spacetime-space point))
  (define current-name (hash-ref process-names pids #f))
  (when (not (equal? current-name expected-name))
    (with-color WHITE-ON-RED
      (output "Weird: ~a should be named ~v, but is named ~v\n"
              pids
              expected-name
              current-name))))

(define (name-process! process-names point name)
  (hash-set! process-names (spacetime-space point) name))

(define (forget-process! process-names point)
  (hash-remove! process-names (spacetime-space point)))

(define (display-notification the-notification process-names ground-state-box)
  (match-define (trace-notification source sink type detail) the-notification)
  (match* (type detail)
    [('turn-begin (process name _beh state))
     (ensure-process-named! process-names sink name)
     (when (or show-turns? show-turns/state?)
       (with-color BLUE
         (output "~a turn begins\n" (format-point process-names sink))))]
    [('turn-end (process name _beh state))
     (ensure-process-named! process-names sink name)
     (when (null? (spacetime-space sink)) (set-box! ground-state-box state))
     (when (or show-turns? show-turns/state?)
       (with-color BLUE
         (output "~a turn ends\n" (format-point process-names sink))
         (when show-turns/state?
           (syndicate-pretty-print state (current-error-port)))))]
    [('spawn (process name _beh state))
     (name-process! process-names sink name)
     (when show-lifecycle?
       (with-color BRIGHT-GREEN
         (output "~a spawned by ~a\n"
                 (format-point process-names sink)
                 (format-point process-names source))))]
    [('exit #f)
     (when show-lifecycle?
       (with-color BRIGHT-RED
         (output "~a schedules an exit\n" (format-point process-names sink))))]
    [('exit exn)
     (when (or show-lifecycle? show-exceptions?)
       (with-color WHITE-ON-RED
         (output "~a raises an exception:\n~a\n"
                 (format-point process-names sink)
                 (exn->string exn))))]
    [('actions-produced actions)
     (when (or show-events? show-actions?)
       (when (positive? (length actions))
         (output "~a enqueues ~a actions as a result of ~a.\n"
                 (format-point process-names sink)
                 (length actions)
                 (format-point process-names source))))]
    [('action-interpreted (? patch? p))
     (cond
       [show-actions?
        (output "~a interprets patch from ~a:\n~a\n"
                (format-point process-names sink)
                (format-point process-names source)
                (patch->pretty-string (label-patch p #t)))]
       [show-events?
        (output "~a interprets patch from ~a.\n"
                (format-point process-names sink)
                (format-point process-names source))]
       [else (void)])]
    [('action-interpreted (message body))
     (cond
       [show-actions?
        (output "~a delivers broadcast message from ~a:\n~a\n"
                (format-point process-names sink)
                (format-point process-names source)
                (pretty-format body))]
       [show-events?
        (output "~a delivers broadcast message from ~a.\n"
                (format-point process-names sink)
                (format-point process-names source))])]
    [('action-interpreted 'quit)
     (when show-lifecycle?
       (with-color BRIGHT-RED
         (output "~a exits\n" (format-point process-names source))))
     (forget-process! process-names source)]
    [('event (list cause (? patch? p)))
     (when show-events?
       (with-color YELLOW
         (output "~a receives a patch event (direct cause ~a, indirect cause ~a):\n~a\n"
                 (format-point process-names sink)
                 (format-point process-names source)
                 (format-point process-names cause)
                 (match (spacetime-space sink)
                   ['() ;; events from the outside world
                    (patch->pretty-string p)]
                   [(cons _ context-path)
                    (format-patch process-names context-path p)]))))]
    [('event (list cause (message body)))
     (when show-events?
       (with-color YELLOW
         (output "~a receives a message event (direct cause ~a, indirect cause ~a):\n~a\n"
                 (format-point process-names sink)
                 (format-point process-names source)
                 (format-point process-names cause)
                 (pretty-format body))))]
    [('event (list _cause #f)) ;; cause will be #f
     (when show-events/polls?
       (with-color YELLOW
         (output "~a is polled\n" (format-point process-names sink))))]))

(define (summarise-ground-state state)
  (syndicate-pretty-print state (current-error-port)))

(define (install-trace-procedure!)
  (define logger (make-logger 'syndicate-trace))
  (define (trace-via-logger n)
    (log-message logger 'info (trace-notification-type n) "" n #f))
  (current-trace-procedures (cons trace-via-logger (current-trace-procedures)))
  logger)

(define ((display-trace logger))
  (define receiver (make-log-receiver logger 'info))
  (define process-names (make-hash))
  (define ground-state-box (box #f))
  (name-process! process-names (spacetime '() #f) 'ground) ;; by convention
  (define next-signal-evt (check-for-unix-signals-support!))
  (parameterize ((pretty-print-columns 100))
    (let loop ()
      (sync (handle-evt receiver
                        (lambda (v)
                          (match-define (vector level message-string data event-name) v)
                          (display-notification data process-names ground-state-box)
                          (loop)))
            (if next-signal-evt
                (handle-evt next-signal-evt
                            (lambda (signame)
                              (match signame
                                ['SIGUSR1
                                 (with-color WHITE-ON-GREEN
                                   (output "\e[2J\e[HProcess name table:\n")
                                   (for [(pid (in-list (sort (hash-keys process-names)
                                                             (order-<? datum-order))))]
                                     (define name (hash-ref process-names pid))
                                     (output "\t~v\t--> ~v\n" pid name)))]
                                ['SIGUSR2
                                 (with-color WHITE-ON-BLUE
                                   (output "\e[2J\e[HGround routing table:\n")
                                   (summarise-ground-state (unbox ground-state-box)))])
                              (loop)))
                never-evt)))))

(void (when (not (set-empty? flags))
        (thread (display-trace (install-trace-procedure!)))))

(when (getenv "SYNDICATE_STDOUT_TO_STDERR")
  (current-output-port (current-error-port)))
