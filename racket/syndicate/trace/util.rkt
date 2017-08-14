#lang racket/base

(provide env-aref
         format-pids
         format-point
         format-patch
         check-for-unix-signals-support!)

(require racket/set)
(require racket/match)
(require (only-in racket/string string-join))
(require "../trie.rkt")
(require "../patch.rkt")
(require "../trace.rkt")
(require "../treap.rkt")
(require "../tset.rkt")

(define (env-aref varname default alist)
  (define key (or (getenv varname) default))
  (cond [(assoc key alist) => cadr]
	[else (error 'env-aref
		     "Expected environment variable ~a to contain one of ~v; got ~v"
		     (map car alist)
		     key)]))

(define (format-pids process-names pids)
  (define pidstr
    (match pids
      ['() "ground"]
      [(cons 'meta rest) (format "context of ~a" (format-pids process-names rest))]
      [_ (string-join (map number->string (reverse pids)) ":")]))
  (match (hash-ref process-names pids #f)
    [#f pidstr]
    [name (format "~a a.k.a ~v" pidstr name)]))

(define (format-point process-names point)
  (match-define (spacetime pids moment) (or point (spacetime #f #f)))
  (string-append (if pids (format-pids process-names pids) "?")
                 (if moment (format " @@~a" moment) "")))

(define (format-patch process-names dataspace-actor-path p)
  (define (expand-pid local-pid) (cons local-pid dataspace-actor-path))
  (define (format-pid local-pid) (format-pids process-names (expand-pid local-pid)))
  (patch->pretty-string
   (patch-relabel p
                  (lambda (local-pids)
                    (string-join (set-map (treap-keys local-pids) format-pid) ", ")))))

(define (check-for-unix-signals-support!)
  (define capture-signal!
    (with-handlers [(void
                     (lambda (e)
                       (log-error "Error signalled during unix-signals check:\n~v\n" e)
                       #f))]
      (dynamic-require 'unix-signals 'capture-signal!)))
  (when (not capture-signal!)
    (log-warning "Cannot load Racket unix-signals package. Signals not available."))
  (and capture-signal!
       (begin (capture-signal! 'SIGUSR1)
              (capture-signal! 'SIGUSR2)
              (let ((lookup-signal-name (dynamic-require 'unix-signals 'lookup-signal-name)))
                (handle-evt (dynamic-require 'unix-signals 'next-signal-evt)
                            lookup-signal-name)))))
