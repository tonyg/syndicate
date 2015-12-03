#lang racket/base

(require racket/match)
(require (only-in racket/port read-line-evt))
(require "../main.rkt")
(require "../drivers/timer.rkt")

(define (quasi-spy e s)
  (printf "----------------------------------------\n")
  (printf "QUASI-SPY:\n")
  (match e
    [(? patch? p) (pretty-print-patch p)]
    [other
     (write other)
     (newline)])
  (printf "========================================\n")
  #f)

(define (r e s)
  (match e
    [(message body) (transition s (message (at-meta `(print (got ,body)))))]
    [_ #f]))

(define (b e n)
  (match e
    [#f (if (< n 10)
	    (transition (+ n 1) (message `(hello ,n)))
	    #f)]
    [_ #f]))

(define (echoer e s)
  (match e
    [(message (at-meta (external-event _ (list (? eof-object?)))))
     (quit)]
    [(message (at-meta (external-event _ (list line))))
     (transition s (message `(print (got-line ,line))))]
    [_ #f]))

(define (ticker e s)
  (match e
    [(? patch? p)
     (printf "TICKER PATCH RECEIVED:\n")
     (pretty-print-patch p)
     #f]
    [(message (timer-expired 'tick now))
     (printf "TICK ~v\n" now)
     (if (< s 3)
         (transition (+ s 1) (message (set-timer 'tick 1000 'relative)))
         (quit))]
    [_ #f]))

(define (printer e s)
  (match e
    [(message (list 'print v))
     (log-info "PRINTER: ~a" v)
     #f]
    [_ #f]))

(run-ground (spawn quasi-spy (void) (sub ?))
	    (spawn-timer-driver)
	    (message (set-timer 'tick 1000 'relative))
	    (spawn ticker
                   1
                   (patch-seq (sub (observe (set-timer ? ? ?)))
                              (sub (timer-expired 'tick ?))))
	    (spawn-world (spawn r (void) (sub ?))
			 (spawn b 0 '()))
	    (spawn echoer
                   (void)
                   (sub (external-event (read-line-evt (current-input-port) 'any) ?)
                        #:meta-level 1))
	    (spawn printer (void) (sub `(print ,?))))
