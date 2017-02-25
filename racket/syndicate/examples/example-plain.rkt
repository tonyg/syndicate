#lang racket/base

(require racket/match)
(require (only-in racket/port read-line-evt))
(require "../main.rkt")
(require "../drivers/timer.rkt")

(define sub-all-except-meta
  (patch-seq (sub ?)
             (unsub (inbound ?))
             (unsub (outbound ?))))

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
    [(message body) (transition s (message (outbound `(print (got ,body)))))]
    [_ #f]))

(define (b e n)
  (match e
    [#f (if (< n 10)
	    (transition (+ n 1) (message `(hello ,n)))
	    #f)]
    [_ #f]))

(define (echoer e s)
  (match e
    [(message (inbound (external-event _ (list (? eof-object?)))))
     (quit)]
    [(message (inbound (external-event _ (list line))))
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

(run-ground (actor quasi-spy (void) sub-all-except-meta)
	    (spawn-timer-driver)
	    (message (set-timer 'tick 1000 'relative))
	    (actor ticker
                   1
                   (patch-seq (sub (observe (set-timer ? ? ?)))
                              (sub (timer-expired 'tick ?))))
	    (dataspace-actor (actor r (void) sub-all-except-meta)
                             (actor b 0 '()))
	    (actor echoer
                   (void)
                   (sub (inbound (external-event (read-line-evt (current-input-port) 'any) ?))))
	    (actor printer (void) (sub `(print ,?))))
