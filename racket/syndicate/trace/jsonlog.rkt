#lang racket/base

(require json)
(require racket/match)
(require racket/format)

(require "../core.rkt")
(require "../dataspace.rkt")
(require "../trace.rkt")
(require "../mux.rkt")
(require "../tset.rkt")
(require "../trie.rkt")

(define (label->jsexpr label)
  (match label
    ['meta "meta"]
    [(? number? n) n]))

(define (annotate-label label w)
  (define proc (hash-ref (dataspace-process-table w) label #f))
  (list (label->jsexpr label)
        (and proc (~a (process-name proc)))))

(define ((prettify-processes w) labels)
  (for/list [(label (tset->list labels))]
    (annotate-label label w)))

(define (serialize-atom x)
  (cond
    [(symbol? x) (symbol->string x)]
    [(seal? x) (hash 'seal (serialize-atom (seal-contents x)))]
    [(number? x) (exact->inexact x)]
    [(string? x) x]
    [(boolean? x) x]
    [else (hash 'misc (~a x))]))

(void
 (thread
  (lambda ()
    (define receiver (make-log-receiver trace-logger 'info))
    (define p (open-output-file "syndicate-log.json" #:exists 'append))
    (let loop ()
      (match-define (vector level message-string data event-name) (sync receiver))
      (match* (event-name data)
        [('internal-action-result (list pids a old-w t)) #:when (transition? t)
         (define new-w (transition-state t))
         (define new-mux (dataspace-mux new-w))
         (define old-table (mux-routing-table (dataspace-mux old-w)))
         (define new-table (mux-routing-table new-mux))
         (when (not (equal? old-table new-table))
           (define j (list (current-inexact-milliseconds)
                           (map label->jsexpr (reverse (cdr pids)))
                           (for/list [((label assertions) (in-hash (mux-interest-table new-mux)))]
                             (list (annotate-label label new-w)
                                   (trie->jsexpr assertions (lambda (v) #t)
                                                 #:serialize-atom serialize-atom)))))
           (write-json j p)
           (newline p))]
        [(_ _) (void)])
      (loop)))))
