#lang racket/base

(provide gen:prospect-pretty-printable
         prospect-pretty-print

         exn->string ;; required from exn-util.rkt
         string-indent
         indented-port-output)

(require racket/generic)
(require racket/pretty)
(require (only-in racket/string string-join string-split))
(require "exn-util.rkt")
(require "route.rkt")

(define-generics prospect-pretty-printable
  (prospect-pretty-print prospect-pretty-printable [port])
  #:defaults ([(lambda (x) (and (not (eq? x #f)) (matcher? x)))
               (define (prospect-pretty-print m [p (current-output-port)])
                 (pretty-print-matcher m p))]
              [(lambda (x) #t)
               (define (prospect-pretty-print v [p (current-output-port)])
                 (pretty-write v p))]))

(define (string-indent amount s)
  (define pad (make-string amount #\space))
  (string-join (for/list [(line (string-split s "\n"))] (string-append pad line)) "\n"))

(define (indented-port-output amount f #:first-line? [first-line? #t])
  (define p (open-output-string))
  (f p)
  (define fully-indented (string-indent amount (get-output-string p)))
  (if first-line?
      fully-indented
      (substring fully-indented amount)))
