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

(define-generics prospect-pretty-printable
  (prospect-pretty-print prospect-pretty-printable [port])
  #:defaults ([(lambda (x) #t)
               (define (prospect-pretty-print v [p (current-output-port)])
                 (pretty-write v p))]))

(define (string-indent amount s)
  (define pad (make-string amount #\space))
  (string-join (for/list [(line (string-split s "\n"))] (string-append pad line)) "\n"))

(define (indented-port-output amount f)
  (define p (open-output-string))
  (f p)
  (string-indent amount (get-output-string p)))
