#lang racket/base

(provide gen:syndicate-pretty-printable
         syndicate-pretty-print

         syndicate-pretty-print->string

         exn->string ;; required from racket/exn
         string-indent
         indented-port-output)

(require racket/generic)
(require racket/pretty)
(require racket/exn)
(require (only-in racket/string string-join string-split))
(require "trie.rkt")

(define-generics syndicate-pretty-printable
  (syndicate-pretty-print syndicate-pretty-printable [port])
  #:defaults ([(lambda (x) (and (not (eq? x #f)) (trie? x)))
               (define (syndicate-pretty-print m [p (current-output-port)])
                 (pretty-print-trie m p))]
              [(lambda (x) #t)
               (define (syndicate-pretty-print v [p (current-output-port)])
                 (pretty-write v p))]))

(define (syndicate-pretty-print->string v)
  (define p (open-output-string))
  (syndicate-pretty-print v p)
  (get-output-string p))

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
