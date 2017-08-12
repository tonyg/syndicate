#lang racket/base

(provide env-aref
         format-pids
         extract-leaf-pids)

(require racket/set)
(require racket/match)
(require (only-in racket/string string-join))
(require "../patch.rkt")

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

(define (extract-leaf-pids sink p)
  (for/list [(pid (in-set (extract-patch-pids p)))]
    (cons pid (cdr sink))))
