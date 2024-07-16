#lang racket/base

(require "actor-lang.rkt")
(require "drivers/repl.rkt")
(require syntax/parse/define)

(provide (except-out (all-from-out "actor-lang.rkt") #%module-begin)
         (rename-out [interactive-module-begin #%module-begin])
         (all-from-out "drivers/repl.rkt"))

(define-syntax-parse-rule (interactive-module-begin forms ...)
  (#%module-begin
   (module+ main (current-ground-dataspace run-ground-background))
   (require/activate syndicate/drivers/repl)
   forms ...))

(define (run-ground-background . boot-actions)
  (thread (lambda () (run-ground boot-actions))))
