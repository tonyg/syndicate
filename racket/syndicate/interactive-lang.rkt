#lang racket/base

(require "actor-lang.rkt")
(require "drivers/repl.rkt")
(require syntax/parse/define)

(provide (except-out (all-from-out "actor-lang.rkt") #%module-begin assert spawn quit retract)
         (rename-out [interactive-module-begin #%module-begin]
                     [repl-assert assert]
                     [repl-spawn spawn]
                     [do-quit quit]
                     [do-retract retract]
                     [do-send send]
                     [do-receive receive])
         (all-from-out "drivers/repl.rkt"))

(define-syntax-parse-rule (interactive-module-begin forms ...)
  (#%module-begin
   (module+ main (current-ground-dataspace run-ground-background))
   (require/activate syndicate/drivers/repl)
   forms ...))

(define (run-ground-background . boot-actions)
  (thread (lambda () (run-ground boot-actions))))
