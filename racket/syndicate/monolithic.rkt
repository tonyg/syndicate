#lang racket/base

(module reader syntax/module-reader
  syndicate/monolithic)

(require (except-in "core-lang.rkt"
                    event?
                    action?
                    clean-transition
                    actor
                    actor/stateless))
(require "monolithic/scn.rkt")
(require "monolithic/core.rkt")
(provide (all-from-out "core-lang.rkt")
         (all-from-out "monolithic/scn.rkt")
         (all-from-out "monolithic/core.rkt"))
