#lang racket/base

(module reader syntax/module-reader
  syndicate/monolithic)

(require (except-in "lang.rkt"
                    event?
                    action?
                    clean-transition
                    spawn))
(require "monolithic/scn.rkt")
(require "monolithic/core.rkt")
(provide (all-from-out "lang.rkt")
         (all-from-out "monolithic/scn.rkt")
         (all-from-out "monolithic/core.rkt"))
