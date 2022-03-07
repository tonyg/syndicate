#lang setup/infotab
(define scribblings '(("scribblings/syndicate.scrbl" ())))
(define racket-launcher-names '("syndicate-broker" "syndicate-render-msd"))
(define racket-launcher-libraries '("broker/server.rkt" "trace/render-msd.rkt"))
(define test-include-paths '("syndicate/tests"))
(define test-omit-paths
  '(;; Sam: example-plain is interactive, I think
    "examples/example-plain.rkt"
    ;; Sam: for whatever reason I get a failure to load libcrypto for f-to-c
    "examples/actor/f-to-c.rkt"
    ;; Sam: this test displays to stderr which the package server does not like
    "tests/nested-spawn-exceptions.rkt"
    ))
