#lang syndicate
;; The actor should receive a single event adding the (at-meta x) assertion.

(require syndicate/pretty)

(spawn-dataspace
 (spawn (lambda (e counter)
          (and e
               (let ((new-counter (+ counter 1)))
                 (printf "Received event ~a:\n~a\n" new-counter (syndicate-pretty-print->string e))
                 (transition (+ counter 1) '()))))
        0
        (list (patch-seq (sub 'x #:meta-level 1)
                         (assert (at-meta 'x))))))
