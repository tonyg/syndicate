#lang syndicate
;; The actor should receive a single event adding an (inbound 'x) assertion.

(require syndicate/pretty)

(spawn-dataspace
 (actor (lambda (e counter)
          (and e
               (let ((new-counter (+ counter 1)))
                 (printf "Received event ~a:\n~a\n" new-counter (syndicate-pretty-print->string e))
                 (transition (+ counter 1) '()))))
        0
        (list (patch-seq (sub (inbound 'x))
                         (assert (outbound 'x))))))
