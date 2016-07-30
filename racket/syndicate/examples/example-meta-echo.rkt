#lang syndicate
;; Test case for a historical bug in Syndicate.
;;
;; When the bug existed, this program received four SCN events in
;; total, whereas it should receive only two.
;;
;; While metamessages were "echo cancelled", and receivers only ever
;; got one copy of a sent metamessage no matter how many metas there
;; were, state changes were not. Issuing a quick enough "pulse" of
;; metaassertion while maintaining interest in it led to an "echo":
;; multiple receipts of the pulse.
;;
;; The fix was to adjust the implementation of state change
;; notifications to cancel the echo for metaassertions.
;;
;; 20160730 I'm in the process of revising the design of dataspace
;; relaying to avoid this problem in a different way. Instead of just
;; having `at-meta` for both inbound and outbound assertions, there
;; are now two constructors, `inbound` and `outbound`, and the relay
;; function of the dataspace pays attention to each in a different
;; way. Now there cannot be (accidental) routing loops: asserting
;; something `outbound`, no matter how briefly, will only ever result
;; in a pulse of an `inbound` assertion.

(require syndicate/pretty)

(spawn-dataspace
 (spawn (lambda (e counter)
          (and e
               (let ((new-counter (+ counter 1)))
                 (printf "Received event ~a:\n~a\n" new-counter (syndicate-pretty-print->string e))
                 (transition (+ counter 1) '()))))
        0
        (list (patch-seq (assert (outbound 'x))
                         (sub (inbound 'x)))
              (retract (outbound 'x)))))
