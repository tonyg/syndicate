#lang prospect
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

(require prospect/pretty)

(spawn-network
 (spawn (lambda (e counter)
          (and e
               (let ((new-counter (+ counter 1)))
                 (printf "Received event ~a:\n~a\n" new-counter (prospect-pretty-print->string e))
                 (transition (+ counter 1) '()))))
        0
        (list (patch-seq (assert (at-meta 'x))
                         (sub 'x #:meta-level 1))
              (retract (at-meta 'x)))))
