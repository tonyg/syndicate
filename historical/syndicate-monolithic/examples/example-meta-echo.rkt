#lang syndicate-monolithic
;; Test case for a historical bug in Syndicate.
;;
;; When the bug existed, this program receiveed four SCN events in
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

(require syndicate/pretty)

(spawn-dataspace
 (spawn (lambda (e counter)
          (and e
               (let ((new-counter (+ counter 1)))
                 (printf "Received event ~a:\n~a\n" new-counter (syndicate-pretty-print->string e))
                 (transition (+ counter 1) '()))))
        0
        (list (scn/union (assertion (at-meta 'x))
                         (subscription 'x #:meta-level 1))
              (scn (subscription 'x #:meta-level 1)))))
