#lang prospect-monolithic
;; Demonstrates a (hopefully soon historical!) bug in Syndicate.
;;
;; When the bug exists, this program receives four SCN events in
;; total, whereas it should receive only two.
;;
;; While metamessages are "echo cancelled", and receivers only ever
;; get one copy of a sent metamessage no matter how many metas there
;; are, state changes are not (yet). Issuing a quick enough "pulse" of
;; metaassertion while maintaining interest in it leads to an "echo":
;; multiple receipts of the pulse.
;;
;; The fix is to adjust the implementation of state change
;; notifications to cancel the echo for metaassertions.

(require prospect/pretty)

(spawn-network
 (spawn (lambda (e counter)
          (and e
               (let ((new-counter (+ counter 1)))
                 (printf "Received event ~a:\n~a\n" new-counter (prospect-pretty-print->string e))
                 (transition (+ counter 1) '()))))
        0
        (list (scn/union (assertion (at-meta 'x))
                         (subscription 'x #:meta-level 1))
              (scn (subscription 'x #:meta-level 1)))))
