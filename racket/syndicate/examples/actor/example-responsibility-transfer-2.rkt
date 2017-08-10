#lang syndicate
;; Demonstrates responsibility transfer.
;;
;; Previously, `spawn` was expanded in place into the new actor's
;; initial actions. This reordering was confusing, as demonstrated in
;; example-responsibility-transfer-1.rkt, motivating a change in the
;; `actor` constructor to make it carry not only a boot procedure but
;; a set of initial assertions.
;;
;; Correct output:
;;
;; Supply 1 asserted.
;;
;; If only (A) is commented out:
;;
;; Supply 1 asserted.
;; Supply 1 retracted.
;; Supply 1 asserted.
;;
;; We see this because `service-1`'s initial patch takes effect
;; *after* previously queued actions -- namely, the patch from
;; `factory-1` retracting (observe (list 'X 1)). The effect of the
;; #:assertions clause is to start `service-1` off with some
;; assertions even before it has produced any actions at all.
;;
;; If only (B) is commented out:
;;
;; Supply 1 asserted.
;; Supply 1 retracted.
;;
;; We see this because even though `service-1` is given initial
;; assertions, the HLL *clears them* as part of its startup. It
;; requires the user to explicitly add endpoints for the assertions to
;; be maintained. Think of the `#:assertions` as *transient*, just for
;; the transition between the startup and normal running phases.
;;
;; If both (A) and (B) are commented out:
;;
;; Supply 1 asserted.
;; Supply 1 retracted.
;;
;; This is straightforwardly because once `factory-1`'s assertion of
;; (observe (list 'X 1)) is withdrawn, there are no instructions for
;; any other party to take it over.

(spawn #:name 'factory-1
       (on (asserted (list 'X 1))
           (spawn #:name 'service-1
                  #:assertions (observe (list 'X 1))  ;; (A)
                  (stop-when (retracted (list 'X 1))) ;; (B)
                  (on (message 'dummy))) ;; exists just to keep the
                                         ;; service alive if there are
                                         ;; no other endpoints
           ;; spawn executes *before* teardown of this on-asserted
           ;; endpoint, and thus before the patch withdrawing (observe
           ;; (list 'X 1)).
           (stop-current-facet)))

(spawn (on (asserted (observe (list 'X $supplier)))
           (printf "Supply ~v asserted.\n" supplier)
           (assert! (list 'X supplier)))
       (on (retracted (observe (list 'X $supplier)))
           (printf "Supply ~v retracted.\n" supplier)))
