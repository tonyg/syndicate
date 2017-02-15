#lang syndicate/actor
;; Demonstrates a (fixed) bug in define/query-value scoping.
;;
;; Buggy output:
;;
;; w is #f
;; Process #f (0) died with exception:
;; application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: '(val 123)
;;   arguments...:
;;    '(val 123)
;;   context...:
;;
;; Expected output:
;;
;; w is #f
;; w is '(val 123)
;;
;; Diagnosis:
;;
;; The field being defined, w, was being shadowed by the w binding in
;; the query pattern. Adding `(let ((F field-name)) ...)` in the
;; query-value* macro (and friends) avoids the issue.

(spawn (define/query-value w #f ($ w (list 'val _)) w)
       (begin/dataflow
         (log-info "w is ~v" (w))))

(spawn (assert (list 'val 123)))
