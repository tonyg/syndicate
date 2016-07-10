#lang syndicate/actor
;; Illustrates a (now fixed) bug where mutation altering a
;; subscription caused the `retracted` half of a during instance to be
;; lost.
;;
;; Symptomatic output:
;; x=123 v=999
;; x=124 v=999
;;
;; Correct output:
;; x=123 v=999
;; finally for x=124 v=999
;; x=124 v=999
;;
;; Should eventually be turned into some kind of test case.

(struct foo (x y) #:prefab)

(actor (field [x 123])
       (react
        (assert (foo (x) 999))
        (during (foo (x) $v)
                (log-info "x=~a v=~a" (x) v)
                (when (= (x) 123) (x 124))
                (on-stop
                 (log-info "finally for x=~a v=~a" (x) v)))))
