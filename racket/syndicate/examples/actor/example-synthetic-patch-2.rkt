#lang syndicate
;; Illustrates a bug where an endpoint whose interest moves with time,
;; where that interest eventually overlaps with existing interest,
;; fails to be notified of an otherwise-known assertion.
;;
;; Symptomatic output:
;;
;; Outer value 4 = 4
;; Value 0 = 0
;; Value 1 = 1
;; Value 2 = 2
;; Value 3 = 3
;;
;; Correct output:
;;
;; Outer value 4 = 4
;; Value 0 = 0
;; Value 1 = 1
;; Value 2 = 2
;; Value 3 = 3
;; Value 4 = 4
;; Value 5 = 5

(spawn (field [of-interest 0])

       (during 'ready
         (on (asserted (list (of-interest) $v))
             (printf "Value ~a = ~a\n" (of-interest) v)
             (of-interest (+ (of-interest) 1))))

       (on (asserted (list 4 $v))
           (printf "Outer value ~a = ~a\n" 4 v)))

(spawn (assert (list 0 0))
       (assert (list 1 1))
       (assert (list 2 2))
       (assert (list 3 3))
       (assert (list 4 4))
       (assert (list 5 5))
       (on-start (flush!)
                 (assert! 'ready)))
