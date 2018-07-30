#lang typed/syndicate/roles

;; Expected Output:
;; +42
;; +18
;; +88
;; -18

(define-type-alias ds-type
  (U (Tuple Int)
     (Observe (Tuple ★/t))))

(dataspace ds-type
  (spawn ds-type
   (print-role
   (start-facet doomed
     (fields)
     (assert (tuple 18))
     (on (asserted (tuple 42))
         (stop doomed
                (start-facet the-afterlife
                  (fields)
                  (assert (tuple 88))))))))

  (spawn ds-type
    (start-facet obs
      (fields)
      (assert (tuple 42))
      (on (asserted (tuple (bind x Int)))
          (printf "+~v\n" x))
      (on (retracted (tuple (bind x Int)))
          (printf "-~v\n" x)))))