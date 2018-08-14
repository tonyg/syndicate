#lang typed/syndicate/roles

;; Expected Output
;; f: 0
;; f: 18

(define-type-alias ds-type
  (U (Tuple String Int)
     (Observe ★/t)))

(dataspace ds-type
  (spawn ds-type
    (start-facet server
      (field [f Int 0])
      (begin/dataflow
        (printf "f = ~v\n" (ref f)))
      (on (asserted (tuple "key" (bind v Int)))
          (set! f v))))
  (spawn ds-type
    (start-facet client
      (assert (tuple "key" 18)))))