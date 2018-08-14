#lang typed/syndicate/roles

;; Expected Output
;; size: 0
;; size: 2

(define-type-alias ds-type
  (U (Tuple String Int)
     (Observe ★/t)))

(dataspace ds-type
  (spawn ds-type
    (start-facet querier
      (define/query-set key (tuple "key" (bind v Int)) v)
      (assert (tuple "size" (set-count (ref key))))))
  (spawn ds-type
    (start-facet client
      (assert (tuple "key" 18))
      (assert (tuple "key" 88))
      (during (tuple "size" (bind v Int))
        (on start
            (printf "size: ~v\n" v))))))