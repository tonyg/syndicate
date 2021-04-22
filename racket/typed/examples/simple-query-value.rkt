#lang typed/syndicate

;; Expected Output
;; query: 0
;; query: 19

(define-type-alias ds-type
  (U (Tuple String Int)
     (Observe ★/t)))

(run-ground-dataspace ds-type
  (spawn ds-type
    (start-facet querier
      (define/query-value key 0 (tuple "key" (bind v Int)) (+ v 1))
      (assert (tuple "query" (ref key)))))
  (spawn ds-type
    (start-facet client
      (assert (tuple "key" 18))
      (during (tuple "query" (bind v Int))
        (on start
            (printf "query: ~v\n" v))))))
