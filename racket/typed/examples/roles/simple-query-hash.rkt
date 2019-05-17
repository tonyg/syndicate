#lang typed/syndicate/roles

;; Expected Output
;; size: 0
;; size: 2

(assertion-struct output : Output (v))

(define-type-alias ds-type
  (U (Tuple String Int)
     (Output Int)
     (Observe ★/t)))

(run-ground-dataspace ds-type
  (spawn ds-type
   (start-facet querier
     (define/query-hash key# (tuple (bind k String) (bind v Int)) k v)
     (assert (output (hash-count (ref key#))))))
   (spawn ds-type
    (start-facet client
      (assert (tuple "key1" 18))
      (assert (tuple "key2" 88))
      (during (output (bind v Int))
        (on start
            (printf "size: ~v\n" v))))))
