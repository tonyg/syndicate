#lang typed/syndicate

;; Expected Output
;; adding key2 -> 88
;; adding key1 -> 18
;; size: 0
;; size: 2
;; removing key2
;; adding key2 -> 99

(assertion-struct output : Output (v))

(define-type-alias ds-type
  (U (Tuple String Int)
     (Output Int)
     (Observe ★/t)))

(run-ground-dataspace ds-type
  (spawn ds-type
   (start-facet querier
     (define/query-hash key# (tuple (bind k String) (bind v Int)) k v
       #:on-add (printf "adding ~a -> ~a\n" k v)
       #:on-remove (printf "removing ~a\n" k))
     (assert (output (hash-count (ref key#))))))
   (spawn ds-type
    (start-facet client
      (assert (tuple "key1" 18))
      (on start
          (start-facet tmp
            (field [v Int 88])
            (assert (tuple "key2" (ref v)))
            (on (asserted (output 2))
                (set! v 99))))
      (during (output (bind v Int))
        (on start
            (printf "size: ~v\n" v))))))
