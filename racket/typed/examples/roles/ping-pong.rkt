#lang typed/syndicate/roles

;; Expected Output
;; pong: 8339

(define-type-alias ds-type
  (U (Tuple String Int)
     (Observe (Tuple String ★/t))))

(dataspace ds-type
  (spawn ds-type
    (start-facet echo
      (on (asserted (tuple "ping" (bind x Int)))
          (start-facet _
            (assert (tuple "pong" x))))))
  (spawn ds-type
    (start-facet serve
      (assert (tuple "ping" 8339))
      (on (asserted (tuple "pong" (bind x Int)))
          (printf "pong: ~v\n" x)))))