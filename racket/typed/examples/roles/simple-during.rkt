#lang typed/syndicate/roles

;; Expected Output
;; +GO
;; +ready
;; -GO
;; -ready

(define-type-alias ds-type
  (U (Tuple String) (Observe (Tuple ★/t))))

(dataspace ds-type
  (spawn ds-type
    (start-facet _
      (fields)
      (during (tuple "GO")
        (assert (tuple "ready")))))
  (spawn ds-type
    (start-facet flag
      (fields)
      ;; type error when this was mistakenly just "GO"
      (assert (tuple "GO"))
      (on (asserted (tuple "ready"))
          (stop flag))))
  (spawn ds-type
    (start-facet obs
      (fields)
      (during (tuple (bind s String))
        (on start
            (printf "+~a\n" s))
        (on stop
            (printf "-~a\n" s))))))
