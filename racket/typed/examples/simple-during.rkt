#lang typed/syndicate

;; Expected Output
;; +GO
;; +ready
;; -GO
;; -ready

(define-type-alias ds-type
  (U (Tuple String) (Observe (Tuple ★/t))))

(run-ground-dataspace ds-type
  (spawn ds-type
    (start-facet _
      (during (tuple "GO")
        (assert (tuple "ready")))))
  (spawn ds-type
    (start-facet flag
      ;; type error when this was mistakenly just "GO"
      (assert (tuple "GO"))
      (on (asserted (tuple "ready"))
          (stop flag))))
  (spawn ds-type
    (start-facet obs
      (during (tuple (bind s String))
        (on start
            (printf "+~a\n" s))
        (on stop
            (printf "-~a\n" s))))))
