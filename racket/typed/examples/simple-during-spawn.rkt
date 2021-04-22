#lang typed/syndicate

;; Expected Output
;; +parent
;; +GO
;; +ready
;; -parent
;; -GO
;; -ready

(define-type-alias ds-type
  (U (Tuple String) (Observe (Tuple ★/t))))

(run-ground-dataspace ds-type
                      (spawn ds-type
                             (start-facet parent
                                          (assert (tuple "parent"))
                                          (during/spawn (tuple "GO")
                                                        (assert (tuple "ready")))
                                          (on (asserted (tuple "ready"))
                                              (stop parent))))
                      (spawn ds-type
                             (start-facet flag
                                          (assert (tuple "GO"))
                                          (on (retracted (tuple "parent"))
                                              (stop flag))))
                      (spawn ds-type
                             (start-facet obs
                                          (during (tuple (bind s String))
                                                  (on start
                                                      (printf "+~a\n" s))
                                                  (on stop
                                                      (printf "-~a\n" s))))))
