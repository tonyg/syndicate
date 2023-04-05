#lang typed/syndicate

(define-constructor* (create-cell : CreateCellT id value))

(define-type-alias ds-type
  (U (Message (CreateCellT Symbol Int))
     (Observe (CreateCellT ★/t ★/t)))
  )

(define (spawn-cell-factory)
  (spawn ds-type
         (start-facet cell-factory
           (on (message (create-cell (bind id Symbol) (bind init Int)))
               (spawn ds-type
                      (react #f))))))
