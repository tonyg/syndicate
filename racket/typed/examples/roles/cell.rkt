#lang typed/syndicate/roles

;; adapted from section 8.3 of Tony's dissertation

(define-constructor* (cell : CellT id value))
(define-constructor* (create-cell : CreateCellT id value))
(define-constructor* (update-cell : UpdateCellT id value))
(define-constructor* (delete-cell : DeleteCellT id))

(define-type-alias ID Int)
(define-type-alias Value String)

(define-type-alias Cell
  (Role (cell)
        (Shares (CellT ID Value))
        (Reacts (Message (UpdateCellT ID ★/t))
                )
        (Reacts (Message (DeleteCellT ID))
                (Stop cell))))

(define-type-alias CellFactory
  (Role (cell-factory)
        (Reacts (Message (CreateCellT ID Value))
                ;; want to say that what it spawns is a Cell
                (Spawns ★/t))))

(define-type-alias Reader
  (Role (reader)
        (Shares (Observe (CellT ID ★/t)))))

(define-type-alias Writer
  (Role (writer)
        ;; sends update and delete messages
        ))

(define-type-alias ds-type
  (U (CellT ID Value)
     (Observe (CellT ID ★/t))
     (Message (CreateCellT ID Value))
     (Message (UpdateCellT ID Value))
     (Message (DeleteCellT ID))
     (Observe (CreateCellT ★/t ★/t))
     (Observe (UpdateCellT ID ★/t))
     (Observe (DeleteCellT ID))))

(define (spawn-cell! [initial-value : Value])
  (define id 1234)
  (send! (create-cell id initial-value))
  id)

(define (spawn-cell-factory)
  (spawn ds-type
   (start-facet cell-factory
     (on (message (create-cell (bind id ID) (bind init Value)))
         (spawn ds-type
           (start-facet the-cell
             (field [value Value init])
             (assert (cell id (ref value)))
             (on (message (update-cell id (bind new-value Value)))
                 (set! value new-value))
             (on (message (delete-cell id))
                 (stop the-cell))))))))


(define (spawn-cell-monitor [id : ID])
  (spawn ds-type
    (start-facet monitor
      (on (asserted (cell id (bind value Value)))
          (printf "Cell ~a updated to: ~a\n" id value))
      (on (retracted (cell id discard))
          (printf "Cell ~a deleted\n" id)))))
