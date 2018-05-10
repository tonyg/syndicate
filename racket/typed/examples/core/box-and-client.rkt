#lang typed/syndicate/core

(define-constructor (set-box new-value)
  #:type-constructor SetBoxT
  #:with SetBox (SetBoxT Int))

(define-constructor (box-state value)
  #:type-constructor BoxStateT
  #:with BoxState (BoxStateT Int))

(define-type-alias τ-c
  (U BoxState
     (Observe (BoxStateT ★/t))
     SetBox
     (Observe (SetBoxT ★/t))))

(dataspace τ-c
  (list
  (actor τ-c
         (lambda ([e : (Event τ-c)]
                  [current-value : Int])
           (let ([sets (project [(set-box (bind v Int)) (patch-added e)] v)])
             (if (empty? sets)
                 idle
                 (let ([new-value (first sets)])
                   (displayln new-value)
                   (transition new-value (list (patch (make-assertion-set (box-state new-value))
                                                      (make-assertion-set (box-state current-value)))))))))
         0
         (make-assertion-set (box-state 0)
                             (observe (set-box ★))))

  (actor τ-c
         (lambda ([e : (Event τ-c)]
                  [s : (Tuple)])
           (let ([updates (project [(box-state (bind v Int)) (patch-added e)] v)])
             (if (empty? updates)
                 idle
                 (let ([new-value (first updates)])
                   (if (> new-value 9)
                       (quit)
                       (transition s (list (patch (make-assertion-set (set-box (+ new-value 1)))
                                                  (make-assertion-set (set-box ★))))))))))
         (tuple)
         (make-assertion-set (observe (box-state ★))))))