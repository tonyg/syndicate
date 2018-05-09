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

(actor τ-c
       (lambda ([e : (Event τ-c)]
                [current-value : Int])
         (quit))
       0
       (make-assertion-set (box-state 0)
                           (observe (set-box ★))))

#;(actor (lambda (e current-value)
         (match-event e
           [(message (set-box new-value))
            (log-info "box: taking on new-value ~v" new-value)
            (transition new-value (patch-seq (retract (box-state current-value))
                                             (assert (box-state new-value))))]))
       0
       (patch-seq (sub (set-box ?))
                  (assert (box-state 0))))