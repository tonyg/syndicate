#lang racket

(provide (all-defined-out))

;; an [LTL X] is one of
;;  - (always [LTL X])
;;  - (eventually [LTL X])
;;  - (weak-until [LTL X] [LTL X])
;;  - (strong-until [LTL X] [LTL X])
;;  - (ltl-implies [LTL X] [LTL X])
;;  - (ltl-and [Listof [LTL X]])
;;  - (ltl-or  [Listof [LTL X]])
;;  - (ltl-not [LTL X])
;;  - (atomic X)
;;  - Bool
;; where X represents the type of atomic propositions

(struct always [p] #:prefab)
(struct eventually [p] #:prefab)
(struct atomic [p] #:prefab)
(struct weak-until [p q] #:prefab)
(struct strong-until [p q] #:prefab)
(struct ltl-implies [p q] #:prefab)
(struct ltl-and [p q] #:prefab)
(struct ltl-or [p q] #:prefab)
(struct ltl-not [p] #:prefab)

;; [LTL X] {X -> Y} -> [LTL Y]
(define (map-atomic ltl op)
  (let loop ([ltl ltl])
    (match ltl
      [(always p)
       (always (loop p))]
      [(eventually p)
       (eventually (loop p))]
      [(weak-until p q)
       (weak-until (loop p) (loop q))]
      [(strong-until p q)
       (strong-until (loop p) (loop q))]
      [(ltl-implies p q)
       (ltl-implies (loop p) (loop q))]
      [(ltl-and p q)
       (ltl-and (loop p) (loop q))]
      [(ltl-or p q)
       (ltl-or (loop p) (loop q))]
      [(ltl-not p)
       (ltl-not (loop p))]
      [(atomic x)
       (atomic (op x))]
      [#t
       #t]
      [#f
       #f])))

(define (&& . args)
  (fold-bin-op ltl-and args #t))

(define (|| . args)
  (fold-bin-op ltl-or args #f))

(define (fold-bin-op op args base)
  (let loop ([args args])
    (match args
      ['()
       base]
      [(list x y)
       (op x y)]
      [(cons fst rst)
       (op fst (loop rst))])))
