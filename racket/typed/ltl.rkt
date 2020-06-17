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

(struct always [p] #:transparent)
(struct eventually [p] #:transparent)
(struct atomic [p] #:transparent)
(struct weak-until [p q] #:transparent)
(struct strong-until [p q] #:transparent)
(struct ltl-implies [p q] #:transparent)
(struct ltl-and [p q] #:transparent)
(struct ltl-or [p q] #:transparent)
(struct ltl-not [p] #:transparent)

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
