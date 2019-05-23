#lang typed/syndicate/roles

(require rackunit/turnstile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define id
 (Λ [τ]
   (lambda ([x : τ])
     x)))

(check-type id : (∀ (τ) (→fn τ τ)))

(check-type ((inst id Int) 1)
            : Int
            ⇒ 1)
(check-type ((inst id String) "hello")
            : String
            ⇒ "hello")

(define poly-first
  (Λ [τ σ]
     (lambda ([t : (Tuple τ σ)])
       (select 0 t))))

(check-type poly-first : (∀ (A B) (→fn (Tuple A B) A)))

(check-type ((inst poly-first Int String) (tuple 13 "XD"))
            : Int
            ⇒ 13)

(check-type ((inst poly-first Int String) (tuple 13 "XD"))
            : Int
            ⇒ 13)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (∀ (X) (id2 [x : X] -> X))
  x)

(check-type ((inst id2 Int) 42)
            : Int
            ⇒ 42)

(define (∀ (X) (id3 [x : X]))
  x)

(check-type (+ ((inst id3 Int) 42) 1)
            : Int
            ⇒ 43)

;; test type variable scoping

(define (∀ (X) (id4 [x : X] -> X))
  (match x
    [(bind y X) y]))

(check-type ((inst id2 String) "shelly flowers")
            : String
            ⇒ "shelly flowers")
(define id5
  (Λ [τ]
     (lambda ([x : τ])
       (match x
         [(bind y τ) y]))))

(typecheck-fail (inst id5 (→fn Int Int))
                #:with-msg "types must be instantiable")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shorthands for match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-type (match 5
              [$x:Int (add1 x)])
            : Int
            ⇒ 6)

(check-type (match (tuple 3 "hello")
              [(tuple _ $str:String)
               str])
            : String
            ⇒ "hello")

(check-type (match (tuple 3 "hello")
              [(tuple _ $str)
               str])
            : String
            ⇒ "hello")
