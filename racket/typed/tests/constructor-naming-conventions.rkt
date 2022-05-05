#lang typed/syndicate

(require rackunit/turnstile)

(define-constructor* (trust lvl))

(check-type (trust 5) : (TrustT Int))

(define-constructor* (hungry-hippos count appetite)
  #:with HungryHippos (HungryHipposT Int String))

(check-type (hungry-hippos 12 "massive") : HungryHippos)

(define-constructor* (doggy [color : String] [weight : Int]))

(check-type (doggy "black" 60) : (DoggyT String Int))
(check-type (doggy "brown" 45) : Doggy)

(define-constructor* (leaf))

(check-type (leaf) : (LeafT))
(check-type (leaf) : Leaf)
