#lang typed/syndicate

(require rackunit/turnstile)

(define-constructor* (trust lvl))

(check-type (trust 5) : (TrustT Int))

(define-constructor* (hungry-hippos count appetite)
  #:with HungryHippos (HungryHipposT Int String))

(check-type (hungry-hippos 12 "massive") : HungryHippos)
