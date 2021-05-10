#lang info

(define scribblings '(("scribblings/typed-syndicate.scrbl" ())))

(define compile-omit-paths
  '("examples"
    "tests"))

(define test-omit-paths
  ;; a number of the examples use SPIN for model checking which I need
  ;; to figure out how to get working on the package server
  '("examples/"))
