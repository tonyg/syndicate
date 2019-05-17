#lang racket

(provide string->words
         split-at/lenient-)

(require (only-in racket/list
                  split-at))
(module+ test
  (require rackunit))

(define (string->words s)
    (map (lambda (w) (string-trim w #px"\\p{P}")) (string-split s)))

(module+ test
  (check-equal? (string->words "good day sir")
                (list "good" "day" "sir"))
  (check-equal? (string->words "")
                (list))
  (check-equal? (string->words "good eve ma'am")
                (list "good" "eve" "ma'am"))
  (check-equal? (string->words "please sir. may I have another?")
                (list "please" "sir" "may" "I" "have" "another"))
  ;; TODO - currently fails
  #;(check-equal? (string->words "but wait---there's more")
                  (list "but" "wait" "there's" "more")))

;; (Listof A) Nat -> (List (Listof A) (Listof A))
;; like split-at but allow a number larger than the length of the list
(define (split-at/lenient- lst n)
  (define-values (a b)
    (split-at lst (min n (length lst))))
  (list a b))
