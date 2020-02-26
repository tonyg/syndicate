#lang typed/syndicate/roles

(require rackunit/turnstile)

(define-type-alias WordCount (Hash String Int))

(define (word-count-increment [h : WordCount]
                              [word : String]
                              -> WordCount)
  (hash-update/failure h
                       word
                       add1
                       0))

(define (count-new-words [word-count : WordCount]
                         [words : (List String)]
                         -> WordCount)
  (for/fold ([result word-count])
            ([word words])
    (word-count-increment result word)))

(check-type (count-new-words (hash) (list "hi" "bye"))
            : WordCount
            ⇒ (hash "bye" 1 "hi" 1))

;; OG error:
; <pkgs>/syndicate/typed/tests/regression-count-new-words.rkt:20.4: #%app: bad syntax
;   in: (#%app word-count-increment result word)

;; turns out I needed a #:cut in the rule for #%app (even tho it was the last
;; syntax-parse case??)
