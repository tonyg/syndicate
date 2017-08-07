#lang racket/base
;; Common syntax classes.

(provide (for-syntax assertions
                     name))

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax syntax/srcloc))

(require "trie.rkt")

(begin-for-syntax
  (define-splicing-syntax-class assertions
    (pattern (~seq #:assertions P0) #:attr P #'(pattern->trie '<initial-assertions> P0))
    (pattern (~seq #:assertions* P)) ;; P should be an expression yielding a trie
    (pattern (~seq) #:attr P #'trie-empty))

  (define-splicing-syntax-class name
    (pattern (~seq #:name N))
    (pattern (~seq) #:attr N #'#f)))
