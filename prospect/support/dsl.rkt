#lang racket/base

(require (for-syntax racket/base))
(provide define&provide-dsl-helper-syntaxes)

(define (illegal-use id context stx)
  (raise-syntax-error #f (format "Illegal use of ~a outside ~a" id context) stx))

(define-syntax-rule (define&provide-dsl-helper-syntaxes context (identifier ...))
  (begin (provide identifier ...)
	 (define-syntax (identifier stx) (illegal-use 'identifier context stx))
         ...))
