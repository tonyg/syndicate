#lang racket/base

(provide random-hex-string)

(require (only-in file/sha1 bytes->hex-string))
(require (only-in racket/random crypto-random-bytes))

(define (random-hex-string half-length)
  (bytes->hex-string (crypto-random-bytes half-length)))
