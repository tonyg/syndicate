#lang racket/base

(provide fmod
         hsv->color
         color-by-hash)

(require 2htdp/image)

(define (fmod a b)
  (- a (* b (truncate (/ a b)))))

(define (hsv->color h s v)
  (define h* (fmod (/ h 60.0) 6))
  (define chroma (* v s))
  (define x (* chroma (- 1 (abs (- (fmod h* 2) 1)))))
  (define-values (r g b)
    (cond
     [(< h* 1) (values chroma x 0)]
     [(< h* 2) (values x chroma 0)]
     [(< h* 3) (values 0 chroma x)]
     [(< h* 4) (values 0 x chroma)]
     [(< h* 5) (values x 0 chroma)]
     [else     (values chroma 0 x)]))
  (define m (- v chroma))
  (define (scale x) (inexact->exact (truncate (* 255 (+ x m)))))
  (make-color (scale r) (scale g) (scale b)))

(define (color-by-hash v)
  (hsv->color (* 360.0 (/ (bitwise-and (equal-hash-code v) 16777215) 16777216.0)) 1 1))
