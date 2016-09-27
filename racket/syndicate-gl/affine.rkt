#lang racket/base
;; 2D affine transformation matrices.

;; These are *active* transformations: they transform vectors to new
;; vectors, rather than coordinate systems to new coordinate systems.

(provide (struct-out transformation-matrix)

         identity-transformation
         translation-transformation
         rotation-transformation
         stretching-transformation
         shearing-transformation
         invert-transformation
         compose-transformation

         transform-point
         transform-vector
         untransform-point
         untransform-vector)

(require (only-in racket/math pi))
(require racket/match)

(struct transformation-matrix (a b c d tx ty) #:prefab)

(define identity-transformation
  (transformation-matrix 1 0 0 1 0 0))

(define (translation-transformation x y)
  (transformation-matrix 1 0 0 1 x y))

(define (rad deg) (* deg (/ pi 180.0)))

(define (rotation-transformation theta-d)
  (match theta-d
    [(or 0 360) identity-transformation]
    [(or 90 -270) (transformation-matrix 0 1 -1 0 0 0)]
    [180 (transformation-matrix -1 0 0 -1 0 0)]
    [(or 270 -90) (transformation-matrix 0 -1 1 0 0 0)]
    [_
     (define theta-r (rad theta-d))
     (define c (cos theta-r))
     (define s (sin theta-r))
     (transformation-matrix c s (- s) c 0 0)]))

(define (stretching-transformation sx [sy sx])
  (transformation-matrix sx 0 0 sy 0 0))

(define (shearing-transformation sx sy)
  (transformation-matrix 1 sy sx 1 0 0))

(define (invert-transformation m)
  (define det (determinant m))
  (when (zero? det) (error 'invert-transformation "Zero determinant"))
  (define -det (- det))
  (match-define (transformation-matrix a b c d tx ty) m)
  (transformation-matrix (/ d det)
                         (/ b -det)
                         (/ c -det)
                         (/ a det)
                         (/ (- (* c ty) (* d tx)) det)
                         (/ (- (* b tx) (* a ty)) det)))

(define (determinant m)
  (match-define (transformation-matrix a b c d _ _) m)
  (- (* a d) (* b c)))

(define (compose-transformation* m1 m0)
  (match-define (transformation-matrix a b c d tx ty) m1)
  (match-define (transformation-matrix e f g h sx sy) m0)
  (transformation-matrix (+ (* a e) (* c f))
                         (+ (* b e) (* d f))
                         (+ (* a g) (* c h))
                         (+ (* b g) (* d h))
                         (+ (* a sx) (* c sy) tx)
                         (+ (* b sx) (* d sy) ty)))

(define compose-transformation
  (case-lambda
    [() identity-transformation]
    [(m) m]
    [(m1 m0) (compose-transformation* m1 m0)]
    [mtxs (foldr compose-transformation* identity-transformation mtxs)]))

(define (transform-point m v)
  (match-define (transformation-matrix a b c d tx ty) m)
  (define x (real-part v))
  (define y (imag-part v))
  (make-rectangular (+ (* a x) (* c y) tx)
                    (+ (* b x) (* d y) ty)))

(define (transform-vector m v)
  (match-define (transformation-matrix a b c d _ _) m)
  (define x (real-part v))
  (define y (imag-part v))
  (make-rectangular (+ (* a x) (* c y))
                    (+ (* b x) (* d y))))

(define (untransform-point m v)
  (transform-point (invert-transformation m) v))

(define (untransform-vector m v)
  (transform-vector (invert-transformation m) v))

(module+ test
  (require rackunit)

  (define eps 0.00001)
  (define invrt2 (/ (sqrt 2)))

  (define (within-eps a b) (< (magnitude (- a b)) eps))

  (define-binary-check (check-transformation~? actual expected)
    (match-let (((transformation-matrix aa ab ac ad atx aty) actual)
                ((transformation-matrix ea eb ec ed etx ety) expected))
      (and (within-eps aa ea)
           (within-eps ab eb)
           (within-eps ac ec)
           (within-eps ad ed)
           (within-eps atx etx)
           (within-eps aty ety))))

  (check-= (transform-point (rotation-transformation 0) +i) +i eps)
  (check-= (transform-point (rotation-transformation 90) +i) -1 eps)
  (check-= (transform-point (rotation-transformation 180) +i) -i eps)
  (check-= (transform-point (rotation-transformation 270) +i) 1 eps)
  (check-= (transform-point (rotation-transformation -90) +i) 1 eps)
  (check-= (transform-point (rotation-transformation 360) +i) +i eps)

  (check-= (transform-point (rotation-transformation 0) 1) 1 eps)
  (check-= (transform-point (rotation-transformation 90) 1) +i eps)
  (check-= (transform-point (rotation-transformation 180) 1) -1 eps)
  (check-= (transform-point (rotation-transformation 270) 1) -i eps)
  (check-= (transform-point (rotation-transformation -90) 1) -i eps)
  (check-= (transform-point (rotation-transformation 360) 1) 1 eps)

  (check-= (transform-point (rotation-transformation -45) 1) (make-rectangular invrt2 (- invrt2)) eps)
  (check-= (transform-point (rotation-transformation 45) 1) (make-rectangular invrt2 invrt2) eps)
  (check-= (transform-point (rotation-transformation 135) 1) (make-rectangular (- invrt2) invrt2) eps)

  (check-= (transform-point (stretching-transformation 2) 1) 2 eps)
  (check-= (transform-point (stretching-transformation 2) +i) +2i eps)
  (check-= (transform-point (stretching-transformation 2) 1+i) 2+2i eps)

  (check-= (transform-point (compose-transformation (translation-transformation 0 2)
                                                    (rotation-transformation 45))
                            1)
           (make-rectangular invrt2 (+ invrt2 2))
           eps)

  (check-= (transform-point (compose-transformation (rotation-transformation 45)
                                                    (translation-transformation 0 2))
                            1)
           -0.7071067811865474+2.121320343559643i
           eps)

  (check-= (transform-point (invert-transformation
                             (compose-transformation (rotation-transformation 45)
                                                     (translation-transformation 0 2)))
                            -0.7071067811865474+2.121320343559643i)
           1
           eps)

  (check-transformation~? (compose-transformation (rotation-transformation -90)
                                                  (translation-transformation 0 2)
                                                  (rotation-transformation 90))
                          (translation-transformation 2 0))

  (check-transformation~? (compose-transformation (rotation-transformation -45)
                                                  (translation-transformation 0 (* 2 (sqrt 2)))
                                                  (rotation-transformation 45))
                          (translation-transformation 2 2))

  ;; Cairo's drawing model has *device coordinates* and *user
  ;; coordinates*. In the Cairo tutorial, we are given the task of
  ;; mapping a 1.0x1.0 workspace onto the 100x100 pixel square in the
  ;; middle of a 120x120 pixel surface, and shown three different ways
  ;; of achieving this:
  ;;
  ;;  - cairo_translate (cr, 10, 10); cairo_scale (cr, 100, 100);
  ;;
  ;;  - cairo_scale (cr, 100, 100); cairo_translate (cr, 0.1, 0.1);
  ;;
  ;;  - cairo_matrix_t mat; cairo_matrix_init (&mat, 100, 0, 0, 100, 10, 10);
  ;;    cairo_transform (cr, &mat);
  ;;
  ;; Let's see what those look like here. We'll assume a right-handed
  ;; coordinate system for both the workspace and the surface, so we
  ;; can judge a correct outcome by seeing that (0,0) on the workspace
  ;; should map to (10,10) on the surface, that (1,1) on the workspace
  ;; should map to (110,110), and that the other two corners should
  ;; map correspondingly.

  (let ()
    (define (apply-to-inputs m)
      (map (lambda (v) (transform-point m v))
           (list 0 1 1+i +i)))

    (define expected-outputs (list 10+10i 110+10i 110+110i 10+110i))

    (define-binary-check (check-list~? actual expected)
      (andmap within-eps actual expected))

    (check-list~? (apply-to-inputs (compose-transformation (translation-transformation 10 10)
                                                           (stretching-transformation 100 100)))
                  expected-outputs)
    (check-list~? (apply-to-inputs (compose-transformation (stretching-transformation 100 100)
                                                           (translation-transformation 0.1 0.1)))
                  expected-outputs)
    (check-list~? (apply-to-inputs (transformation-matrix 100 0 0 100 10 10))
                  expected-outputs))

  ;; The Cairo tutorial also makes this note regarding line widths:
  ;; "While you're operating under a scale, the width of your line is
  ;; multiplied by that scale." That is, in Cairo, you reason about
  ;; line widths in user coordinates, just as with everything else.

  (let* ((m (transformation-matrix 100 0 0 100 10 10)))
    ;; transform-vector is analogous to Cairo's
    ;; "cairo_user_to_device_distance" function.
    (check-= (transform-vector m 0.01+0.01i) 1+i eps)
    ;; untransform-vector is analogous to Cairo's
    ;; "cairo_device_to_user_distance" function.
    (check-= (untransform-vector m 1+i) 0.01+0.01i eps))

  )
