#lang racket/gui

(provide texture%)

(require sgl/gl)
(require sgl/gl-vectors)

(define texture%
  (class object%
    (init [(initial-bitmap bitmap)])
    (field [width 0]
           [height 0]
           [textures #f])

    (define/public (get-width) width)
    (define/public (get-height) height)

    (define/public (bind-texture)
      (when (not textures) (error 'bind-texture "Attempt to use disposed texture%"))
      (glBindTexture GL_TEXTURE_2D (gl-vector-ref textures 0)))

    (define/public (load-from-bitmap! bitmap)
      (when textures (dispose))
      (set! textures (glGenTextures 1))
      (bind-texture)
      (define image-data
        (let ()
          (set! width (send bitmap get-width))
          (set! height (send bitmap get-height))
          (define dc (new bitmap-dc% [bitmap bitmap]))
          (define pixels (* width height))
          (define vec (make-gl-ubyte-vector (* pixels 4)))
          (define data (make-bytes (* pixels 4)))
          (send dc get-argb-pixels 0 0 width height data #f #t) ;; premultiplied
          (for ((i (in-range pixels)))
            (for ((j (in-range 4)))
              (gl-vector-set! vec (+ (* i 4) j) (bytes-ref data (+ (* i 4) (- 3 j))))))
          vec))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (glTexImage2D GL_TEXTURE_2D 0 4 width height 0 GL_BGRA GL_UNSIGNED_BYTE image-data))

    (define/public (dispose)
      (when textures
        (glDeleteTextures textures)
        (set! textures #f)))

    (super-new)
    (load-from-bitmap! initial-bitmap)))
