#lang syndicate
;; Multiple animated sprites.
;;
;; 2016-12-12 With the current implementations of (a) Syndicate's
;; dataspaces and (b) Syndicate's 2D sprite support, my reasonably new
;; 2.6GHz laptop can animate 20 logos at 256x256 pixels at about 20
;; frames per second on a single core.
;;
;; For comparison, Kay recounts in "The Early History of Smalltalk"
;; (1993) that "by the Fall of '73 [Steve Purcell] could demo 80
;; ping-pong balls and 10 flying horses running at 10 frames per
;; second in 2 1/2 D" in an early Smalltalk (?) on a NOVA.

(require 2htdp/image)
(require images/logos)
(require "../2d.rkt")

(define speed-limit 40)
(define sprite-count 20)

(define (spawn-background)
  (spawn
   (during (inbound (window $width $height))
           (assert (outbound
                    (scene (seal `((push-matrix (scale ,width ,height)
                                                (texture ,(rectangle 1 1 "solid" "white")))))
                           (seal `())))))))

(define i:logo (plt-logo))

(define (spawn-logo)
  (spawn (field [x 100] [y 100])
         (field [dx (* (- (random) 0.5) speed-limit)]
                [dy (* (- (random) 0.5) speed-limit)])
         (define/query-value w #f (inbound ($ w (window _ _))) w)
         (assert (outbound (simple-sprite 0
                                          (x)
                                          (y)
                                          (image-width i:logo)
                                          (image-height i:logo)
                                          i:logo)))
         (define (bounce f df limit)
           (define v (f))
           (define limit* (- limit (image-width i:logo)))
           (cond [(< v 0) (f 0) (df (abs (df)))]
                 [(> v limit*) (f limit*) (df (- (abs (df))))]
                 [else (void)]))
         (on (message (inbound (frame-event _ _ _ _)))
             (when (w) ;; don't animate until we know the window bounds
               (x (+ (x) (dx)))
               (y (+ (y) (dy)))
               (bounce x dx (window-width (w)))
               (bounce y dy (window-height (w)))))))

(spawn-background)
(for [(i sprite-count)]
  (spawn-logo))

(spawn (on (message (inbound (frame-event $counter $timestamp _ _)))
           (when (and (zero? (modulo counter 100)) (positive? timestamp))
             (log-info "~v frames, ~v ms ==> ~v Hz"
                       counter
                       timestamp
                       (/ counter (/ timestamp 1000.0))))))

(spawn* (assert! (outbound 'fullscreen))
        (until (message (inbound (key-event #\q #t _))))
        (assert! (outbound 'stop)))

(module+ main (current-ground-dataspace (2d-dataspace)))
