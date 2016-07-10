#lang racket

(require (only-in syndicate seal))
(require syndicate/actor)
(require 2htdp/image)
(require "../2d.rkt")

(define (spawn-background)
  (actor
   (react
    (during (window $width $height) #:meta-level 1
            (assert (scene (seal `((push-matrix (scale ,width ,(* height 2))
                                                (translate 0 -0.25)
                                                (texture
                                                 ,(overlay/xy (rectangle 1 1 "solid" "white")
                                                              0 0
                                                              (rectangle 1 2 "solid" "black"))))
                                   ;; (rotate -30)
                                   ;; (scale 5 5)
                                   ))
                           (seal `()))
                    #:meta-level 1)))))

(define (spawn-player-avatar)
  (local-require 2htdp/planetcute)
  (define CC character-cat-girl)

  (actor (react
          (field [x 100] [y 100])
          (assert (simple-sprite -0.5 (x) (y) (image-width CC) (image-height CC) CC)
                  #:meta-level 1)

          (field [keys-down (set)])
          (on (asserted (key-pressed $k)) (keys-down (set-add (keys-down) k)))
          (on (retracted (key-pressed $k)) (keys-down (set-remove (keys-down) k)))
          (define (key->delta k distance) (if (set-member? (keys-down) k) distance 0))

          (on (message (frame-event _ _ $elapsed-ms _) #:meta-level 1)
              (define-values (old-x old-y) (values (x) (y)))
              (define distance (* 0.360 elapsed-ms))
              (define nx (+ old-x (key->delta 'right distance) (key->delta 'left (- distance))))
              (define ny (+ old-y (key->delta 'down distance) (key->delta 'up (- distance))))
              (when (not (and (= nx old-x) (= ny old-y)))
                (x nx)
                (y ny))))))

(define (spawn-frame-counter)
  (actor (react (field [i empty-image])
                (assert (simple-sprite -10 300 10 (image-width (i)) (image-height (i)) (i))
                        #:meta-level 1)
                (on (message (frame-event $counter $sim-time-ms _ _) #:meta-level 1)
                    (when (> sim-time-ms 0)
                      (define fps (/ counter (/ sim-time-ms 1000.0)))
                      (i (text (format "~a fps" fps) 22 "black")))))))

(2d-dataspace (spawn-keyboard-integrator)
              (spawn-background)
              ;; (spawn-frame-counter)
              (spawn-player-avatar)
              (actor (react (assert (simple-sprite 0 50 50 50 50 (circle 50 "solid" "orange"))
                                    #:meta-level 1)
                            (assert (simple-sprite -1 60 60 50 50 (circle 50 "solid" "green"))
                                    #:meta-level 1)))
              (actor (until (message (key-event #\q #t _) #:meta-level 1))
                     (assert! 'stop #:meta-level 1)))
(exit 0)
