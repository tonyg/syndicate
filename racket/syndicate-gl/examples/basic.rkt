#lang syndicate/actor

(require racket/set)
(require 2htdp/image)
(require "../2d.rkt")

(define (spawn-background)
  (actor
   (react
    (during (inbound (window $width $height))
            (assert (outbound
                     (scene (seal `((push-matrix (scale ,width ,(* height 2))
                                                 (translate 0 -0.25)
                                                 (texture
                                                  ,(overlay/xy (rectangle 1 1 "solid" "white")
                                                               0 0
                                                               (rectangle 1 2 "solid" "black"))))
                                    ;; (rotate -30)
                                    ;; (scale 5 5)
                                    ))
                            (seal `()))))))))

(define (spawn-player-avatar)
  (local-require 2htdp/planetcute)
  (define CC character-cat-girl)

  (actor (react
          (field [x 100] [y 100])
          (assert (outbound (simple-sprite -0.5 (x) (y) (image-width CC) (image-height CC) CC)))

          (field [keys-down (set)])
          (on (asserted (key-pressed $k)) (keys-down (set-add (keys-down) k)))
          (on (retracted (key-pressed $k)) (keys-down (set-remove (keys-down) k)))
          (define (key->delta k distance) (if (set-member? (keys-down) k) distance 0))

          (on (message (inbound (frame-event _ _ $elapsed-ms _)))
              (define-values (old-x old-y) (values (x) (y)))
              (define distance (* 0.360 elapsed-ms))
              (define nx (+ old-x (key->delta 'right distance) (key->delta 'left (- distance))))
              (define ny (+ old-y (key->delta 'down distance) (key->delta 'up (- distance))))
              (when (not (and (= nx old-x) (= ny old-y)))
                (x nx)
                (y ny))))))

(define (spawn-frame-counter)
  (actor (react (field [i empty-image])
                (assert (outbound
                         (simple-sprite -10 300 10 (image-width (i)) (image-height (i)) (i))))
                (on (message (inbound (frame-event $counter $sim-time-ms _ _)))
                    (when (> sim-time-ms 0)
                      (define fps (/ counter (/ sim-time-ms 1000.0)))
                      (i (text (format "~a fps" fps) 22 "black")))))))

(spawn-keyboard-integrator)
(spawn-background)
;; (spawn-frame-counter)
(spawn-player-avatar)
(actor (react (assert (outbound (simple-sprite 0 50 50 50 50 (circle 50 "solid" "orange"))))
              (assert (outbound (simple-sprite -1 60 60 50 50 (circle 50 "solid" "green"))))))
(actor (until (message (inbound (key-event #\q #t _))))
       (assert! (outbound 'stop)))

(module+ main (current-ground-dataspace (2d-dataspace)))
