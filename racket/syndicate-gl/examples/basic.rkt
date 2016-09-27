#lang syndicate/actor

(require racket/set)
(require 2htdp/image)
(require "../2d.rkt")

(define (spawn-background)
  (actor
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
                           (seal `())))))))

(define (draggable-mixin touching? x y)
  (define (idle)
    (react (stop-when #:when (touching?)
                      (message (inbound (mouse-event 'left-down (mouse-state $mx $my _ _ _))))
                      (dragging (- mx (x)) (- my (y))))))

  (define (dragging dx dy)
    (react (on (message (inbound (mouse-event 'motion (mouse-state $mx $my _ _ _))))
               (x (- mx dx))
               (y (- my dy)))
           (stop-when (message (inbound (mouse-event 'left-up _))) (idle))
           (stop-when (message (inbound (mouse-event 'leave _))) (idle))))

  (idle))

(define (draggable-shape name orig-x orig-y z plain-image hover-image
                         #:coordinate-map-id [coordinate-map-id #f]
                         #:parent [parent-id #f])
  (actor (field [x orig-x] [y orig-y])
         (define/query-value touching? #f (inbound (touching name)) #t)
         (assert (outbound (simple-sprite #:parent parent-id
                                          #:coordinate-map-id coordinate-map-id
                                          #:touchable-id name
                                          #:touchable-predicate in-unit-circle?
                                          z (x) (y) 50 50
                                          (if (touching?)
                                              hover-image
                                              plain-image))))
         (on-start (draggable-mixin touching? x y))))

(define (tooltip touching? x y w h label-string)
  (define label-text (text label-string 22 "black"))
  (define label (overlay label-text (empty-scene (+ (image-width label-text) 10)
                                                 (+ (image-height label-text) 10))))
  (define (pos)
    (define v (- (x) (image-width label) 10))
    (if (negative? v)
        (+ (x) w 10)
        v))
  (react (assert #:when (touching?)
                 (outbound (simple-sprite -10
                                          (pos)
                                          (+ (y) (* 1/2 h) (- (* 1/2 (image-height label))))
                                          (image-width label)
                                          (image-height label)
                                          label)))))

(define (spawn-player-avatar)
  (local-require 2htdp/planetcute)
  (define CC character-cat-girl)

  (actor (field [x 100] [y 100])
         (assert (outbound (simple-sprite #:touchable-id 'player
                                          #:coordinate-map-id 'player
                                          -0.5 (x) (y) (image-width CC) (image-height CC) CC)))

         (field [keys-down (set)])
         (on (asserted (key-pressed $k)) (keys-down (set-add (keys-down) k)))
         (on (retracted (key-pressed $k)) (keys-down (set-remove (keys-down) k)))
         (define (key->delta k distance) (if (set-member? (keys-down) k) distance 0))

         (define/query-value touching? #f (inbound (touching 'player)) #t)
         (on-start (draggable-mixin touching? x y))

         (on (asserted (inbound (coordinate-map 'player $xform)))
             ;; TODO: figure out why this causes lag in frame updates
             (log-info "Player coordinate map: ~v" xform))

         (on-start (tooltip touching? x y (image-width CC) (image-height CC) "The Player"))

         (on (message (inbound (frame-event _ _ $elapsed-ms _)))
             (define-values (old-x old-y) (values (x) (y)))
             (define distance (* 0.360 elapsed-ms))
             (define nx (+ old-x (key->delta 'right distance) (key->delta 'left (- distance))))
             (define ny (+ old-y (key->delta 'down distance) (key->delta 'up (- distance))))
             (when (not (and (= nx old-x) (= ny old-y)))
               (x nx)
               (y ny)))))

(define (spawn-frame-counter)
  (actor (field [i empty-image])
         (assert (outbound
                  (simple-sprite -10 300 10 (image-width (i)) (image-height (i)) (i))))
         (on (message (inbound (frame-event $counter $sim-time-ms _ _)))
             (when (> sim-time-ms 0)
               (define fps (/ counter (/ sim-time-ms 1000.0)))
               (i (text (format "~a fps" fps) 22 "black"))))))

(spawn-keyboard-integrator)
(spawn-mouse-integrator)
(spawn-background)
;; (spawn-frame-counter)
(spawn-player-avatar)

(draggable-shape 'orange 50 50 0
                 (circle 50 "solid" "orange")
                 (circle 50 "solid" "red"))

(draggable-shape 'yellow 10 -10 0 #:parent 'orange
                 (circle 50 "solid" "yellow")
                 (circle 50 "solid" "purple"))

(draggable-shape 'green 60 60 -1
                 (circle 50 "solid" "green")
                 (circle 50 "solid" "cyan"))

(actor* (until (message (inbound (key-event #\q #t _))))
        (assert! (outbound 'stop)))

(actor (during (inbound (touching $id))
               (on-start (log-info "Touching ~v" id))
               (on-stop (log-info "No longer touching ~v" id))))

(module+ main (current-ground-dataspace (2d-dataspace)))
