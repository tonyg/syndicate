#lang racket

(require syndicate)
(require 2htdp/image)
(require "../2d.rkt")

(define window-projection (at-meta (?! (window ? ?))))
(define key-pressed-projection (key-pressed (?!)))

(define (spawn-background)
  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (define-values (added removed) (patch-project/set/single p window-projection))
              (transition s (for/list [(w added)]
                              (match-define (window width height) w)
                              (update-scene `((push-matrix (scale ,width ,(* height 2))
                                                           (translate 0 -0.25)
                                                           (texture
                                                            ,(overlay/xy (rectangle 1 1 "solid" "white")
                                                                         0 0
                                                                         (rectangle 1 2 "solid" "black"))))
                                              ;; (rotate -30)
                                              ;; (scale 5 5)
                                              )
                                            `())))]
             [_ #f]))
         (void)
         (patch-seq
          (sub (window ? ?) #:meta-level 1)
          ;; (assert 'fullscreen #:meta-level 1)
          )))

(define (spawn-player-avatar)
  (local-require 2htdp/planetcute)
  (define CC character-cat-girl)
  (define (move-to x y keys-down)
    (transition (list x y keys-down)
                (update-sprites
                 (simple-sprite 0 x y (image-width CC) (image-height CC) CC))))
  (spawn (lambda (e s)
           (match-define (list x y keys-down) s)
           (match e
             [(? patch? p)
              (define-values (added removed)
                (patch-project/set/single p key-pressed-projection))
              (define new-keys-down (set-subtract (set-union keys-down added) removed))
              (transition (list x y new-keys-down) '())]
             [(message (at-meta (frame-event _ _ elapsed-ms _)))
              (define-values (old-x old-y) (values x y))
              (define distance (* 0.360 elapsed-ms))
              (let* ((x (if (set-member? keys-down 'left) (- x distance) x))
                     (x (if (set-member? keys-down 'right) (+ x distance) x))
                     (y (if (set-member? keys-down 'up) (- y distance) y))
                     (y (if (set-member? keys-down 'down) (+ y distance) y)))
                (and (not (and (= x old-x) (= y old-y)))
                     (move-to x y keys-down)))]
             [_ #f]))
         (list 100 100 (set))
         (patch-seq
          (update-sprites
           (simple-sprite -0.5 100 100 (image-width CC) (image-height CC) CC))
          (sub (frame-event ? ? ? ?) #:meta-level 1)
          (sub (key-pressed ?)))))

(define (spawn-frame-counter)
  (spawn (lambda (e s)
           (match e
             [(message (at-meta (frame-event counter sim-time-ms _ _)))
              (and (> sim-time-ms 0)
                   (let ((i (text (format "~a fps" (/ counter (/ sim-time-ms 1000.0))) 22 "black")))
                     (transition s (update-sprites (simple-sprite -10 300 10
                                                                  (image-width i)
                                                                  (image-height i)
                                                                  i)))))]
             [_ #f]))
         (void)
         (sub (frame-event ? ? ? ?) #:meta-level 1)))

(2d-network (spawn-keyboard-integrator)
            (spawn-background)
            ;; (spawn-frame-counter)
            (spawn-player-avatar)
            (spawn (lambda (e s) #f)
                   (void)
                   (update-sprites (simple-sprite 0 50 50 50 50 (circle 50 "solid" "orange"))
                                   (simple-sprite -1 60 60 50 50 (circle 50 "solid" "green"))))
            (spawn (lambda (e s)
                     (match e
                       [(message _)
                        (transition s (assert 'stop #:meta-level 1))]
                       [_ #f]))
                   (void)
                   (sub (key-event #\q #t ?) #:meta-level 1))
            )
(exit 0)
