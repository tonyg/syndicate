#lang racket

(require prospect)
(require 2htdp/image)
(require "../2d.rkt")

(define window-projection (compile-projection (at-meta (?! (window ? ?)))))

(2d-world (spawn (lambda (e s)
                   (match e
                     [(? patch? p)
                      (define-values (added removed) (patch-project/set/single p window-projection))
                      (transition s (for/list [(w added)]
                                      (match-define (window width height) w)
                                      (update-scene `((push-matrix (scale ,width ,height)
                                                                   (texture ,(rectangle 1 1 "solid" "white"))))
                                                    `())))]
                     [_ #f]))
                 (void)
                 (sub (window ? ?) #:meta-level 1)
                 ;; (assert 'fullscreen #:meta-level 1)
                 )
          (let ((move-to (lambda (x y)
                           (transition (list x y)
                                       (update-sprites (simple-sprite 0 x y 10 10 (rectangle 1 1 "solid" "blue")))))))
            (spawn (lambda (e s)
                     (match-define (list x y) s)
                     (match e
                       [(message (at-meta (key-event 'left _))) (move-to (- x 2) y)]
                       [(message (at-meta (key-event 'right _))) (move-to (+ x 2) y)]
                       [(message (at-meta (key-event 'up _))) (move-to x (- y 2))]
                       [(message (at-meta (key-event 'down _))) (move-to x (+ y 2))]
                       [_ #f]))
                   (list 100 100)
                   (update-sprites (simple-sprite -0.5 100 100 10 10 (rectangle 1 1 "solid" "blue")))
                   (sub (key-event ? ?) #:meta-level 1)))
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
                 (sub (key-event #\q ?) #:meta-level 1))
          )
(exit 0)
