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
          (spawn (lambda (e s) #f)
                 (void)
                 (update-sprites (sprite 0 `((translate 50 50)
                                             (scale 50 50)
                                             (texture ,(circle 50 "solid" "orange"))
                                             ))
                                 (sprite -1 `((translate 60 60)
                                              (scale 50 50)
                                              (texture ,(circle 50 "solid" "green"))
                                              ))))
          (spawn (lambda (e s)
                   (match e
                     [(message _)
                      (transition s (assert 'stop #:meta-level 1))]
                     [_ #f]))
                 (void)
                 (sub (key-event #\q ?) #:meta-level 1))
          )
(exit 0)
