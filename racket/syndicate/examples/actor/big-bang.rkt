#lang syndicate

(require syndicate/big-bang)
(require racket/math)

(define (button #:background [background "grey"]
                #:foreground [foreground "white"]
                #:font-size [font-size 22]
                name x y label callback)
  (define label-image (text label font-size foreground))
  (spawn (on (message (inbound (mouse-event _ _ name "button-down"))) (callback))
         (assert (outbound
                  (window name x y 0
                          (seal
                           (overlay label-image
                                    (rectangle (+ (image-width label-image) 20)
                                               (+ (image-height label-image) 20)
                                               "solid"
                                               background))))))))

(define (draggable-shape name orig-x orig-y image)
  (define (window-at x y) (window name x y 10 (seal image)))
  (define (mouse-left-event-type? t) (member t '("leave" "button-up")))
  (define (idle ticks0 x0 y0)
    (react (field [ticks ticks0] [x x0] [y y0])
           (assert (outbound (window-at (x) (y))))
           (on (message (inbound (tick-event)))
               (ticks (+ (ticks) 1))
               (y (+ (y) (* (cos (* (ticks) 10 1/180 pi)) 4))))
           (stop-when (message (inbound (mouse-event $mx $my name "button-down")))
                      (dragging mx my (- mx (x)) (- my (y))))))
  (define (dragging mx0 my0 dx dy)
    (react (field [mx mx0] [my my0])
           (assert (outbound (window-at (- (mx) dx) (- (my) dy))))
           (on (message (inbound (mouse-event $nmx $nmy _ "drag")))
               (mx nmx)
               (my nmy))
           (stop-when (message (inbound (mouse-event $mx $my _ (? mouse-left-event-type? $t))))
                      (idle 0 (- mx dx) (- my dy)))))
  (spawn* (idle 0 orig-x orig-y)))

(spawn (during (inbound (active-window $id))
               (assert (outbound (window 'active-window-label 300 0 0
                                         (seal (text (format "~v" id) 22 "black")))))))
(button #:background "red" 'stop-button 0 0 "Exit"
        (lambda () (assert! (outbound 'stop))))
(draggable-shape 'c1 50 50 (circle 30 "solid" "orange"))
(draggable-shape 's1 100 100 (star 40 "solid" "firebrick"))

(module+ main
  (current-ground-dataspace
   (big-bang-dataspace #:width 640
                       #:height 480)))
