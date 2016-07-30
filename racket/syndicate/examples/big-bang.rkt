#lang syndicate

(require "../big-bang.rkt")
(require racket/math)

(define (button #:background [background "grey"]
                #:foreground [foreground "white"]
                #:font-size [font-size 22]
                name x y label callback)
  (spawn (lambda (e s)
           (match e
             [(message (inbound (mouse-event _ _ _ "button-down"))) (transition s (callback))]
             [_ #f]))
         (void)
         (patch-seq
          (let ((label-image (text label font-size foreground)))
            (update-window name x y
                           (overlay label-image
                                    (rectangle (+ (image-width label-image) 20)
                                               (+ (image-height label-image) 20)
                                               "solid"
                                               background))))
          (sub (inbound (mouse-event ? ? name ?))))))

(define (draggable-shape name orig-x orig-y image)
  (struct idle (ticks x y) #:transparent)
  (struct dragging (dx dy) #:transparent)
  (define (move-to x y) (update-window name x y image #:z 10))
  (define (mouse-sub active-pat)
    (patch-seq (unsub (inbound (mouse-event ? ? ? ?)))
               (sub (inbound (mouse-event ? ? active-pat ?)))))
  (spawn (match-lambda**
          [((message (inbound (tick-event))) (idle ticks bx by))
           (define new-ticks (+ ticks 1))
           (define displacement (* (cos (* new-ticks 10 1/180 pi)) 4))
           (define new-y (+ by displacement))
           (transition (idle new-ticks bx new-y)
                       (move-to bx new-y))]
          [((message (inbound (mouse-event mx my _ "button-down"))) (idle _ bx by))
           (transition (dragging (- mx bx) (- my by)) (mouse-sub ?))]
          [((message (inbound (mouse-event mx my _ "drag"))) (dragging dx dy))
           (transition (dragging dx dy) (move-to (- mx dx) (- my dy)))]
          [((message (inbound (mouse-event mx my _ (or "leave" "button-up")))) (dragging dx dy))
           (transition (idle 0 (- mx dx) (- my dy))
                       (list (move-to (- mx dx) (- my dy))
                             (mouse-sub name)))]
          [(_ _) #f])
         (idle 0 orig-x orig-y)
         (patch-seq (sub (inbound (tick-event)))
                    (mouse-sub name)
                    (move-to orig-x orig-y))))

(spawn (lambda (e s)
         (match e
           [(? patch? p)
            (define-values (in out)
              (patch-project/set/single p (inbound (?! (active-window ?)))))
            (transition s (update-window 'active-window-label 300 0
                                         (text (format "~v" in) 22 "black")))]
           [_ #f]))
       (void)
       (sub (inbound (active-window ?))))
(button #:background "red" 'stop-button 0 0 "Exit"
        (lambda () (assert (outbound 'stop))))
(draggable-shape 'c1 50 50 (circle 30 "solid" "orange"))
(draggable-shape 's1 100 100 (star 40 "solid" "firebrick"))

(module+ main
  (current-ground-dataspace
   (big-bang-dataspace #:width 640
                       #:height 480)))
