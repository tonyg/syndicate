#lang syndicate

;; actor adapter for canvas-double-click% and cells-canvas%
(require 7GUI/canvas-double-click)
(require 7GUI/task-7-view)
(require (only-in "../../widgets.rkt" qc))

(provide spawn-cells-canvas
         (struct-out single-click)
         (struct-out double-click)
         (struct-out update-grid))

(require racket/gui/base
         (except-in racket/class field))

(message-struct single-click (x y))
(message-struct double-click (x y))
(message-struct update-grid (cells))

;; ---------------------------------------------------------------------------------------------------
(define cells-canvas%
  (class canvas-double-click%
    (define/augment-final (on-click x y) (send-ground-message (single-click x y)))
    (define/augment-final (on-double-click x y) (send-ground-message (double-click x y)))
    (define *content #f)
    (define/public (update-grid cells)
      (set! *content cells)
      (qc (define dc (send this get-dc))
          (paint-grid dc *content)))
    (super-new [paint-callback (lambda (_self dc) (when *content (paint-grid dc *content)))])))

(define (spawn-cells-canvas parent width height)
  (define parent-component (seal-contents parent))
  (define canvas (new cells-canvas% [parent parent-component] [style '(hscroll vscroll)]))
  (qc (send canvas init-auto-scrollbars width height 0. 0.)
      (send canvas show-scrollbars #t #t))

  (spawn
   (on (message (update-grid $cells))
       (qc (send canvas update-grid cells)))
   (on (message (inbound (single-click $x $y)))
       (send! (single-click x y)))
   (on (message (inbound (double-click $x $y)))
       (send! (double-click x y)))))
