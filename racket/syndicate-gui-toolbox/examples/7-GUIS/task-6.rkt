#lang syndicate

(require "../../widgets.rkt")
(require racket/list
         racket/gui/base
         (except-in racket/class field))

;; a circle drawer with undo/redo facilities (unclear spec for resizing)

(message-struct circle-canvas-event (type x y))
(message-struct resize (circ d))
(message-struct draw-circles (closest others))

;; ---------------------------------------------------------------------------------------------------
(define Default-Diameter 20)

(struct circle (x y d action) #:transparent)

(define (draw-1-circle dc brush c)
  (match-define (circle x y d _a) c)
  (send dc set-brush brush)
  (define r (/ d 2))
  (send dc draw-ellipse (- x r) (- y r) d d))


;; N N (Circle -> Real]
(define ((distance xm ym) c)
  (match-define (circle xc yc _d _a) c)
  (sqrt (+ (expt (- xc xm) 2) (expt (- yc ym) 2))))

;; ---------------------------------------------------------------------------------------------------
(define solid-gray  (new brush% [color "gray"]))
(define white-brush (new brush% [color "white"]))

(define circle-canvas%
  (class canvas%
    (inherit on-paint get-dc)

    (define/override (on-event evt)
      (define type (send evt get-event-type))
      (define x (send evt get-x))
      (define y (send evt get-y))
      (send-ground-message (circle-canvas-event type x y)))

    (define (paint-callback _self _evt)
      (draw-circles *last-closest *last-others))

    (define *last-closest #f)
    (define *last-others #f)

    (define/public (draw-circles closest (others-without-closest #f))
      (set! *last-closest closest)
      (set! *last-others others-without-closest)
      (define dc (get-dc))
      (send dc clear)
      (when others-without-closest
        (for ((c others-without-closest)) (draw-1-circle dc white-brush c)))
      (when closest (draw-1-circle dc solid-gray closest)))

    (super-new [paint-callback paint-callback])))

(define (spawn-circle-canvas parent frame undo-but redo-but)
  (define cc (new circle-canvas% [parent (seal-contents parent)][style '(border)]))

  (spawn
   (field [*circles '()]
          [*history '()]
          [*x 0]
          [*y 0]
          [*in-adjuster #f])

   (define (add-circle! x y)
     (define added (circle x y Default-Diameter 'added))
     (*circles (cons added (*circles))))

   (define (resize! old-closest new-d)
     (match-define (circle x y d a) old-closest)
     (define resized
       (match a
         ['added (circle x y new-d `(resized (,d)))]
         [`(resized . ,old-sizes) (circle x y new-d `(resized ,(cons d old-sizes)))]))
     (*circles (cons resized (remq old-closest (*circles)))))

   (define (undo)
     (when (cons? (*circles))
       (define fst (first (*circles)))
       (match fst
         [(circle x y d 'added) (*circles (rest (*circles)))]
         [(circle x y d `(resized (,r0 . ,sizes)))
          (*circles (cons (circle x y r0 `(resized (,d))) (rest (*circles))))])
       (*history (cons fst (*history)))))

   (define (redo)
     (when (cons? (*history))
       (define fst (first (*history)))
       (if (eq? (circle-action fst) 'added)
           (begin (*circles (cons fst (*circles))) (*history (rest (*history))))
           (begin (*circles (cons fst (rest (*circles)))) (*history (rest (*history)))))))

   (define (the-closest xm ym (circles (*circles)))
     (define cdistance (distance xm ym))
     (define-values (good-circles distance*)
       (for*/fold ([good-circles '()][distance* '()])
                  ((c circles) (d (in-value (cdistance c))) #:when (< d (/ (circle-d c) 2)))
         (values (cons c good-circles) (cons d distance*))))
     (and (cons? distance*) (first (argmin second (map list good-circles distance*)))))

   (define (is-empty-area xm ym (circles (*circles)))
     (define dist (distance xm ym))
     (for/and ((c circles)) (> (dist c) (/ (+ (circle-d c) Default-Diameter) 2))))

   (on (message 'unlock-canvas) (*in-adjuster #f))
   (on (message 'lock-canvas) (*in-adjuster #t))

   ;; no closest
   (define (draw!)
     (send cc draw-circles #f (*circles)))

   (on (message (resize $old-closest $new-d))
       (resize! old-closest new-d)
       (draw!))

   (on (message (draw-circles $close $others))
       (send cc draw-circles close others))

   (on (message (button-press undo-but))
       (undo)
       (draw!))

   (on (message (button-press redo-but))
       (redo)
       (draw!))

   (on (message (inbound (circle-canvas-event $type $x $y)))
       (unless (*in-adjuster)
         (*x x)
         (*y y)
         (cond
           [(eq? 'leave type) (*x #f)]
           [(eq? 'enter type) (*x 0)]
           [(and (eq? 'left-down type) (is-empty-area (*x) (*y)))
            (add-circle! (*x) (*y))
            (draw!)]
           [(and (eq? 'right-down type) (the-closest (*x) (*y)))
            => (λ (tc)
                 (*in-adjuster #t)
                 (popup-adjuster tc *circles frame)
                 (send cc draw-circles tc (*circles)))])))
   ))

(define (popup-adjuster closest-circle *circles frame)
  (define pid (gensym 'popup))
  (send! (popup-menu frame pid "adjuster" 100 100 (list "adjust radius")))
  (react (stop-when (message (no-popdown-selected pid)) (send! 'unlock-canvas))
         (stop-when (message (popdown-item-selected pid _)) (adjuster! closest-circle *circles))))

(define (adjuster! closest-circle *circles)
  (define d0 (circle-d closest-circle))
  (define frame (spawn-adjuster-dialog closest-circle (remq closest-circle (*circles))))
  (spawn-adjuster-slider #:parent frame #:init-value d0))

(define adjuster-dialog%
  (class frame% (init-field closest-circle)
    (match-define (circle x* y* _d _a) closest-circle)

    (define/augment (on-close)
      (send-ground-message 'adjuster-closed))

    (super-new [label (format "Adjust radius of circle at (~a,~a)" x* y*)])))

(define (spawn-adjuster-dialog closest-circle others)
  (match-define (circle x* y* old-d _a) closest-circle)
  (define dialog
    (parameterize ((current-eventspace (make-eventspace)))
      (new adjuster-dialog% [closest-circle closest-circle])))
  (send dialog show #t)
  (spawn
   ;; well, there's only one slider
   (define/query-value *d old-d (slider@ _ $v) v)
   (on (message (slider-update _ $v))
       ;; resize locally while adjusting
       (send! (draw-circles (circle x* y* (*d) '_dummy_) others)))
   (on (message (inbound 'adjuster-closed))
       ;; resize globally
       (send! 'unlock-canvas)
       (send! (resize closest-circle (*d)))
       (stop-current-facet)))
  (seal dialog))


(define (spawn-adjuster-slider #:parent parent
                               #:init-value init-value)
  (spawn-slider #:parent parent #:label "" #:min-value 10 #:max-value 100 #:init-value init-value))

;; ---------------------------------------------------------------------------------------------------
(spawn
(define frame  (spawn-frame #:label "Circle Drawer" #:width 400))
(define hpane1 (spawn-horizontal-pane #:parent frame #:min-height 20 #:alignment '(center center)))
(define undo-but (spawn-button #:label "Undo" #:parent hpane1))
(define redo-but (spawn-button #:label "Redo" #:parent hpane1))
(define hpane2 (spawn-horizontal-panel #:parent frame #:min-height 400 #:alignment '(center center)))
(define canvas (spawn-circle-canvas hpane2 frame undo-but redo-but))

(on (asserted (frame@ frame))
    (send! (show frame #t)))
)
