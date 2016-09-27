#lang racket/gui

(provide (struct-out window)
         (struct-out frame-event)
         (struct-out key-event)
         (struct-out key-pressed)
         (struct-out mouse-event)
         (struct-out mouse-state)
         (struct-out touching)
         (struct-out coordinate-map)
         (struct-out scene)
         (except-out (struct-out sprite) sprite)
         (rename-out [sprite <sprite>] [make-sprite sprite])
         (struct-out request-gc)
         in-unit-circle?
         in-unit-square?
         simple-sprite
         update-scene
         update-sprites
         spawn-keyboard-integrator
         spawn-mouse-integrator
         2d-dataspace)

(require data/order)
(require data/splay-tree)
(require data/queue)
(require sgl/gl)
(require sgl/gl-vectors)

(require (prefix-in image: 2htdp/image))
(require (prefix-in pict: pict))

(require syndicate)
(require syndicate/trie)
(require syndicate/ground)

(require "texture.rkt")
(require "affine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Shared state maintained by dataspace. Describes current window dimensions.
(struct window (width height) #:transparent)

;; Message sent by dataspace. Describes frame about to be rendered.
(struct frame-event (counter timestamp elapsed-ms target-frame-rate) #:transparent)

;; Message sent by dataspace. Describes a key event. Key is a sealed
;; key-event%. `press?` is #t when the key is pressed (or
;; autorepeated!), and #f when it is released.
(struct key-event (code press? key) #:transparent)

;; Assertion. Indicates that the named key is held down. See role
;; KeyboardIntegrator and spawn-keyboard-integrator.
(struct key-pressed (code) #:transparent)

;; Message sent by dataspace. Describes a mouse event. State is a
;; MouseState.
(struct mouse-event (type state) #:transparent)

;; Assertion. Indicates that the mouse is in a particular state. See
;; role MouseIntegrator and spawn-mouse-integrator.
(struct mouse-state (x y left-down? middle-down? right-down?) #:transparent)

;; Assertion. Indicates that the mouse is touching a particular touchable.
(struct touching (id) #:transparent)

;; Assertion. Communicates aggregate device-to-user transformation
;; requested as part of sprite instruction sequences.
(struct coordinate-map (id matrix) #:transparent)

;; Shared state maintained by program. Prelude and postlude are to be
;; sealed instruction lists. It is an error to have more than exactly
;; one active such record at a given time.
(struct scene (prelude postlude) #:transparent)

;; A SpriteID is an equal?-comparable dataspace-unique value.

;; Shared state maintained by program. `id` is a SpriteID, and
;; `parent-id` is an (Option SpriteID); #f in `parent-id` means that
;; this sprite is a child of the root. Z is to be a number, negative
;; toward camera. Instructions to be a sealed instruction list.
(struct sprite (id parent-id z instructions) #:transparent)

;; Message. Requests that the OpenGL loop perform a major
;; garbage-collection while *pausing the simulation's real-time
;; correspondence*. This lets a GC take place without such severe
;; simulation glitches as happen when doing it in-world.
(struct request-gc () #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-scene prelude postlude #:meta-level [meta-level 1])
  (patch-seq (retract (outbound* meta-level (scene ? ?)))
             (assert (outbound* meta-level (scene (seal prelude) (seal postlude))))))

(define (make-sprite z instructions #:id [id #f] #:parent [parent-id #f])
  (sprite (or id (gensym 'sprite)) parent-id z (seal instructions)))

(define (in-unit-circle? x y)
  (<= (+ (sqr (- x 0.5)) (sqr (- y 0.5))) (sqr 0.5)))

(define (in-unit-square? x y)
  (and (<= 0 x 1)
       (<= 0 y 1)))

(define (simple-sprite z x y w h i
                       #:parent [parent-id #f]
                       #:rotation [rotation 0]
                       #:coordinate-map-id [coordinate-map-id #f]
                       #:touchable-id [touchable-id #f]
                       #:touchable-predicate [touchable-predicate in-unit-square?])
  (make-sprite #:id touchable-id
               #:parent parent-id
               z
               `((translate ,x ,y)
                 ,@(if (zero? rotation) `() `((rotate ,rotation)))
                 (push-matrix
                  (scale ,w ,h)
                  ,@(if touchable-id
                        `((touchable ,touchable-id ,touchable-predicate))
                        `())
                  (texture ,i))
                 ,@(if coordinate-map-id
                       `((coordinate-map ,coordinate-map-id))
                       `())
                 (render-children))))

(define (update-sprites #:meta-level [meta-level 1] . ss)
  (patch-seq* (cons (retract (outbound* meta-level (sprite ? ? ? ?)))
                    (map (lambda (s) (assert (outbound* meta-level s))) ss))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KeyboardIntegrator. Integrates key-events into key-pressed assertions.
(define (spawn-keyboard-integrator #:meta-level [meta-level 1])
  (spawn (lambda (e s)
           (match e
             [(message (inbound* meta-level (key-event code press? _)))
              (transition (void) ((if press? assert retract) (key-pressed code)))]
             [#f #f]))
         (void)
         (sub (inbound* meta-level (key-event ? ? ?)))))

;; MouseIntegrator. Integrates mouse-events into mouse-state assertions.
(define (spawn-mouse-integrator #:meta-level [meta-level 1])
  (define retract-state (retract (mouse-state ? ? ? ? ?)))
  (spawn (lambda (e s)
           (match e
             [(message (inbound* meta-level (mouse-event 'leave _)))
              (transition (void) retract-state)]
             [(message (inbound* meta-level (mouse-event type new-state)))
              (transition (void) (patch-seq retract-state (assert new-state)))]
             [#f #f]))
         (void)
         (sub (inbound* meta-level (mouse-event ? ?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Touchable is one of
;;
;; - (touchable Any TransformationMatrix (Number Number -> Boolean))
;;   Represents a composed device-to-user transformation, plus a
;;   predicate on user coordinates, and an ID to use when the
;;   predicate answers truthily.
;;
;; - (touchable-map)
;;   Represents the location in a sequence of touchables where the
;;   aggregate partial device-to-user transformation used when mapping
;;   along parent-child relationship edges in the sprite tree should
;;   be applied to child sprites.
;;
(struct touchable (id transformation predicate) #:transparent)
(struct touchable-map () #:transparent)

;; A Children is a (SplayTree Sprite CompiledInstructions), ordered
;; first by sprite-z, then sprite-id hash code, then
;; sprite-instructions hash-code.
;;
;; A ChildMap is a (Hash SpriteID Children), mapping sprite-id to the
;; children of that sprite.

;; (compiled-instructions (ChildMap SpriteID -> Void)
;;                        (Listof Touchable)
;;                        (Listof CoordinateMap)
;;                        (Listof Resource)
;;                        (Option TransformationMatrix))
;; A single compiled sprite. The resources and coordinate-maps aren't
;; in any particular order, but the touchables are: the leftmost
;; touchable is the first to check; that is, it is the *topmost*
;; touchable in this sprite. The child-xform, if present, is the
;; transformation needed to map between mouse coordinates and child
;; sprite space; if absent, no (render-children) instruction was found
;; in this sprite's render code.
(struct compiled-instructions (render-proc touchables coordinate-maps resources child-xform))

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define (compile-instructions instrs)
  (define touchables '())
  (define coordinate-maps '())
  (define resources '())
  (define child-xform #f)

  (define (instructions->racket-code instrs xform)
    (define-values (code-rev new-xform)
      (for/fold [(code-rev '()) (xform xform)] [(instr (in-list instrs))]
        (define-values (new-code new-xform) (instruction->racket-code instr xform))
        (values (cons new-code code-rev) new-xform)))
    (values (reverse code-rev) new-xform))

  (define (instruction->racket-code instr xform)
    (match instr
      [`(rotate ,(? number? deg))
       (values `(glRotated ,deg 0 0 -1)
               (compose-transformation xform (rotation-transformation deg)))]
      [`(scale ,(? number? x) ,(? number? y))
       (values `(glScaled ,x ,y 1)
               (compose-transformation xform (stretching-transformation x y)))]
      [`(translate ,(? number? x) ,(? number? y))
       (values `(glTranslated ,x ,y 0)
               (compose-transformation xform (translation-transformation x y)))]
      [`(color ,(? color-number? r) ,(? color-number? g) ,(? color-number? b) ,(? color-number? a))
       (values `(glColor4d ,r ,g ,b ,a) xform)]
      [`(texture ,i)
       (define entry (image->texture-cache-entry i))
       (set! resources (cons entry resources))
       (values `(draw-gl-face ,(send entry get-texture)) xform)]
      [`(texture ,i ,l ,t ,w ,h) #:when (andmap number? (list l t w h))
       (define entry (image->texture-cache-entry i))
       (set! resources (cons entry resources))
       (values `(draw-gl-face ,(send entry get-texture) ,l ,t ,w ,h) xform)]
      [`(touchable ,id ,predicate)
       (set! touchables (cons (touchable id xform predicate) touchables))
       (values `(void) xform)]
      [`(coordinate-map ,id)
       (set! coordinate-maps (cons (coordinate-map id xform) coordinate-maps))
       (values `(void) xform)]
      [`(push-matrix ,instr ...)
       (define-values (code _new-xform) (instructions->racket-code instr xform))
       (values `(begin (glPushMatrix) ,@code (glPopMatrix)) xform)]
      [`(begin ,instr ...)
       (define-values (code new-xform) (instructions->racket-code instr xform))
       (values `(begin ,@code (void)) new-xform)]
      [`(render-children) ;; we assume that there will only be one of these
       (set! child-xform xform)
       (set! touchables (cons (touchable-map) touchables))
       (values `(render-sprites! CHILDMAP SELF-ID) xform)]
      [other
       (error 'instruction->racket-code "unknown render instruction: ~v" other)]))

  (define-values (code final-transformation)
    (instruction->racket-code `(begin ,@instrs) identity-transformation))
  (define render-proc (eval `(lambda (CHILDMAP SELF-ID) ,code) ns))
  (compiled-instructions render-proc
                         touchables
                         coordinate-maps
                         resources
                         child-xform))

(define (compiled-instructions-dispose! i)
  (when i
    (for [(resource (compiled-instructions-resources i))]
      (send resource dispose))))

(define (color-number? n)
  (and (number? n)
       (<= 0.0 n 1.0)))

(define (image->bitmap i)
  (cond
    [(is-a? i bitmap%)
     i]
    [(image:image? i)
     (define w (max 1 (image:image-width i)))
     (define h (max 1 (image:image-height i)))
     (define bm (make-object bitmap% w h #f #t))
     (define dc (send bm make-dc))
     (send i draw dc
           0 0
           0 0
           w h
           0 0
           #f)
     bm]
    [(pict:pict? i)
     (pict:pict->bitmap i)]
    [else
     (error 'image->bitmap "unsupported image type ~v" i)]))

(define (image->texture-cache-entry i)
  (texture-cache-get i image->bitmap))

;; (define (lerp a b v) (+ (* v a) (* (- 1 v) b)))

(define (draw-gl-face texture [left 0] [top 0] [width 1] [height 1])
  (define bot (+ top height))
  (define right (+ left width))
  (send texture bind-texture)
  (glBegin GL_QUADS)
  (glNormal3d 0 0 -1)
  (glTexCoord2d left top)
  (glVertex3d 0 0 0)
  (glTexCoord2d right top)
  (glVertex3d 1 0 0)
  (glTexCoord2d right bot)
  (glVertex3d 1 1 0)
  (glTexCoord2d left bot)
  (glVertex3d 0 1 0)
  (glEnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-instructions (compile-instructions '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scene-projection (?! (scene ? ?)))
(define sprite-projection (?! (sprite ? ? ? ?)))

(define sprite-order
  (order 'sprite-order
         sprite?
         (lambda (a b) (and (equal? (sprite-id a) (sprite-id b))
                            (= (sprite-z a) (sprite-z b))
                            (eq? (sprite-instructions a)
                                 (sprite-instructions b))))
         (lambda (a b) (or (> (sprite-z a) (sprite-z b))
                           (and (= (sprite-z a) (sprite-z b))
                                (let ((a-id-code (equal-hash-code (sprite-id a)))
                                      (b-id-code (equal-hash-code (sprite-id b))))
                                  (or (< a-id-code b-id-code)
                                      (and (= a-id-code b-id-code)
                                           (< (eq-hash-code (sprite-instructions a))
                                              (eq-hash-code (sprite-instructions b)))))))))))

(define (remove-sprite! childmap s)
  (define sprites (hash-ref childmap (sprite-parent-id s) #f))
  (when sprites
    (compiled-instructions-dispose! (splay-tree-ref sprites s #f))
    (splay-tree-remove! sprites s)
    (when (dict-empty? sprites) (hash-remove! childmap (sprite-parent-id s)))))

(define (add-sprite! childmap s)
  (define sprites (hash-ref childmap (sprite-parent-id s)
                            (lambda ()
                              (define ss (make-splay-tree sprite-order))
                              (hash-set! childmap (sprite-parent-id s) ss)
                              ss)))
  (define instrs `((color 1 1 1 1)
                   (push-matrix ,@(seal-contents (sprite-instructions s)))))
  (define i (compile-instructions instrs))
  (splay-tree-set! sprites s i))

(define (for-each-child-sprite childmap id f)
  (define children (hash-ref childmap id #f))
  (let loop ((iter (and children (splay-tree-iterate-first children))))
    (when iter
      (define s (splay-tree-iterate-key children iter))
      (define ci (splay-tree-iterate-value children iter))
      (f s ci)
      (loop (splay-tree-iterate-next children iter)))))

(define (render-sprites! childmap self-id)
  (for-each-child-sprite childmap self-id
                         (lambda (s ci)
                           ((compiled-instructions-render-proc ci) childmap (sprite-id s)))))

(define (render-scene! prelude childmap postlude)
  ((compiled-instructions-render-proc prelude) childmap #f)
  (render-sprites! childmap #f)
  ((compiled-instructions-render-proc postlude) childmap #f))

(define (detect-touch prelude childmap postlude state)
  (and state
       (let ()
         (define x (mouse-state-x state))
         (define y (mouse-state-y state))
         (or (detect-touch* childmap #f postlude x y)
             (detect-sprites-touch childmap #f x y)
             (detect-touch* childmap #f prelude x y)))))

(define (detect-sprites-touch childmap self-id x y)
  (define sprites (hash-ref childmap self-id #f))
  (let loop ((iter (and sprites (splay-tree-iterate-greatest sprites))))
    (and iter
         (let ((s (splay-tree-iterate-key sprites iter)))
           (define ci (splay-tree-iterate-value sprites iter))
           (or (detect-touch* childmap (sprite-id s) ci x y)
               (loop (splay-tree-iterate-greatest/<? sprites s)))))))

(define (detect-touch* childmap self-id ci x y)
  (for/or [(t (in-list (compiled-instructions-touchables ci)))]
    (match t
      [(touchable id xform contains?)
       (define-values (ux uy) (untransform-point* xform x y))
       (and (contains? ux uy) (touching id))]
      [(touchable-map)
       (define xform (compiled-instructions-child-xform ci))
       (define-values (ux uy) (untransform-point* xform x y))
       (detect-sprites-touch childmap self-id ux uy)])))

(define (untransform-point* xform x y)
  (define p (untransform-point xform (make-rectangular x y)))
  (values (real-part p) (imag-part p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dataspace-canvas%
  (class canvas%
    (inherit refresh with-gl-context swap-gl-buffers)

    (init boot-actions)

    (define counter 0)
    (define start-time (current-inexact-milliseconds))
    (define prev-frame-time start-time)
    (define/public (sim-time)
      (- (current-inexact-milliseconds) start-time))

    (define initialised? #f)

    (define near-depth 10) ;; 2.5D
    (define far-depth 15) ;; 2.5D

    (define prelude empty-instructions)
    (define childmap (make-hash))
    (define postlude empty-instructions)
    (define fullscreen? #f)

    (define current-mouse-state #f)
    (define current-touching #f)
    (define current-coordinate-maps (hash))

    (define-values (proc pending-transition)
      (spawn->process+transition (spawn-dataspace boot-actions)))
    (define event-queue (make-queue))

    (define target-frame-rate 60)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (sleep-and-refresh)
      (define target-sim-time (* counter (/ target-frame-rate)))
      (define sleep-time (- target-sim-time (/ (sim-time) 1000.0)))
      (when (positive? sleep-time)
        (sleep/yield sleep-time))
      (refresh))

    (define/public (set-target-frame-rate! r)
      (set! target-frame-rate r))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (inject-event! e)
      (enqueue! event-queue e))

    (define (deliver-event e)
      (clean-transition ((process-behavior proc) e (process-state proc))))

    (define (quiesce!)
      (define txn pending-transition)
      (set! pending-transition #f)
      (process-transition txn #t))

    (define (process-transition txn need-poll?)
      (match txn
        [#f ;; inert
         (if (queue-empty? event-queue)
             (when need-poll? (process-transition (deliver-event #f) #f))
             (process-transition (deliver-event (dequeue! event-queue)) #t))]
        [(transition new-state actions)
         (set! proc (update-process-state proc new-state))
         (for-each process-action! actions)
         (process-transition #f #t)]))

    (define (process-action! a)
      (match a
        [(? patch? p)
         (process-scene-updates! p)
         (process-sprite-updates! p)
         (process-stop-requests! p)
         (process-fullscreen-requests! p)]
        [(message (request-gc))
         (perform-gc-request!)]
        [(message _) (void)]))

    (define (process-scene-updates! p)
      (define-values (added removed) (patch-project/set/single p scene-projection))
      (when (not (set-empty? removed))
        (compiled-instructions-dispose! prelude)
        (compiled-instructions-dispose! postlude)
        (set! prelude empty-instructions)
        (set! postlude empty-instructions))
      (for [(s added)]
        (match-define (scene (seal pre) (seal post)) s)
        (set! prelude (compile-instructions pre))
        (set! postlude (compile-instructions post))))

    (define (process-sprite-updates! p)
      (define-values (added removed) (patch-project/set/single p sprite-projection))
      ;; Remove old sprites first, to recycle their texture identifiers (if any)
      (for [(s removed)] (remove-sprite! childmap s))
      (for [(s added)] (add-sprite! childmap s))
      (when (not (and (set-empty? added) (set-empty? removed)))
        (update-touching!)
        (update-coordinate-maps!))
      (flush-texture-cache!))

    (define (process-stop-requests! p)
      (when (trie-lookup (patch-added p) 'stop #f)
        (send (send this get-top-level-window) show #f)))

    (define (process-fullscreen-requests! p)
      (define changed? #f)
      (when (trie-lookup (patch-removed p) 'fullscreen #f)
        (set! changed? #t)
        (set! fullscreen? #f))
      (when (trie-lookup (patch-added p) 'fullscreen #f)
        (set! changed? #t)
        (set! fullscreen? #t))
      (when changed?
        (send (send this get-top-level-window) fullscreen fullscreen?)))

    (define (perform-gc-request!)
      (define pre-gc (current-inexact-milliseconds))
      (collect-garbage 'major)
      (define post-gc (current-inexact-milliseconds))
      (define delta (- post-gc pre-gc))
      (log-info "(request-gc) took ~a milliseconds" delta)
      (set! start-time (+ start-time delta)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (let* ((this-frame-time (sim-time))
                 (elapsed-ms (- this-frame-time prev-frame-time)))
            (when (not (negative? elapsed-ms))
              (inject-event! (message
                              (frame-event counter this-frame-time elapsed-ms target-frame-rate)))
              (set! counter (+ counter 1)))
            (set! prev-frame-time this-frame-time))
          (quiesce!)
          (unless initialised?
            (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA) ;; premultiplied
            (glEnable GL_BLEND)
            (glEnable GL_TEXTURE_2D)
            (glClearColor 0 0 0 1)
            (set! initialised? #t))
          (glClear GL_COLOR_BUFFER_BIT)
          (glLoadIdentity)
          (glTranslated 0 0 (- near-depth))
          (render-scene! prelude childmap postlude)
          (glFlush)
          (swap-gl-buffers)))
      (queue-callback (lambda () (sleep-and-refresh)) #f))

    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (inject-event! (patch-seq (retract (window ? ?))
                                    (assert (window width height))))
          (quiesce!)
          (glViewport 0 0 width height)
          (glMatrixMode GL_PROJECTION)
          (glLoadIdentity)
          (glOrtho 0 width height 0 0.1 100)
          (glMatrixMode GL_MODELVIEW)
          (glLoadIdentity)))
      (refresh))

    (define/override (on-char key)
      (with-gl-context
        (lambda ()
          (inject-event!
           (message
            (match (send key get-key-code)
              ['release (key-event (send key get-key-release-code) #f (seal key))]
              [code     (key-event code                            #t (seal key))])))
          (quiesce!))))

    (define/override (on-event mouse)
      (with-gl-context
        (lambda ()
          (define x (send mouse get-x))
          (define y (send mouse get-y))
          (define s (mouse-state x
                                 y
                                 (send mouse get-left-down)
                                 (send mouse get-middle-down)
                                 (send mouse get-right-down)))
          (set! current-mouse-state s)
          (update-touching!)
          (inject-event! (message (mouse-event (send mouse get-event-type) s)))
          (quiesce!))))

    (define (update-touching!)
      (define new-touching (detect-touch prelude childmap postlude current-mouse-state))
      (when (not (equal? new-touching current-touching))
        (define retract-old (retract current-touching))
        (if new-touching
            (inject-event! (patch-seq retract-old (assert new-touching)))
            (inject-event! retract-old))
        (set! current-touching new-touching)))

    (define (update-coordinate-maps!)
      (define aggregate-patch patch-empty)

      (define (update-single-map! cmid cmx)
        (define existing (hash-ref current-coordinate-maps cmid #f))
        (define proposed (coordinate-map cmid cmx))
        (when (not (equal? existing proposed))
          (set! current-coordinate-maps (hash-set current-coordinate-maps cmid proposed))
          (set! aggregate-patch (patch-seq aggregate-patch
                                           (retract (coordinate-map cmid ?))
                                           (assert proposed)))))

      (let process-children-of ((id #f) (xform identity-transformation))
        (for-each-child-sprite childmap id
                               (lambda (s ci)
                                 (for-each (lambda (cm)
                                             (match-define (coordinate-map cmid cmx) cm)
                                             (update-single-map!
                                              cmid
                                              (compose-transformation xform cmx)))
                                           (compiled-instructions-coordinate-maps ci))
                                 (define child-xform (compiled-instructions-child-xform ci))
                                 (when child-xform
                                   (process-children-of (sprite-id s)
                                                        (compose-transformation xform
                                                                                child-xform))))))

      (when (not (patch-empty? aggregate-patch))
        (inject-event! aggregate-patch)))

    (super-new (style '(gl no-autoclear)))))

(define ((2d-dataspace #:label [frame-label "syndicate-gl"]
                       #:width [width #f]
                       #:height [height #f]
                       #:exit? [exit? #t])
         . boot-actions)
  (collect-garbage 'incremental)
  (collect-garbage 'major)
  (define frame (new frame%
                     [style '(fullscreen-button)]
                     [label frame-label]
                     [width (or width 640)]
                     [height (or height 480)]))
  (define c (new dataspace-canvas%
                 [parent frame]
                 [boot-actions boot-actions]))
  (unless (send (send (send c get-dc) get-gl-context) ok?)
    (error '2d-dataspace "OpenGL context failed to initialize"))
  (send c focus)
  (send frame show #t)
  (yield 'wait)
  (when exit? (exit 0)))
