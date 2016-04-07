#lang racket/gui

(provide (struct-out window)
         (struct-out frame-event)
         (struct-out key-event)
         (struct-out key-pressed)
         (struct-out scene)
         (except-out (struct-out sprite) sprite)
         (rename-out [sprite <sprite>] [make-sprite sprite])
         (struct-out request-gc)
         simple-sprite
         update-scene
         update-sprites
         spawn-keyboard-integrator
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

;; Shared state maintained by program. Prelude and postlude are to be
;; sealed instruction lists. It is an error to have more than exactly
;; one active such record at a given time.
(struct scene (prelude postlude) #:transparent)

;; Shared state maintained by program. Z is to be a number, negative
;; toward camera. Instructions to be a sealed instruction list.
(struct sprite (z instructions) #:transparent)

;; Message. Requests that the OpenGL loop perform a major
;; garbage-collection while *pausing the simulation's real-time
;; correspondence*. This lets a GC take place without such severe
;; simulation glitches as happen when doing it in-world.
(struct request-gc () #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-scene prelude postlude #:meta-level [meta-level 1])
  (patch-seq (retract (scene ? ?) #:meta-level meta-level)
             (assert (scene (seal prelude) (seal postlude)) #:meta-level meta-level)))

(define (make-sprite z instructions)
  (sprite z (seal instructions)))

(define (simple-sprite z x y w h i)
  (make-sprite z `((translate ,x ,y)
                   (scale ,w ,h)
                   (texture ,i))))

(define (update-sprites #:meta-level [meta-level 1] . ss)
  (patch-seq* (cons (retract (sprite ? ?) #:meta-level meta-level)
                    (map (lambda (s) (assert s #:meta-level meta-level)) ss))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KeyboardIntegrator. Integrates key-events into key-pressed assertions.
(define (spawn-keyboard-integrator #:meta-level [meta-level 1])
  (spawn (lambda (e s)
           (match e
             [(message (at-meta (key-event code press? _)))
              (transition (void) ((if press? assert retract) (key-pressed code)))]
             [#f #f]))
         (void)
         (sub (key-event ? ? ?) #:meta-level meta-level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct compiled-instructions (render-thunk resources))

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define (compile-instructions instrs)
  (define-values (code resources) (instruction->racket-code `(begin ,@instrs)))
  (define render-thunk (eval `(lambda () ,code) ns))
  (compiled-instructions render-thunk resources))

(define (compiled-instructions-dispose! i)
  (when i
    (for [(resource (compiled-instructions-resources i))]
      (send resource dispose))))

(define (instructions->racket-code instrs)
  (define-values (code-rev resources)
    (for/fold [(code-rev '()) (resources '())] [(instr (in-list instrs))]
      (define-values (new-code new-resources) (instruction->racket-code instr))
      (values (cons new-code code-rev) (append new-resources resources))))
  (values (reverse code-rev) resources))

(define (color-number? n)
  (and (number? n)
       (<= 0.0 n 1.0)))

(define (instruction->racket-code instr)
  (match instr
    [`(rotate ,(? number? deg))
     (values `(glRotated ,deg 0 0 -1) '())]
    [`(scale ,(? number? x) ,(? number? y))
     (values `(glScaled ,x ,y 1) '())]
    [`(translate ,(? number? x) ,(? number? y))
     (values `(glTranslated ,x ,y 0) '())]
    [`(color ,(? color-number? r) ,(? color-number? g) ,(? color-number? b) ,(? color-number? a))
     (values `(glColor4d ,r ,g ,b ,a) '())]
    [`(texture ,i)
     (define entry (image->texture-cache-entry i))
     (values `(draw-gl-face ,(send entry get-texture)) (list entry))]
    [`(push-matrix ,instr ...)
     (define-values (code resources) (instructions->racket-code instr))
     (values `(begin (glPushMatrix) ,@code (glPopMatrix)) resources)]
    [`(begin ,instr ...)
     (define-values (code resources) (instructions->racket-code instr))
     (values `(begin ,@code (void)) resources)]
    [other
     (error 'instruction->racket-code "unknown render instruction: ~v" other)]))

(define (image->bitmap i)
  (cond
    [(is-a? i bitmap%)
     i]
    [(image:image? i)
     (define w (image:image-width i))
     (define h (image:image-height i))
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

(define (draw-gl-face texture)
  (send texture bind-texture)
  (glBegin GL_QUADS)
  (glNormal3d 0 0 -1)
  (glTexCoord2i 0 0)
  (glVertex3d 0 0 0)
  (glTexCoord2i 1 0)
  (glVertex3d 1 0 0)
  (glTexCoord2i 1 1)
  (glVertex3d 1 1 0)
  (glTexCoord2i 0 1)
  (glVertex3d 0 1 0)
  (glEnd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-instructions (compile-instructions '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scene-projection (?! (scene ? ?)))
(define sprite-projection (?! (sprite ? ?)))

(define sprite-order
  (order 'sprite-order
         sprite?
         (lambda (a b) (and (= (sprite-z a) (sprite-z b))
                            (eq? (sprite-instructions a)
                                 (sprite-instructions b))))
         (lambda (a b) (or (> (sprite-z a) (sprite-z b))
                           (and (= (sprite-z a) (sprite-z b))
                                (< (eq-hash-code (sprite-instructions a))
                                   (eq-hash-code (sprite-instructions b))))))))

(define (remove-sprite! sprites s)
  (compiled-instructions-dispose! (splay-tree-ref sprites s #f))
  (splay-tree-remove! sprites s))

(define (add-sprite! sprites s)
  (define instrs `((color 1 1 1 1)
                   (push-matrix ,@(seal-contents (sprite-instructions s)))))
  (define i (compile-instructions instrs))
  (splay-tree-set! sprites s i))

(define (render-scene! prelude sprites postlude)
  ((compiled-instructions-render-thunk prelude))
  (let loop ((iter (splay-tree-iterate-first sprites)))
    (when iter
      ((compiled-instructions-render-thunk (splay-tree-iterate-value sprites iter)))
      (loop (splay-tree-iterate-next sprites iter))))
  ((compiled-instructions-render-thunk postlude)))

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
    (define sprites (make-splay-tree sprite-order))
    (define postlude empty-instructions)
    (define fullscreen? #f)

    (define dataspace (make-dataspace boot-actions))
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
      (clean-transition (dataspace-handle-event e dataspace)))

    (define (quiesce!)
      (let loop ((txn #f) (need-poll? #t))
        (match txn
          [#f ;; inert
           (if (queue-empty? event-queue)
               (when need-poll? (loop (deliver-event #f) #f))
               (loop (deliver-event (dequeue! event-queue)) #t))]
          [(transition new-dataspace actions)
           (set! dataspace new-dataspace)
           (for-each process-action! actions)
           (loop #f #t)])))

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
      (for [(s removed)] (remove-sprite! sprites s))
      (for [(s added)] (add-sprite! sprites s))
      ;; (log-info "~a sprites" (splay-tree-count sprites))
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
          (let ((this-frame-time (sim-time)))
            (inject-event! (message (frame-event counter
                                                 this-frame-time
                                                 (- this-frame-time prev-frame-time)
                                                 target-frame-rate)))
            (set! counter (+ counter 1))
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
          (render-scene! prelude sprites postlude)
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

    (super-new (style '(gl no-autoclear)))))

(define (2d-dataspace #:width [width #f]
                      #:height [height #f]
                      . boot-actions)
  (collect-garbage 'incremental)
  (collect-garbage 'major)
  (define frame (new frame%
                     [style '(fullscreen-button)]
                     [label "syndicate-gl"]
                     [width (or width 640)]
                     [height (or height 480)]))
  (define c (new dataspace-canvas%
                 [parent frame]
                 [boot-actions boot-actions]))
  (unless (send (send (send c get-dc) get-gl-context) ok?)
    (error '2d-dataspace "OpenGL context failed to initialize"))
  (send c focus)
  (send frame show #t)
  (yield 'wait))
