#lang racket/base

(require 2htdp/image)
(require 2htdp/planetcute)

(require racket/set)
(require racket/match)
(require racket/promise)
(require plot/utils) ;; for vector utilities

(require syndicate)
(require syndicate/drivers/timer)
(require syndicate-gl/2d)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layers:
;;
;; - External I/O
;;   as arranged by syndicate-gl/2d
;;   including keyboard events, interface to rendering, and frame timing
;;
;; - Ground
;;   corresponds to computer itself
;;   device drivers
;;   applications (e.g. in this instance, the game)
;;
;; - Game
;;   running application
;;   per-game state, such as score and count-of-deaths
;;   process which spawns levels
;;   regular frame ticker
;;
;; - Level
;;   model of the game world
;;   actors represent entities in the world, mostly
;;   misc actors do physicsish things
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Common Data Definitions
;;
;; A Vec is a (vector Number Number)
;; A Point is a (vector Number Number)
;; (See vector functions in plot/utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Ground Layer Protocols

;;-------------------------------------------------------------------------
;; ### Scene Management
;;     - assertion: ScrollOffset
;;     - assertion: OnScreenDisplay
;;     - role: SceneManager
;;         Displays the scene backdrop and adjusts display coordinates via ScrollOffset.
;;
;; A ScrollOffset is a (scroll-offset Vec), indicating the vector to *subtract*
;; from world coordinates to get device coordinates.
(struct scroll-offset (vec) #:transparent)
;;
;; An OnScreenDisplay is an (on-screen-display Number Number (Seal Image)),
;; representing an item to display in a fixed window-relative position
;; above the scrolled part of the scene. If the coordinates are
;; positive, they measure right/down from the left/top of the image;
;; if negative, they measure left/up from the right/bottom.
(struct on-screen-display (x y sealed-image) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Game Layer Protocols

;;-------------------------------------------------------------------------
;; ### Scoring
;;     - message: AddToScore
;;     - assertion: CurrentScore
;;     - role: ScoreKeeper
;;         Maintains the score as private state.
;;         Publishes the score using a CurrentScore.
;;         Responds to AddToScore by updating the score.
;;
;; An AddToScore is an (add-to-score Number), a message
;; which signals a need to add the given number to the player's
;; current score.
(struct add-to-score (delta) #:transparent)
;;
;; A CurrentScore is a (current-score Number), an assertion
;; indicating the player's current score.
(struct current-score (value) #:transparent)

;;-------------------------------------------------------------------------
;; ### Level Spawning
;;     - assertion: LevelRunning
;;     - message: LevelCompleted
;;     - role: LevelSpawner
;;         Maintains the current level number as private state.
;;         Spawns a new Level when required.
;;         Monitors LevelRunning - when it drops, the level is over.
;;         Receives LevelCompleted messages. If LevelRunning drops without
;;         a LevelCompleted having arrived, the level ended in failure and
;;         should be restarted. If LevelComplete arrived before LevelRunning
;;         dropped, the level was completed successfully, and the next level
;;         should be presented.
;;     - role: Level
;;         Running level instance. Maintains LevelRunning while it's still
;;         going. Sends LevelCompleted if the player successfully completed
;;         the level.
;;
;; A LevelRunning is a (level-running), an assertion indicating that the
;; current level is still in progress.
(struct level-running () #:transparent)
;;
;; A LevelCompleted is a (level-completed), a message indicating that
;; the current level was *successfully* completed before it terminated.
(struct level-completed () #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ## Level Layer Protocols

;;-------------------------------------------------------------------------
;; ### Movement and Physics
;;     - message: JumpRequest
;;     - assertion: Impulse
;;     - assertion: Position
;;     - assertion: GamePieceConfiguration
;;     - assertion: Touching
;;     - role: PhysicsEngine
;;         Maintains positions, velocities and accelerations of all GamePieces.
;;         Uses GamePieceConfiguration for global properties of pieces.
;;         Publishes Position to match.
;;         Listens to FrameDescription, using it to advance the simulation.
;;         Considers only mobile GamePieces for movement.
;;         Takes Impulses as the baseline for moving GamePieces around.
;;         For massive mobile GamePieces, applies gravitational acceleration.
;;         Computes collisions between GamePieces.
;;         Uses Attributes of GamePieces to decide what to do in response to collisions.
;;         For 'touchable GamePieces, a Touching row is asserted.
;;         Responds to JumpRequest by checking whether the named piece is in a
;;         jumpable location, and sets its upward velocity negative if so.
;;     - role: GamePiece
;;         Maintains private state. Asserts Impulse to move around,
;;         and GamePieceConfiguration to get things started. May issue
;;         JumpRequests at any time. Represents both the player,
;;         enemies, the goal(s), and platforms and blocks in the
;;         environment. Asserts a Sprite two layers out to render
;;         itself.
;;
;; An ID is a Symbol; the special symbol 'player indicates the player's avatar.
;; Gensyms from (gensym 'enemy) name enemies, etc.
;;
;; A JumpRequest is a (jump-request ID), a message indicating a *request* to jump,
;; not necessarily honoured by the physics engine.
(struct jump-request (id) #:transparent)
;;
;; An Impulse is an (impulse ID Vec), an assertion indicating a contribution to
;; the net *requested* velocity of the given gamepiece.
(struct impulse (id vec) #:transparent)
;;
;; A Position is a (position ID Point Vec), an assertion describing
;; the current actual top-left corner and (physics-related, not
;; necessarily graphics-related) size of the named gamepiece.
(struct position (id top-left size) #:transparent)
;;
;; An Attribute is either
;;  - 'player - the named piece is a player avatar
;;  - 'touchable - the named piece reacts to the player's touch
;;  - 'solid - the named piece can be stood on / jumped from
;;  - 'mobile - the named piece is not fixed in place
;;  - 'massive - the named piece is subject to effects of gravity
;;               (it is an error to be 'massive but not 'mobile)
;;
;; A GamePieceConfiguration is a
;;  - (game-piece-configuration ID Point Vec (Set Attribute))
;; an assertion specifying not only the *existence* of a named
;; gamepiece, but also its initial position and size and a collection
;; of its Attributes.
(struct game-piece-configuration (id initial-position size attributes) #:transparent)
;;
;; A Touching is a
;;  - (touching ID ID Side)
;; an assertion indicating that the first ID is touching the second on
;; the named side of the second ID.
(struct touching (a b side) #:transparent)
;;
;; A Side is either 'top, 'left, 'right, 'bottom or the special value
;; 'mid, indicating an unknown or uncomputable side.

(define (game-piece-has-attribute? g attr)
  (set-member? (game-piece-configuration-attributes g) attr))

;;-------------------------------------------------------------------------
;; ### Player State
;;     - message: Damage
;;     - assertion: Health
;;     - role: Player
;;         Maintains hitpoints, which it reflects using Health.
;;         Responds to Damage.
;;         When hitpoints drop low enough, removes the player from the board.
;;
;; A Damage is a (damage ID Number), a message indicating an event that should
;; consume the given number of health points of the named gamepiece.
(struct damage (id hit-points) #:transparent)
;;
;; A Health is a (health ID Number), an assertion describing the current hitpoints
;; of the named gamepiece.
(struct health (id hit-points) #:transparent)

;;-------------------------------------------------------------------------
;; ### World State
;;     - assertion: LevelSize
;;     - role: DisplayControl
;;         Maintains a LevelSize assertion.
;;         Observes the Position of the player, and computes and maintains a
;;         ScrollOffset two layers out, to match.
;;         Also kills the player if they wander below the bottom of the level.
;;
;; A LevelSize is a (level-size Vec), an assertion describing the right-hand and
;; bottom edges of the level canvas (in World coordinates).
(struct level-size (vec) #:transparent)

;; -----------
;; Interaction Diagrams (to be refactored into the description later)
;;
;; ================================================================================
;;
;; title Jump Sequence
;;
;; Player -> Physics: (jump 'player)
;; note right of Physics: Considers the request.
;; note right of Physics: Denied -- Player is not on a surface.
;;
;; Player -> Physics: (jump 'player)
;; note right of Physics: Considers the request.
;; note right of Physics: Accepted.
;; note right of Physics: Updates velocity, position
;; Physics -> Subscribers: (vel 'player ...)
;; Physics -> Subscribers: (pos 'player ...)
;;
;;
;; ================================================================================
;;
;; title Display Control Updates
;;
;; Physics -> DisplayCtl: (pos 'player ...)
;; note right of DisplayCtl: Compares player pos to level size
;; DisplayCtl -> Subscribers: (at-meta (at-meta (scroll-offset ...)))
;;
;; ================================================================================
;;
;; title Movement Sequence
;;
;; Moveable -> Physics: (mobile ID Boolean)
;; Moveable -> Physics: (attr ID ...)
;; Moveable -> Physics: (impulse ID vec)
;; note right of Physics: Processes simulation normally
;; Physics -> Subscribers: (pos ID ...)
;; Physics -> Subscribers: (vel ID ...)
;;
;; ================================================================================
;;
;; title Keyboard Interpretation
;;
;; Keyboard -> Player: (press right-arrow)
;; Player -->> Physics: assert (impulse ID (vec DX 0))
;;
;; note right of Physics: Processes simulation normally
;;
;; Keyboard -> Player: (press left-arrow)
;; Player -->> Physics: assert (impulse ID (vec 0 0))
;;
;; Keyboard -> Player: (release right-arrow)
;; Player -->> Physics: assert (impulse ID (vec -DX 0))
;;
;; Keyboard -> Player: (press space)
;; Player -> Physics: (jump)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icon

(struct icon (pict scale hitbox-width-fraction hitbox-height-fraction baseline-fraction)
  #:transparent)

(define (icon-width i) (* (image-width (icon-pict i)) (icon-scale i)))
(define (icon-height i) (* (image-height (icon-pict i)) (icon-scale i)))
(define (icon-hitbox-width i) (* (icon-width i) (icon-hitbox-width-fraction i)))
(define (icon-hitbox-height i) (* (icon-height i) (icon-hitbox-height-fraction i)))
(define (icon-hitbox-size i) (vector (icon-hitbox-width i) (icon-hitbox-height i)))

(define (focus->top-left i x y)
  (vector (- x (/ (icon-hitbox-width i) 2))
          (- y (icon-hitbox-height i))))

(define (icon-sprite i layer pos)
  (match-define (vector x y) pos)
  (simple-sprite layer
                 (- x (/ (- (icon-width i) (icon-hitbox-width i)) 2))
                 (- y (- (* (icon-baseline-fraction i) (icon-height i)) (icon-hitbox-height i)))
                 (icon-width i)
                 (icon-height i)
                 (icon-pict i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various projections

(define window-projection1 (at-meta (?! (window ? ?))))
(define window-projection3 (at-meta (at-meta (at-meta (?! (window ? ?))))))
(define scroll-offset-projection (scroll-offset (?!)))
(define on-screen-display-projection (?! (on-screen-display ? ? ?)))
(define key-pressed-projection (at-meta (at-meta (key-pressed (?!)))))
(define position-projection (?! (position ? ? ?)))
(define impulse-projection (?! (impulse ? ?)))
(define game-piece-configuration-projection (?! (game-piece-configuration ? ? ? ?)))
(define touching-projection (?! (touching ? ? ?)))
(define level-size-projection (level-size (?!)))

(define (update-set-from-patch orig p projection)
  (define-values (added removed) (patch-project/set/single p projection))
  (set-subtract (set-union orig added) removed))

(define (update-hash-from-patch orig p projection key-f val-f)
  (define-values (added removed) (patch-project/set/single p projection))
  (define h (for/fold [(h orig)] [(e removed)] (hash-remove h (key-f e))))
  (for/fold [(h h)] [(e added)] (hash-set h (key-f e) (val-f e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SceneManager

(define (spawn-scene-manager)
  (struct scene-manager-state (size offset osds fullscreen?) #:prefab)
  (define backdrop (rectangle 1 1 "solid" "white"))

  (define (update-window-size s p)
    (define added (trie-project/set/single (patch-added p) window-projection1))
    (for/fold [(s s)] [(w added)]
      (match-define (window width height) w)
      (struct-copy scene-manager-state s [size (vector width height)])))

  (define (update-scroll-offset s p)
    (define-values (added removed) (patch-project/set/single p scroll-offset-projection))
    (for/fold [(s s)] [(vec added)]
      (struct-copy scene-manager-state s [offset vec])))

  (define (update-osds s p)
    (struct-copy scene-manager-state s
                 [osds (update-set-from-patch (scene-manager-state-osds s)
                                              p
                                              on-screen-display-projection)]))

  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (let* ((s (update-window-size s p))
                     (s (update-scroll-offset s p))
                     (s (update-osds s p)))
                (match-define (vector width height) (scene-manager-state-size s))
                (match-define (vector ofs-x ofs-y) (scene-manager-state-offset s))
                (define osd-blocks
                  (for/list [(osd (in-set (scene-manager-state-osds s)))]
                    (match-define (on-screen-display raw-x raw-y (seal i)) osd)
                    (define x (if (negative? raw-x) (+ width raw-x) raw-x))
                    (define y (if (negative? raw-y) (+ height raw-y) raw-y))
                    `(push-matrix (translate ,x ,y)
                                  (scale ,(image-width i) ,(image-height i))
                                  (texture ,i))))
                (transition s
                            (update-scene `((push-matrix
                                             (scale ,width ,height)
                                             (texture ,backdrop))
                                            (translate ,(- ofs-x) ,(- ofs-y)))
                                          `((translate ,ofs-x ,ofs-y)
                                            ,@osd-blocks))))]
             [(message (at-meta (key-event #\f _ _)))
              (define fullscreen? (not (scene-manager-state-fullscreen? s)))
              (let* ((s (struct-copy scene-manager-state s [fullscreen? fullscreen?])))
                (transition s
                            (patch-seq (retract 'fullscreen #:meta-level 1)
                                       (if fullscreen?
                                           (assert 'fullscreen #:meta-level 1)
                                           patch-empty))))]
             [_ #f]))
         (scene-manager-state (vector 0 0) (vector 0 0) (set) #f)
         (patch-seq (sub (key-event #\f #t ?) #:meta-level 1)
                    (sub (scroll-offset ?))
                    (sub (on-screen-display ? ? ?))
                    (sub (window ? ?) #:meta-level 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ScoreKeeper

(define (spawn-score-keeper)
  (define (update-display new-score)
    (define i (text (format "Score: ~a" new-score) 24 "white"))
    (patch-seq (retract (on-screen-display ? ? ?) #:meta-level 1)
               (assert (on-screen-display -150 10 (seal i)) #:meta-level 1)))
  (spawn (lambda (e s)
           (match e
             [(message (add-to-score delta))
              (define new-score (+ s delta))
              (log-info "Score increased by ~a to ~a" delta new-score)
              (define message (text (format "Score: ~a" new-score) 24 "white"))
              (transition new-score
                          (patch-seq (retract (current-score ?))
                                     (assert (current-score delta))
                                     (update-display new-score)))]
             [_ #f]))
         0
         (patch-seq (sub (add-to-score ?))
                    (update-display 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PhysicsEngine

(define impulse-multiplier 0.360) ;; 360 pixels per second
(define jump-vel (vector 0 -2))
(define gravity 0.004)

(define (spawn-physics-engine)
  (struct physics-state (configs    ;; Hash ID -> GamePieceConfiguration
                         positions  ;; Hash ID -> Point
                         velocities ;; Hash ID -> Vector
                         impulses   ;; Hash ID -> Vector
                         ) #:prefab)

  (define (piece-cfg s id) (hash-ref (physics-state-configs s) id))
  (define (piece-pos s id) (hash-ref (physics-state-positions s) id (lambda () (vector 0 0))))
  (define (piece-vel s id) (hash-ref (physics-state-velocities s) id (lambda () (vector 0 0))))
  (define (piece-imp s id) (hash-ref (physics-state-impulses s) id (lambda () (vector 0 0))))

  (define ((remove-game-piece-configurations p) s)
    (define removed (trie-project/set/single (patch-removed p)
					     game-piece-configuration-projection))
    (transition
     (for/fold [(s s)] [(g removed)]
       (define id (game-piece-configuration-id g))
       (struct-copy physics-state s
                    [configs (hash-remove (physics-state-configs s) id)]
                    [positions (hash-remove (physics-state-positions s) id)]
                    [velocities (hash-remove (physics-state-velocities s) id)]))
     (for/list [(g removed)]
       (define id (game-piece-configuration-id g))
       (retract (position id ? ?)))))

  (define ((add-game-piece-configurations p) s)
    (define added (trie-project/set/single (patch-added p)
					   game-piece-configuration-projection))
    (transition
     (for/fold [(s s)] [(g added)]
       (match-define (game-piece-configuration id initial-position _ _) g)
       (struct-copy physics-state s
                    [configs (hash-set (physics-state-configs s) id g)]
                    [positions (hash-set (physics-state-positions s) id initial-position)]
                    [velocities (hash-set (physics-state-velocities s) id (vector 0 0))]))
     (for/list [(g added)]
       (match-define (game-piece-configuration id initial-position size _) g)
       (assert (position id initial-position size)))))

  (define ((update-impulses p) s)
    (transition
     (struct-copy physics-state s
                  [impulses (update-hash-from-patch (physics-state-impulses s)
                                                    p
                                                    impulse-projection
                                                    impulse-id
                                                    impulse-vec)])
     '()))

  (define ((update-piece g old-pos new-pos new-vel) s)
    (define id (game-piece-configuration-id g))
    (transition
     (struct-copy physics-state s
                  [positions (hash-set (physics-state-positions s) id new-pos)]
                  [velocities (hash-set (physics-state-velocities s) id new-vel)])
     (and (not (v= old-pos new-pos))
          (patch-seq (retract (position id ? ?))
                     (assert (position id new-pos (game-piece-configuration-size g)))))))

  (define (find-support p size s)
    (match-define (vector p-left p-top) p)
    (match-define (vector p-w p-h) size)
    (define p-right (+ p-left p-w))
    (define p-bottom (+ p-top p-h))
    (for/or [((id g) (in-hash (physics-state-configs s)))
             #:when (game-piece-has-attribute? g 'solid)]
      (match-define (vector left top) (piece-pos s id))
      (and (< (abs (- top p-bottom)) 0.5)
           (<= left p-right)
           (match (game-piece-configuration-size g)
             [(vector w h)
              (<= p-left (+ left w))])
           g)))

  (define (segment-intersection-time p0 r q0 q1)
    ;; See http://stackoverflow.com/a/565282/169231
    ;; Enhanced to consider the direction of impact with the segment,
    ;; too: only returns an intersection when the vector of motion is
    ;; at an obtuse angle to the normal of the segment.
    (define s (v- q1 q0))
    (define rxs (vcross2 r s))
    (cond [(< (abs rxs) 0.005) #f] ;; zeroish; lines are parallel (and maybe collinear)
          [else
           (define q-p (v- q0 p0))
           (define q-pxs (vcross2 q-p s))
           (define t (/ q-pxs rxs))
           (and (<= 0 t 1)
                (let* ((q-pxr (vcross2 q-p r))
                       (u (/ q-pxr rxs)))
                  (and (< 0 u 1)
                       (let* ((q-norm
                               (vnormalize (vector (vector-ref s 1) (- (vector-ref s 0))))))
                         (and (not (positive? (vdot r q-norm)))
                              (- t 0.001))))))]))

  (define (three-corners top-left size)
    (match-define (vector w h) size)
    (values (v+ top-left (vector w 0))
            (v+ top-left size)
            (v+ top-left (vector 0 h))))

  (define (clip-movement-by top-left moved-top-left size solid-top-left solid-size)
    (define-values (solid-top-right solid-bottom-right solid-bottom-left)
      (three-corners solid-top-left solid-size))
    (define-values (top-right bottom-right bottom-left)
      (three-corners top-left size))
    (define r (v- moved-top-left top-left))
    (define t
      (apply min
             (for/list [(p (in-list (list #;top-left #;top-right bottom-right bottom-left)))]
               (min (or (segment-intersection-time p r solid-top-left solid-top-right) 1)
                    ;; TODO: some means of specifying *which edges* should appear solid.
                    #;(or (segment-intersection-time p r solid-top-right solid-bottom-right) 1)
                    #;(or (segment-intersection-time p r solid-bottom-right solid-bottom-left) 1)
                    #;(or (segment-intersection-time p r solid-bottom-left solid-top-left) 1)))))
    (v+ top-left (v* r t)))

  (define (clip-movement-by-solids s p0 p1 size)
    (for/fold [(p1 p1)]
              [((id g) (in-hash (physics-state-configs s)))
               #:when (game-piece-has-attribute? g 'solid)]
      (clip-movement-by p0 p1 size (piece-pos s id) (game-piece-configuration-size g))))

  (define (touched-during-movement? top-left moved-top-left size touchable-top-left touchable-size)
    (define r (v- moved-top-left top-left))
    (if (positive? (vmag^2 r)) ;; r is nonzero, in other words
        (let ()
          (define-values (touchable-top-right touchable-bottom-right touchable-bottom-left)
            (three-corners touchable-top-left touchable-size))
          (define-values (top-right bottom-right bottom-left)
            (three-corners top-left size))
          (for/or [(p (in-list (list top-left top-right bottom-right bottom-left)))]
            (or
             (and (segment-intersection-time p r touchable-top-right touchable-bottom-right) 'right)
             (and (segment-intersection-time p r touchable-bottom-right touchable-bottom-left) 'bottom)
             (and (segment-intersection-time p r touchable-bottom-left touchable-top-left) 'left)
             (and (segment-intersection-time p r touchable-top-left touchable-top-right) 'top))))
        (let ()
          (match-define (vector left top) top-left)
          (match-define (vector touchable-left touchable-top) touchable-top-left)
          (match-define (vector width height) size)
          (match-define (vector touchable-width touchable-height) touchable-size)
          (and (<= left (+ touchable-left touchable-width))
               (<= top (+ touchable-top touchable-height))
               (<= touchable-left (+ left width))
               (<= touchable-top (+ top height))
               'mid))))

  (define (touchables-touched-during-movement s p0 p1 size)
    (for/fold [(ts '())]
              [((id g) (in-hash (physics-state-configs s)))
               #:when (game-piece-has-attribute? g 'touchable)]
      (define side
        (touched-during-movement? p0 p1 size (piece-pos s id) (game-piece-configuration-size g)))
      (if side (cons (cons side g) ts) ts)))

  (define ((update-game-piece elapsed-ms id state-at-beginning-of-frame) s)
    (define g (piece-cfg state-at-beginning-of-frame id))
    (define size (game-piece-configuration-size g))
    (define pos0 (piece-pos state-at-beginning-of-frame id))
    (define support (find-support pos0 size state-at-beginning-of-frame))

    (define vel0 (piece-vel state-at-beginning-of-frame id))
    (define imp0 (piece-imp state-at-beginning-of-frame id))

    (define vel1 (cond
                   [(and support (not (negative? (vector-ref vel0 1))))
                    (piece-vel state-at-beginning-of-frame
                               (game-piece-configuration-id support))]
                   [(game-piece-has-attribute? g 'massive)
                    (v+ vel0 (vector 0 (* gravity elapsed-ms)))]
                   [else
                    vel0]))

    (define pos1 (v+ pos0 (v* (v+ vel1 imp0) (* impulse-multiplier elapsed-ms))))
    (define final-pos (clip-movement-by-solids state-at-beginning-of-frame pos0 pos1 size))
    ;; TODO: figure out how to cancel just the component of velocity blocked by the obstacle(s)
    ;; - which will avoid the "sticking to the wall" artifact
    (define final-vel (if (v= pos1 final-pos) vel1 (vector 0 0))) ;; stop at collision
    (define touchables
      (touchables-touched-during-movement state-at-beginning-of-frame pos0 final-pos size))
    (sequence-transitions
     (transition s
                 (patch-seq*
                  (cons (retract (touching id ? ?))
                        (for/list [(t touchables)]
                          (match-define (cons side tg) t)
                          (assert
                           (touching id (game-piece-configuration-id tg) side))))))
     (update-piece g pos0 final-pos final-vel)))

  (define (evaluate-jump-request id s)
    (define g (piece-cfg s id))
    (define pos (piece-pos s id))
    (define support (find-support pos (game-piece-configuration-size g) s))
    (and support
         ((update-piece g pos (v+ pos (vector 0 -1)) jump-vel) s)))

  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (sequence-transitions (transition s '())
                                    (remove-game-piece-configurations p)
                                    (add-game-piece-configurations p)
                                    (update-impulses p))]
             [(message (jump-request id))
              (evaluate-jump-request id s)]
             [(message (at-meta (at-meta (at-meta (frame-event counter _ elapsed-ms _)))))
              (when (zero? (modulo counter 10))
                (log-info "Instantaneous frame rate at frame ~a: ~a Hz"
                          counter
                          (/ 1000.0 elapsed-ms)))
              (for/fold [(t (transition s '()))]
                        [((id g) (in-hash (physics-state-configs s)))
                         #:when (game-piece-has-attribute? g 'mobile)]
                (transition-bind (update-game-piece elapsed-ms id s) t))]
             [_ #f]))
         (physics-state (hash)
                        (hash)
                        (hash)
                        (hash))
         (patch-seq (sub (impulse ? ?))
                    (sub (game-piece-configuration ? ? ? ?))
                    (sub (jump-request ?))
                    (sub (frame-event ? ? ? ?) #:meta-level game-level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player

(define player-id 'player)
(define planetcute-scale 1/2)

(define (spawn-player-avatar initial-focus-x initial-focus-y)
  (struct player-state (pos hit-points keys-down) #:prefab)

  (define i (icon character-cat-girl planetcute-scale 2/6 3/10 13/16))
  (define initial-top-left (focus->top-left i initial-focus-x initial-focus-y))
  (define initial-player-state (player-state initial-top-left 1 (set)))

  (define (sprite-update s)
    (update-sprites #:meta-level game-level (icon-sprite i 0 (player-state-pos s))))

  (define ((monitor-position-change p) s)
    (define s1
      (for/fold [(s s)] [(pos (trie-project/set/single (patch-added p) position-projection))]
        (match-define (position _ hitbox-top-left _) pos)
        (struct-copy player-state s [pos hitbox-top-left])))
    (transition s1 (sprite-update s1)))

  (define ((integrate-keypresses p) s)
    (transition
     (struct-copy player-state s
                  [keys-down (update-set-from-patch (player-state-keys-down s)
                                                    p
                                                    key-pressed-projection)])
     '()))

  (define (any-key-down? s . ks)
    (for/or [(k ks)] (set-member? (player-state-keys-down s) k)))

  (define ((maybe-jump s0) s)
    (transition s
                (and (not (any-key-down? s0 #\space #\.))
                     (any-key-down? s #\space #\.)
                     (message (jump-request player-id)))))

  (define (update-impulse s)
    (let* ((h-impulse 0)
           (h-impulse (+ h-impulse (if (any-key-down? s 'left 'prior) -1 0)))
           (h-impulse (+ h-impulse (if (any-key-down? s 'right 'next) 1 0))))
      (transition s
                  (patch-seq (retract (impulse player-id ?))
                             (assert (impulse player-id (vector h-impulse 0)))))))

  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (sequence-transitions (transition s '())
                                    (monitor-position-change p)
                                    (integrate-keypresses p)
                                    (maybe-jump s)
                                    update-impulse)]
             [(message (damage _ amount))
              (define hit-points (player-state-hit-points s))
              (define new-hit-points (- hit-points amount))
              (if (positive? new-hit-points)
                  (transition (struct-copy player-state s [hit-points (- hit-points amount)]) '())
                  (quit))]
             [_ #f]))
         initial-player-state
         (patch-seq
          (sub (damage player-id ?))
          (assert (health player-id (player-state-hit-points initial-player-state)))
          (assert (game-piece-configuration player-id
                                            initial-top-left
                                            (icon-hitbox-size i)
                                            (set 'player 'mobile 'massive)))
          (sub (position player-id ? ?))
          (sub (key-pressed 'left) #:meta-level 2)
          (sub (key-pressed 'right) #:meta-level 2)
          (sub (key-pressed #\space) #:meta-level 2)
          (sub (key-pressed 'prior) #:meta-level 2)
          (sub (key-pressed 'next) #:meta-level 2)
          (sub (key-pressed #\.) #:meta-level 2)
          (sprite-update initial-player-state)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Block

(define (spawn-ground-block top-left size #:color [color "purple"])
  (match-define (vector x y) top-left)
  (match-define (vector w h) size)
  (define block-id (gensym 'ground-block))
  (define block-pict (rectangle w h "solid" color))
  (spawn (lambda (e s)
           (match e
             [_ #f]))
         (void)
         (patch-seq
          (update-sprites #:meta-level game-level (simple-sprite 0 x y w h block-pict))
          (assert (game-piece-configuration block-id
                                            top-left
                                            size
                                            (set 'solid))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Goal piece
;;
;; When the player touches a goal, sends LevelCompleted one layer out.

(define (spawn-goal-piece initial-focus-x initial-focus-y)
  (define goal-id (gensym 'goal))

  (define i (icon key planetcute-scale 1/3 2/5 4/5))
  (define initial-top-left (focus->top-left i initial-focus-x initial-focus-y))

  (spawn (lambda (e s)
           (match e
             [(? patch/added?) (transition s (message (at-meta (level-completed))))]
             [_ #f]))
         (void)
         (patch-seq
          (assert (game-piece-configuration goal-id
                                            initial-top-left
                                            (icon-hitbox-size i)
                                            (set 'touchable)))
          (sub (touching player-id goal-id ?))
          (update-sprites #:meta-level game-level (icon-sprite i -1 initial-top-left)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enemy

(define (spawn-enemy initial-x initial-y range-lo range-hi
                     #:speed [speed 0.2]
                     #:facing [initial-facing 'right])
  (struct enemy-state [level-size facing] #:prefab)

  (define enemy-id (gensym 'enemy))

  (define i (icon enemy-bug planetcute-scale 9/10 1/3 5/6))
  (define i-flipped (struct-copy icon i [pict (flip-horizontal (icon-pict i))]))
  (define initial-top-left (focus->top-left i initial-x initial-y))
  (define initial-state (enemy-state #f initial-facing))

  (define (sprite-patch s top-left)
    (update-sprites #:meta-level game-level
                    (icon-sprite (match (enemy-state-facing s)
                                   ['right i]
                                   ['left i-flipped])
                                 -1
                                 top-left)))

  (define (motion-patch s)
    (patch-seq (retract (impulse enemy-id ?))
               (assert (impulse enemy-id (vector (* speed (match (enemy-state-facing s)
                                                            ['right 1]
                                                            ['left -1]))
                                                 0)))))

  (define ((monitor-level-size-change p) s)
    (transition (for/fold [(s s)] [(vec (trie-project/set/single (patch-added p)
								 level-size-projection))]
                  (struct-copy enemy-state s [level-size vec]))
                '()))

  (define ((monitor-position-change p) s)
    (define positions (trie-project/set/single (patch-added p) position-projection))
    (and (not (set-empty? positions))
         (match (set-first positions)
           [(position _ (and top-left (vector left top)) (vector width height))
            (match (enemy-state-level-size s)
              [(vector _ level-height) #:when (> top level-height) (quit)]
              [_
               (define old-facing (enemy-state-facing s))
               (define new-facing (cond [(< left range-lo) 'right]
                                        [(> (+ left width) range-hi) 'left]
                                        [else old-facing]))
               (if (equal? old-facing new-facing)
                   (transition s (sprite-patch s top-left))
                   (let ((new-s (struct-copy enemy-state s [facing new-facing])))
                     (transition new-s
                                 (list (motion-patch new-s)
                                       (sprite-patch new-s top-left)))))])])))

  (define ((damage-contacts p) s)
    (define-values (to-damage squashed?)
      (for/fold [(to-damage '()) (squashed? #f)]
                [(t (trie-project/set/single (patch-added p) touching-projection))]
        (match-define (touching who _ side) t)
        (if (eq? side 'top)
            (values to-damage #t)
            (values (cons who to-damage) squashed?))))
    (define damage-actions (for/list [(who to-damage)] (message (damage who 1))))
    (if squashed?
        (quit (list damage-actions (message (at-meta (add-to-score 1)))))
        (transition s damage-actions)))

  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (sequence-transitions (transition s '())
                                    (monitor-level-size-change p)
                                    (monitor-position-change p)
                                    (damage-contacts p))]
             [_ #f]))
         initial-state
         (patch-seq
          (assert (game-piece-configuration enemy-id
                                            initial-top-left
                                            (icon-hitbox-size i)
                                            (set 'mobile 'massive 'touchable)))
          (sub (level-size ?))
          (sub (position enemy-id ? ?))
          (sub (touching player-id enemy-id ?))
          (motion-patch initial-state)
          (sprite-patch initial-state initial-top-left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DisplayControl

(define (spawn-display-controller level-size-vec)
  (match-define (vector level-width level-height) level-size-vec)

  (define ((update-window-size p) s)
    (define added (trie-project/set/single (patch-added p) window-projection3))
    (transition (for/fold [(s s)] [(w added)]
                  (match-define (window width height) w)
                  (vector width height))
                '()))

  (define (compute-offset pos viewport limit)
    (min (max 0 (- pos (/ viewport 2))) (- limit viewport)))

  (define ((update-scroll-offset-from-player-position p) s)
    (define player-positions (trie-project/set/single (patch-added p) position-projection))
    (and (not (set-empty? player-positions))
         (let ((player-position (set-first player-positions)))
           (match-define (vector ww wh) s)
           (match-define (position _ (vector px py) _) player-position)
           (if (> py level-height)
               (transition s (message (damage player-id +inf.0)))
               (let ((offset-pos (vector (compute-offset px ww level-width)
                                         (compute-offset py wh level-height))))
                 (transition s
                             (patch-seq (retract #:meta-level 2 (scroll-offset ?))
                                        (assert #:meta-level 2 (scroll-offset offset-pos)))))))))

  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (sequence-transitions (transition s '())
                                    (update-window-size p)
                                    (update-scroll-offset-from-player-position p))]
             [_ #f]))
         (vector 0 0)
         (patch-seq (sub (window ? ?) #:meta-level game-level)
                    (sub (position player-id ? ?))
                    (assert (level-size level-size-vec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LevelTerminationMonitor
;;
;; When the player vanishes from the board, or LevelCompleted is seen,
;; kills the dataspace.

(define (spawn-level-termination-monitor)
  (spawn (lambda (e s)
           (match e
             [(? patch/removed?)
              (log-info "Player died! Terminating level.")
              (transition s (quit-dataspace))]
             [(message (at-meta (level-completed)))
              (log-info "Level completed! Terminating level.")
              (transition s (list (message (at-meta (add-to-score 100)))
                                  (quit-dataspace)))]
             [_ #f]))
         (void)
         (patch-seq (sub (game-piece-configuration player-id ? ? ?))
                    (sub (level-completed) #:meta-level 1)
                    (assert (level-running) #:meta-level 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LevelSpawner

(define (spawn-standalone-assertions . patches)
  (<spawn> (lambda ()
             (list (lambda (e s) #f)
                   (transition (void) (patch-seq* patches))
                   #f))))

(define (spawn-background-image level-size scene)
  (match-define (vector level-width level-height) level-size)
  (define scene-width (image-width scene))
  (define scene-height (image-height scene))
  (define level-aspect (/ level-width level-height))
  (define scene-aspect (/ scene-width scene-height))
  (define scale (if (> level-aspect scene-aspect) ;; level is wider, proportionally, than scene
                    (/ level-width scene-width)
                    (/ level-height scene-height)))
  (spawn-standalone-assertions
   (update-sprites #:meta-level game-level
                   (sprite 10
                           `((scale ,(* scene-width scale)
                                    ,(* scene-height scale))
                             (texture ,scene))))))

;; http://www.travelization.net/wp-content/uploads/2012/07/beautiful-grassland-wallpapers-1920x1080.jpg
(define grassland-backdrop (bitmap "beautiful-grassland-wallpapers-1920x1080.jpg"))

(define (spawn-level #:initial-player-x [initial-player-x 50]
                     #:initial-player-y [initial-player-y 50]
                     #:level-size [level-size-vec (vector 4000 2000)]
                     #:scene [scene grassland-backdrop]
                     . actions)
  (spawn-dataspace
   (and scene (spawn-background-image level-size-vec scene))
   (spawn-display-controller level-size-vec)
   (spawn-physics-engine)
   (spawn-player-avatar initial-player-x initial-player-y)
   (spawn-level-termination-monitor)
   actions))

(define standard-ground-height 50)

(define (slab left top width #:color [color "purple"])
  (spawn-ground-block (vector left top) (vector width standard-ground-height) #:color color))

(define levels
  (delay
    (list
     (spawn-level (slab 25 125 100)
                  (slab 50 300 500)
                  (spawn-enemy 100 300 50 550)
                  (spawn-enemy 300 300 50 550 #:facing 'left)
                  (spawn-goal-piece 570 150)
                  (slab 850 300 50)
                  (slab 925 400 50)
                  (slab 975 500 50)
                  (slab 975 600 50)
                  (slab 500 600 150 #:color "orange"))
     (spawn-level (slab 25 300 500)
                  (slab 500 400 500)
                  (slab 1000 500 400)
                  (spawn-goal-piece 1380 500))
     (spawn-level (slab 25 300 1000)
                  (spawn-enemy 600 300 25 1025 #:facing 'left)
                  (spawn-goal-piece 980 300))
     (spawn-level (spawn-goal-piece 250 280)
                  (spawn-enemy 530 200 400 600)
                  (spawn-enemy 500 200 -100 1000 #:facing 'left)
                  (slab 400 200 200)
                  (spawn-ground-block (vector 200 280) (vector 200 200) #:color "orange")
                  (slab 25 300 500)
                  (slab 600 1300 600)
                  (slab 1150 1200 25 #:color "red")
                  (for/list ((n 10))
                    (slab 900 (+ 200 (* n 100)) 50))
                  )
     )))

(define (spawn-numbered-level level-number)
  (list (message (at-meta (at-meta (request-gc))))
        (if (< level-number (length (force levels)))
            (list-ref (force levels) level-number)
            (spawn-standalone-assertions
             (update-sprites #:meta-level 2
                             (let ((message (text "You won!" 72 "red")))
                               (simple-sprite 0
                                              10
                                              100
                                              (image-width message)
                                              (image-height message)
                                              message)))))))

(define (spawn-level-spawner starting-level)
  (struct level-spawner-state (current-level level-complete?) #:prefab)

  (list (spawn (lambda (e s)
                 (match-define (level-spawner-state current-level level-complete?) s)
                 (match e
                   [(? patch/removed?)
                    (define next-level (if level-complete? (+ current-level 1) current-level))
                    (transition (level-spawner-state next-level #f)
                                (spawn-numbered-level next-level))]
                   [(message (level-completed))
                    (transition (struct-copy level-spawner-state s [level-complete? #t]) '())]
                   [_ #f]))
               (level-spawner-state starting-level #f)
               (patch-seq (sub (level-running))
                          (sub (level-completed))))
        (spawn-numbered-level starting-level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define game-level 3) ;; used to specify meta-level to reach external I/O

((2d-dataspace #:width 600 #:height 400)
 (spawn-keyboard-integrator)
 (spawn-scene-manager)
 (spawn-dataspace (spawn-score-keeper)
                  (spawn-level-spawner 0)))
