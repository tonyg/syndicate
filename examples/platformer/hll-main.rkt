#lang syndicate/actor

(require 2htdp/image)
(require 2htdp/planetcute)

(require racket/set)
(require plot/utils) ;; for vector utilities

(require (only-in racket/string string-prefix?))
(require (only-in racket/gui/base play-sound))

(require/activate syndicate/drivers/timer)

(require syndicate-gl/2d)
(module+ main (current-ground-dataspace (2d-dataspace #:width 600 #:height 400)))

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
;; DisplayCtl -> Subscribers: (inbound (inbound (scroll-offset ...)))
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
;; SceneManager

(define (spawn-scene-manager)
  (actor #:name 'scene-manager
         (define backdrop (rectangle 1 1 "solid" "white"))

         (react
          (define/query-value size (vector 0 0) (inbound (window $x $y)) (vector x y))
          (define/query-set osds ($ o (on-screen-display _ _ _)) o)
          (define/query-value offset (vector 0 0) (scroll-offset $v) v)

          (field [fullscreen? #f])
          (assert #:when (fullscreen?) (outbound 'fullscreen))
          (on (message (inbound (key-event #\f #t _)))
              (fullscreen? (not (fullscreen?))))

          (define (compute-backdrop)
            (match-define (vector width height) (size))
            (match-define (vector ofs-x ofs-y) (offset))
            (define osd-blocks
              (for/list [(osd (in-set (osds)))]
                (match-define (on-screen-display raw-x raw-y (seal i)) osd)
                (define x (if (negative? raw-x) (+ width raw-x) raw-x))
                (define y (if (negative? raw-y) (+ height raw-y) raw-y))
                `(push-matrix (translate ,x ,y)
                              (scale ,(image-width i) ,(image-height i))
                              (texture ,i))))
            (scene (seal `((push-matrix
                            (scale ,width ,height)
                            (texture ,backdrop))
                           (translate ,(- ofs-x) ,(- ofs-y))))
                   (seal `((translate ,ofs-x ,ofs-y)
                           ,@osd-blocks))))
          (assert (outbound (compute-backdrop))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ScoreKeeper

(define (spawn-score-keeper)
  (actor #:name 'score-keeper
         (react
          (field [score 0])
          (assert (current-score (score)))
          (assert (outbound
                   (on-screen-display -150 10
                                      (seal (text (format "Score: ~a" (score)) 24 "white")))))
          (on (message (add-to-score $delta))
              (score (+ (score) delta))
              (log-info "Score increased by ~a to ~a" delta (score))
              (play-sound-sequence 270304)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PhysicsEngine

(define impulse-multiplier 0.360) ;; 360 pixels per second
(define jump-vel (vector 0 -2))
(define gravity 0.004)

(define (spawn-physics-engine)
  (actor #:name 'physics-engine
   (react
    (field [configs (hash)]
           [previous-positions (hash)]
           [previous-velocities (hash)]
           [positions (hash)]
           [velocities (hash)])

    (during (game-piece-configuration $id $initial-position $size $attrs)
            (on-start (configs
                       (hash-set (configs) id
                                 (game-piece-configuration id initial-position size attrs))))
            (on-stop (configs (hash-remove (configs) id))
                     (positions (hash-remove (positions) id))
                     (velocities (hash-remove (velocities) id)))
            (assert (position id (hash-ref (positions) id initial-position) size)))

    (define/query-hash impulses (impulse $id $vec) id vec)

    (define (piece-cfg id) (hash-ref (configs) id))
    (define (piece-pos which id)
      (hash-ref (which) id (lambda () (game-piece-configuration-initial-position (piece-cfg id)))))
    (define (piece-vel which id) (hash-ref (which) id (lambda () (vector 0 0))))
    (define (piece-imp id) (hash-ref (impulses) id (lambda () (vector 0 0))))

    (define (update-piece! g new-pos new-vel)
      (positions (hash-set (positions) (game-piece-configuration-id g) new-pos))
      (velocities (hash-set (velocities) (game-piece-configuration-id g) new-vel)))

    (define (find-support p size which-pos)
      (match-define (vector p-left p-top) p)
      (match-define (vector p-w p-h) size)
      (define p-right (+ p-left p-w))
      (define p-bottom (+ p-top p-h))
      (for/or [((id g) (in-hash (configs))) #:when (game-piece-has-attribute? g 'solid)]
        (match-define (vector left top) (piece-pos which-pos id))
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

    (define (clip-movement-by-solids p0 p1 size)
      (for/fold [(p1 p1)]
                [((id g) (in-hash (configs))) #:when (game-piece-has-attribute? g 'solid)]
        (clip-movement-by p0 p1 size
                          (piece-pos previous-positions id)
                          (game-piece-configuration-size g))))

    (define (touched-during-movement? TL moved-TL size touchable-TL touchable-size)
      (define r (v- moved-TL TL))
      (if (positive? (vmag^2 r)) ;; r is nonzero, in other words
          (let ()
            (define-values (touchable-TR touchable-BR touchable-BL)
              (three-corners touchable-TL touchable-size))
            (define-values (TR BR BL)
              (three-corners TL size))
            (for/or [(p (in-list (list TL TR BR BL)))]
              (or
               (and (segment-intersection-time p r touchable-TR touchable-BR) 'right)
               (and (segment-intersection-time p r touchable-BR touchable-BL) 'bottom)
               (and (segment-intersection-time p r touchable-BL touchable-TL) 'left)
               (and (segment-intersection-time p r touchable-TL touchable-TR) 'top))))
          (let ()
            (match-define (vector left top) TL)
            (match-define (vector touchable-left touchable-top) touchable-TL)
            (match-define (vector width height) size)
            (match-define (vector touchable-width touchable-height) touchable-size)
            (and (<= left (+ touchable-left touchable-width))
                 (<= top (+ touchable-top touchable-height))
                 (<= touchable-left (+ left width))
                 (<= touchable-top (+ top height))
                 'mid))))

    (define (touchables-touched-during-movement p0 p1 size)
      (for/fold [(ts '())]
                [((id g) (in-hash (configs))) #:when (game-piece-has-attribute? g 'touchable)]
        (define side (touched-during-movement? p0 p1 size
                                               (piece-pos previous-positions id)
                                               (game-piece-configuration-size g)))
        (if side (cons (cons side g) ts) ts)))

    (define (update-game-piece! elapsed-ms id)
      (define g (piece-cfg id))
      (define size (game-piece-configuration-size g))
      (define pos0 (piece-pos previous-positions id))
      (define support (find-support pos0 size previous-positions))

      (define vel0 (piece-vel previous-velocities id))
      (define imp0 (piece-imp id))

      (define vel1 (cond
                     [(and support (not (negative? (vector-ref vel0 1))))
                      (piece-vel previous-velocities (game-piece-configuration-id support))]
                     [(game-piece-has-attribute? g 'massive)
                      (v+ vel0 (vector 0 (* gravity elapsed-ms)))]
                     [else
                      vel0]))

      (define pos1 (v+ pos0 (v* (v+ vel1 imp0) (* impulse-multiplier elapsed-ms))))
      (define final-pos (clip-movement-by-solids pos0 pos1 size))
      ;; TODO: figure out how to cancel just the component of velocity blocked by the obstacle(s)
      ;; - which will avoid the "sticking to the wall" artifact
      (define final-vel (if (v= pos1 final-pos) vel1 (vector 0 0))) ;; stop at collision
      (define touchables (touchables-touched-during-movement pos0 final-pos size))

      (retract! (touching id ? ?))
      (for [(t touchables)]
        (match-define (cons side tg) t)
        (assert! (touching id (game-piece-configuration-id tg) side)))
      (update-piece! g final-pos final-vel))

    (on (message (jump-request $id))
        (define g (piece-cfg id))
        (define pos (piece-pos positions id))
        (when (find-support pos (game-piece-configuration-size g) positions)
          (play-sound-sequence 270318)
          (update-piece! g pos jump-vel)))

    (on (message (inbound* game-level (frame-event $counter _ $elapsed-ms _)))
        (when (zero? (modulo counter 10))
          (log-info "Instantaneous frame rate at frame ~a: ~a Hz"
                    counter
                    (/ 1000.0 elapsed-ms)))
        (previous-positions (positions))
        (previous-velocities (velocities))
        (for [((id g) (in-hash (configs))) #:when (game-piece-has-attribute? g 'mobile)]
          (update-game-piece! elapsed-ms id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player

(define player-id 'player)
(define planetcute-scale 1/2)

(define (spawn-player-avatar initial-focus-x initial-focus-y)
  (actor #:name 'player-avatar
   (react
    (define i (icon character-cat-girl planetcute-scale 2/6 3/10 13/16))
    (define initial-top-left (focus->top-left i initial-focus-x initial-focus-y))

    (assert (game-piece-configuration player-id
                                      initial-top-left
                                      (icon-hitbox-size i)
                                      (set 'player 'mobile 'massive)))

    (define/query-value pos initial-top-left (position player-id $hitbox-top-left _)
      hitbox-top-left)
    (assert (outbound* game-level (icon-sprite i 0 (pos))))

    (field [hit-points 1])
    (assert (health player-id (hit-points)))
    (stop-when (rising-edge (<= (hit-points) 0)))
    (on (message (damage player-id $amount))
        (hit-points (- (hit-points) amount)))

    (on (asserted (inbound* 2 (key-pressed #\space))) (send! (jump-request player-id)))
    (on (asserted (inbound* 2 (key-pressed #\.)))     (send! (jump-request player-id)))

    (define/query-set keys-down (inbound* 2 (key-pressed $k)) k)
    (define (any-key-down? . ks) (for/or [(k ks)] (set-member? (keys-down) k)))
    (assert (impulse player-id (vector (+ (if (any-key-down? 'left 'prior) -1 0)
                                          (if (any-key-down? 'right 'next) 1 0))
                                       0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Block

(define (spawn-ground-block top-left size #:color [color "purple"])
  (actor #:name (list 'ground-block top-left size color)
         (match-define (vector x y) top-left)
         (match-define (vector w h) size)
         (define block-id (gensym 'ground-block))
         (define block-pict (rectangle w h "solid" color))
         (react
          (assert (outbound* game-level (simple-sprite 0 x y w h block-pict)))
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

  (actor #:name (list 'goal-piece initial-focus-x initial-focus-y)
         (react
          (on (asserted (touching player-id goal-id _))
              (send! (outbound (level-completed))))
          (assert (game-piece-configuration goal-id
                                            initial-top-left
                                            (icon-hitbox-size i)
                                            (set 'touchable)))
          (assert (outbound* game-level (icon-sprite i -1 initial-top-left))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enemy

(define (spawn-enemy initial-x initial-y range-lo range-hi
                     #:speed [speed 0.2]
                     #:facing [initial-facing 'right])
  (actor #:name (list 'enemy initial-x initial-y initial-facing)
   (react
    (define enemy-id (gensym 'enemy))
    (define i (icon enemy-bug planetcute-scale 9/10 1/3 5/6))
    (define i-flipped (struct-copy icon i [pict (flip-horizontal (icon-pict i))]))
    (define initial-top-left (focus->top-left i initial-x initial-y))
    (match-define (vector width height) (icon-hitbox-size i))

    (assert (game-piece-configuration enemy-id
                                      initial-top-left
                                      (icon-hitbox-size i)
                                      (set 'mobile 'massive 'touchable)))

    (define/query-value current-level-size #f (level-size $v) v)

    (define/query-value pos initial-top-left (position enemy-id $top-left _) top-left
      #:on-add (match-let (((vector left top) top-left))
                 (facing (cond [(< left range-lo) 'right]
                               [(> (+ left width) range-hi) 'left]
                               [else (facing)]))))

    (stop-when (rising-edge (and (current-level-size)
                                 (> (vector-ref (pos) 1)
                                    (vector-ref (current-level-size) 1)))))

    (field [facing initial-facing])
    (assert (outbound* game-level
                       (icon-sprite (match (facing) ['right i] ['left i-flipped]) -1 (pos))))

    (assert (impulse enemy-id (vector (* speed (match (facing) ['right 1] ['left -1])) 0)))

    (stop-when (asserted (touching player-id enemy-id 'top))
               (play-sound-sequence 270325)
               (send! (outbound (add-to-score 1))))

    (on (asserted (touching player-id enemy-id $side))
        (when (not (eq? side 'top)) (send! (damage player-id 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DisplayControl

(define (spawn-display-controller level-size-vec)
  (match-define (vector level-width level-height) level-size-vec)

  (actor #:name 'display-controller
   (react
    (field [offset-pos (vector 0 0)])
    (assert (outbound* 2 (scroll-offset (offset-pos))))
    (assert (level-size level-size-vec))

    (define/query-value window-size-vec #f (inbound* game-level (window $w $h)) (vector w h))

    (define (compute-offset pos viewport limit)
      (min (max 0 (- pos (/ viewport 2))) (- limit viewport)))

    (on (asserted (position player-id (vector $px $py) _))
        (when (window-size-vec)
          (match-define (vector ww wh) (window-size-vec))
          (when (> py level-height) (send! (damage player-id +inf.0)))
          (offset-pos (vector (compute-offset px ww level-width)
                              (compute-offset py wh level-height))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LevelTerminationMonitor
;;
;; When the player vanishes from the board, or LevelCompleted is seen,
;; kills the dataspace.

(define (wait-for-level-termination)
  (react/suspend (done)
                 (assert (outbound (level-running)))
                 (stop-when (retracted (game-piece-configuration player-id _ _ _))
                            (log-info "Player died! Terminating level.")
                            (play-sound-sequence 270328)
                            (done))
                 (stop-when (message (inbound (level-completed)))
                            (log-info "Level completed! Terminating level.")
                            (play-sound-sequence 270330)
                            (send! (outbound (add-to-score 100)))
                            (done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LevelSpawner

(define (spawn-standalone-assertions . patches)
  (actor #:name 'standalone-assertions
         (patch! (patch-seq* patches))
         (forever)))

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
                     actions-thunk)
  (lambda ()
    (dataspace (when scene (spawn-background-image level-size-vec scene))
               (spawn-display-controller level-size-vec)
               (spawn-physics-engine)
               (spawn-player-avatar initial-player-x initial-player-y)
               (actions-thunk)
               (wait-for-level-termination))))

(define standard-ground-height 50)

(define (slab left top width #:color [color "purple"])
  (spawn-ground-block (vector left top) (vector width standard-ground-height) #:color color))

(define levels
  (list
   (spawn-level (lambda ()
                  (slab 25 125 100)
                  (slab 50 300 500)
                  (spawn-enemy 100 300 50 550)
                  (spawn-enemy 300 300 50 550 #:facing 'left)
                  (spawn-goal-piece 570 150)
                  (slab 850 300 50)
                  (slab 925 400 50)
                  (slab 975 500 50)
                  (slab 975 600 50)
                  (slab 500 600 150 #:color "orange")))
   (spawn-level (lambda ()
                  (slab 25 300 500)
                  (slab 500 400 500)
                  (slab 1000 500 400)
                  (spawn-goal-piece 1380 500)))
   (spawn-level (lambda ()
                  (slab 25 300 1000)
                  (spawn-enemy 600 300 25 1025 #:facing 'left)
                  (spawn-goal-piece 980 300)))
   (spawn-level (lambda ()
                  (spawn-goal-piece 250 280)
                  (spawn-enemy 530 200 400 600)
                  (spawn-enemy 500 200 -100 1000 #:facing 'left)
                  (slab 400 200 200)
                  (spawn-ground-block (vector 200 280) (vector 200 200) #:color "orange")
                  (slab 25 300 500)
                  (slab 600 1300 600)
                  (slab 1150 1200 25 #:color "red")
                  (for/list ((n 10))
                    (slab 900 (+ 200 (* n 100)) 50)))
                )
   ))

(define (spawn-numbered-level level-number)
  (send! (outbound* 2 (request-gc)))
  (if (< level-number (length levels))
      ((list-ref levels level-number))
      (spawn-standalone-assertions
       (update-sprites #:meta-level 2
                       (let ((message (text "You won!" 72 "red")))
                         (simple-sprite 0
                                        10
                                        100
                                        (image-width message)
                                        (image-height message)
                                        message))))))

(define (spawn-level-spawner starting-level)
  (actor #:name 'level-spawner
   (react (field [current-level starting-level]
                 [level-complete? #f])

          (on (message (level-completed)) (level-complete? #t))

          (on (retracted (level-running))
              (current-level (if (level-complete?) (+ (current-level) 1) (current-level)))
              (level-complete? #f)
              (spawn-numbered-level (current-level)))

          (on-start (spawn-numbered-level starting-level)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sounds

(define (lookup-sound-file sound-number)
  (define sought-prefix (format "sounds/~a__" sound-number))
  (for/or [(filename (in-directory "sounds"))]
    (and (string-prefix? (path->string filename) sought-prefix)
         filename)))

;; TODO: make this a sound driver...
;; TODO: ...and make sound triggering based on assertions of game
;; state, not hardcoding in game logic
(define (play-sound-sequence . sound-numbers)
  (thread (lambda ()
            (for [(sound-number (in-list sound-numbers))]
              (define sound-file (lookup-sound-file sound-number))
              (play-sound sound-file #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define game-level 3) ;; used to specify meta-level to reach external I/O

(spawn-keyboard-integrator)
(spawn-scene-manager)
(dataspace (spawn-score-keeper)
           (spawn-level-spawner 0)
           (forever))
