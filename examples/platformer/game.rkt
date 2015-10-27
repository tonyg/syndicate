#lang racket/base

(require 2htdp/image)
(require 2htdp/planetcute)

(require racket/set)
(require racket/match)
(require plot/utils) ;; for vector utilities

(require prospect)
(require prospect/drivers/timer)
(require prospect-gl/2d)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layers:
;;
;; - External I/O
;;   as arranged by prospect-gl/2d
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
;;     - role: SceneManager
;;         Displays the scene backdrop and adjusts display coordinates via ScrollOffset.
;;
;; A ScrollOffset is a (scroll-offset Vec), indicating the vector to *subtract*
;; from world coordinates to get device coordinates.
(struct scroll-offset (vec) #:transparent)

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
;;         Sometimes, this involves sending Damage.
;;         Responds to JumpRequest by checking whether the named piece is in a
;;         jumpable location, and sets its upward velocity negative if so.
;;         Consumer of LevelSize to figure out regions where, if a GamePiece
;;         crosses into them, Damage should be dealt to the piece.
;;         When the player touches a goal, sends LevelCompleted one layer out and
;;         then kills the world. When the player vanishes from the board, kills
;;         the world.
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
;;  - 'enemy - the named piece is an enemy
;;  - 'solid - the named piece can be stood on / jumped from
;;  - 'goal - the named piece, if touched, causes the level to
;;            End The Game In Victory
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
;; Various projections

(define window-projection1 (compile-projection (at-meta (?! (window ? ?)))))
(define window-projection3 (compile-projection (at-meta (at-meta (at-meta (?! (window ? ?)))))))
(define scroll-offset-projection (compile-projection (scroll-offset (?!))))
(define key-pressed-projection (compile-projection (at-meta (at-meta (key-pressed (?!))))))
(define position-projection (compile-projection (?! (position ? ? ?))))
(define impulse-projection (compile-projection (?! (impulse ? ?))))
(define game-piece-configuration-projection
  (compile-projection (?! (game-piece-configuration ? ? ? ?))))

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
  (struct scene-manager-state (size offset) #:prefab)

  (define (update-window-size s p)
    (define added (matcher-project/set/single (patch-added p) window-projection1))
    (for/fold [(s s)] [(w added)]
      (match-define (window width height) w)
      (struct-copy scene-manager-state s [size (vector width height)])))

  (define (update-scroll-offset s p)
    (define-values (added removed) (patch-project/set/single p scroll-offset-projection))
    (for/fold [(s s)] [(o added)]
      (match-define (scroll-offset vec) o)
      (struct-copy scene-manager-state s [offset vec])))

  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (let* ((s (update-window-size s p))
                     (s (update-scroll-offset s p)))
                (match-define (vector width height) (scene-manager-state-size s))
                (match-define (vector ofs-x ofs-y) (scene-manager-state-offset s))
                (transition s
                            (update-scene `((push-matrix
                                             (scale ,width ,height)
                                             (texture ,(rectangle 1 1 "solid" "white")))
                                            (translate ,(- ofs-x) ,(- ofs-y)))
                                          `())))]
             [_ #f]))
         (scene-manager-state (vector 0 0) (vector 0 0))
         (sub (window ? ?) #:meta-level 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ScoreKeeper

(define (spawn-score-keeper)
  (spawn (lambda (e s)
           (match e
             [(message (add-to-score delta))
              (transition (+ s delta)
                          (retract (current-score ?))
                          (assert (current-score delta)))]
             [_ #f]))
         0
         (sub (add-to-score ?))))

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
    (define removed (matcher-project/set/single (patch-removed p)
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
    (define added (matcher-project/set/single (patch-added p)
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

  (define ((update-piece g new-pos new-vel) s)
    (define id (game-piece-configuration-id g))
    (transition
     (struct-copy physics-state s
                  [positions (hash-set (physics-state-positions s) id new-pos)]
                  [velocities (hash-set (physics-state-velocities s) id new-vel)])
     (patch-seq (retract (position id ? ?))
                (assert (position id new-pos (game-piece-configuration-size g))))))

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
    (define s (v- q1 q0))
    (define rxs (vcross2 r s))
    (cond [(< (abs rxs) 0.005) #f] ;; zeroish; lines are parallel (and maybe collinear)
          [else
           (define q-p (v- q0 p0))
           (define q-pxs (vcross2 q-p s))
           (define t (/ q-pxs rxs))
           (and (< 0 t 1)
                (let* ((q-pxr (vcross2 q-p r))
                       (u (/ q-pxr rxs)))
                  (and (< 0 u 1)
                       (- t 0.001))))]))

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
             (for/list [(p (in-list (list top-left top-right bottom-right bottom-left)))]
               (min (or (segment-intersection-time p r solid-top-left solid-top-right) 1)
                    (or (segment-intersection-time p r solid-top-left solid-bottom-left) 1)
                    (or (segment-intersection-time p r solid-top-right solid-bottom-right) 1)
                    (or (segment-intersection-time p r solid-bottom-left solid-bottom-right) 1)))))
    (v+ top-left (v* r t)))

  (define (clip-movement-by-solids s p0 p1 size)
    (for/fold [(p1 p1)]
              [((id g) (in-hash (physics-state-configs s)))
               #:when (game-piece-has-attribute? g 'solid)]
      (define p1*
        (clip-movement-by p0 p1 size (piece-pos s id) (game-piece-configuration-size g)))
      p1*))

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
    ;; TODO: collision with enemies
    ((update-piece g final-pos final-vel) s))

  (define (evaluate-jump-request id s)
    (define g (piece-cfg s id))
    (define pos (piece-pos s id))
    (define support (find-support pos (game-piece-configuration-size g) s))
    (and support
         ((update-piece g (v+ pos (vector 0 -1)) jump-vel) s)))

  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (sequence-transitions (transition s '())
                                    (remove-game-piece-configurations p)
                                    (add-game-piece-configurations p)
                                    (update-impulses p))]
             [(message (jump-request id))
              (evaluate-jump-request id s)]
             [(message (at-meta (at-meta (at-meta (frame-event _ _ elapsed-ms _)))))
              (for/fold [(t (transition s '()))]
                        [((id g) (in-hash (physics-state-configs s)))
                         #:when (game-piece-has-attribute? g 'mobile)]
                (transition-bind (update-game-piece elapsed-ms id s) t))]
             [_ #f]))
         (physics-state (hash)
                        (hash)
                        (hash)
                        (hash))
         (sub (impulse ? ?))
         (sub (game-piece-configuration ? ? ? ?))
         (sub (jump-request ?))
         (sub (frame-event ? ? ? ?) #:meta-level game-level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player

(define player-id 'player)

(define (spawn-player-avatar initial-focus-x initial-focus-y)
  (struct player-state (x y hit-points keys-down) #:prefab)

  (define icon character-cat-girl)
  (define icon-width (/ (image-width icon) 2))
  (define icon-height (/ (image-height icon) 2))

  (define icon-hitbox-width (* 2/6 icon-width))
  (define icon-hitbox-height (* 3/10 icon-height))
  (define hitbox-offset-x (/ (- icon-width icon-hitbox-width) 2))
  (define hitbox-offset-y (- (* 13/16 icon-height) icon-hitbox-height))

  (define initial-x (- initial-focus-x (/ icon-hitbox-width 2)))
  (define initial-y (- initial-focus-y icon-hitbox-height))
  (define initial-player-state (player-state initial-x initial-y 1 (set)))

  (define (sprite-update s)
    (update-sprites #:meta-level game-level
     (simple-sprite 0
                    (- (player-state-x s) hitbox-offset-x)
                    (- (player-state-y s) hitbox-offset-y)
                    icon-width
                    icon-height
                    icon)))

  (define ((monitor-position-change p) s)
    (define s1
      (for/fold [(s s)] [(pos (matcher-project/set/single (patch-added p) position-projection))]
        (match-define (position _ (vector nx ny) _) pos)
        (struct-copy player-state s [x nx] [y ny])))
    (transition s1 (sprite-update s1)))

  (define ((integrate-keypresses p) s)
    (transition
     (struct-copy player-state s
                  [keys-down (update-set-from-patch (player-state-keys-down s)
                                                    p
                                                    key-pressed-projection)])
     '()))

  (define ((maybe-jump s0) s)
    (transition s
                (and (not (set-member? (player-state-keys-down s0) #\space))
                     (set-member? (player-state-keys-down s) #\space)
                     (message (jump-request player-id)))))

  (define (update-impulse s)
    (let* ((h-impulse 0)
           (h-impulse (+ h-impulse (if (set-member? (player-state-keys-down s) 'left) -1 0)))
           (h-impulse (+ h-impulse (if (set-member? (player-state-keys-down s) 'right) 1 0))))
      (transition s
                  (patch-seq (retract (impulse player-id ?))
                             (assert (impulse player-id (vector h-impulse 0)))))))

  (spawn (lambda (e s)
           (match-define (player-state x y hit-points keys-down) s)
           (match e
             [(? patch? p)
              (sequence-transitions (transition s '())
                                    (monitor-position-change p)
                                    (integrate-keypresses p)
                                    (maybe-jump s)
                                    update-impulse)]
             [(message (damage _ amount))
              (define new-hit-points (- hit-points amount))
              (if (positive? new-hit-points)
                  (transition (struct-copy player-state s
                                           [hit-points (- hit-points amount)])
                              '())
                  (quit))]
             [_ #f]))
         initial-player-state
         (sub (damage player-id ?))
         (assert (health player-id (player-state-hit-points initial-player-state)))
         (assert (level-running) #:meta-level 1)
         (assert (game-piece-configuration player-id
                                           (vector initial-x initial-y)
                                           (vector icon-hitbox-width icon-hitbox-height)
                                           (set 'player 'mobile 'massive)))
         (sub (position player-id ? ?))
         (sub (key-pressed 'left) #:meta-level 2)
         (sub (key-pressed 'right) #:meta-level 2)
         (sub (key-pressed #\space) #:meta-level 2)
         (sprite-update initial-player-state)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground Block

(define standard-ground-height 50)

(define (spawn-ground-block top-left size #:color [color "purple"])
  (match-define (vector x y) top-left)
  (match-define (vector w h) size)
  (define block-id (gensym 'ground-block))

  (spawn (lambda (e s)
           (match e
             [_ #f]))
         (void)
         (update-sprites #:meta-level game-level
                         (simple-sprite 0 x y w h (rectangle w h "solid" color)))
         (assert (game-piece-configuration block-id
                                           top-left
                                           size
                                           (set 'solid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DisplayControl

(define (spawn-display-controller level-size-vec)
  (struct display-controller-state (window-size player-pos offset-pos) #:prefab)

  (define ((update-window-size p) s)
    (define added (matcher-project/set/single (patch-added p) window-projection3))
    (transition
     (for/fold [(s s)] [(w added)]
       (match-define (window width height) w)
       (struct-copy display-controller-state s [window-size (vector width height)]))
     '()))

  (define (compute-offset pos viewport limit)
    (define half-viewport (/ viewport 2))
    (min (max 0 (- pos half-viewport)) (- limit half-viewport)))

  (define ((update-scroll-offset-from-player-position p) s)
    (define s1
      (for/fold [(s s)] [(pos (matcher-project/set/single (patch-added p)
                                                          position-projection))]
        (match-define (vector ww wh) (display-controller-state-window-size s))
        (match-define (position _ (vector px py) _) pos)
        (struct-copy display-controller-state s
                     [offset-pos (vector (compute-offset px ww (vector-ref level-size-vec 0))
                                         (compute-offset py wh (vector-ref level-size-vec 1)))])))
    (transition s1
                (if (equal? (display-controller-state-offset-pos s)
                            (display-controller-state-offset-pos s1))
                    '()
                    (list (retract (scroll-offset ?))
                          (assert (scroll-offset (display-controller-state-offset-pos s1)))))))

  (spawn (lambda (e s)
           (match e
             [(? patch? p)
              (sequence-transitions (transition s '())
                                    (update-window-size p)
                                    (update-scroll-offset-from-player-position p))]
             [_ #f]))
         (display-controller-state (vector 0 0) (vector 0 0) (vector 0 0))
         (sub (window ? ?) #:meta-level game-level)
         (sub (position player-id ? ?))
         (assert (level-size level-size-vec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LevelSpawner

(define (spawn-standalone-assertions . patches)
  (<spawn> (lambda ()
             (list (patch-seq* patches)
                   (lambda (e s) #f)
                   (void)))))

(define (spawn-level #:initial-player-x [initial-player-x 50]
                     #:initial-player-y [initial-player-y 50]
                     #:level-size level-size-vec
                     . actions)
  (spawn-world
   (spawn-display-controller level-size-vec)
   (spawn-physics-engine)
   (spawn-player-avatar initial-player-x initial-player-y)
   actions))

(define (spawn-numbered-level level-number)
  (match level-number
    [0 (spawn-level #:level-size (vector 4000 800)
                    (spawn-ground-block (vector 400 200) (vector 200 standard-ground-height))
                    (spawn-ground-block (vector 200 280) (vector 200 200)
                                        #:color "orange")
                    (spawn-ground-block (vector 25 300) (vector 500 standard-ground-height)))]))

(define (spawn-level-spawner)
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
               (level-spawner-state 0 #f)
               (sub (level-running))
               (sub (level-completed)))
        (spawn-numbered-level 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define game-level 3) ;; used to specify meta-level to reach external I/O

(2d-world #:width 600 #:height 400
          (spawn-keyboard-integrator)
          (spawn-scene-manager)
          (spawn-world (spawn-score-keeper)
                       (spawn-level-spawner)
                       )
          )
