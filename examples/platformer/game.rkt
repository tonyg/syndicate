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
;;     - assertion: Velocity
;;     - assertion: Position
;;     - assertion: Massive
;;     - assertion: Attribute
;;     - assertion: InitialPosition
;;     - role: PhysicsEngine
;;         Maintains positions, velocities and accelerations of all GamePieces.
;;         Uses InitialPosition to place a piece at its creation.
;;         Publishes Velocity and Position to match.
;;         Listens to FrameDescription, using it to advance the simulation.
;;         Takes Impulses as the baseline for moving GamePieces around.
;;         For Massive GamePieces, applies gravitational acceleration.
;;         Computes collisions between GamePieces.
;;         Uses Attributed Aspects of GamePieces to decide what to do in response.
;;         Sometimes, this involves sending Damage.
;;         Responds to JumpRequest by checking whether the named piece is in a
;;         jumpable location, and sets its upward velocity negative if so.
;;         Consumer of LevelSize to figure out regions where, if a GamePiece
;;         crosses into them, Damage should be dealt to the piece.
;;         When the player touches a goal, sends LevelCompleted one layer out and
;;         then kills the world. When the player vanishes from the board, kills
;;         the world.
;;     - role: GamePiece
;;         Maintains private state. Asserts Impulse to move around, asserts Massive
;;         and Attribute as required, asserts InitialPosition to get things
;;         started. May issue JumpRequests at any time. Represents both the player,
;;         enemies, the goal(s), and platforms and blocks in the environment.
;;         Asserts a Sprite two layers out to render itself.

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
;; An ID is a Symbol; the special symbol 'player indicates the player's avatar.
;; Gensyms from (gensym 'enemy) name enemies, etc.
;;
;; A JumpRequest is a (jump-request ID), a message indicating a *request* to jump,
;; not necessarily honoured by the physics engine.
;;
;; An Impulse is an (impulse ID Vec), an assertion indicating a contribution to
;; the net *requested* velocity of the given gamepiece.
;;
;; A Velocity is a (velocity ID Vec), an assertion describing the net *actual*
;; velocity of the named gamepiece.
;;
;; A Position is a (position ID Point), an assertion describing the current actual
;; position of the named gamepiece.
;;
;; A Massive is a (massive ID), an assertion noting that the named gamepiece
;; should be subject to the effects of gravity.
;;
;; An Attribute is an (attribute ID Aspect), an assertion describing some aspect of
;; the named gamepiece
;;
;; An Aspect is either
;;  - 'player - the named piece is a player avatar
;;  - 'enemy - the named piece is an enemy
;;  - 'solid - the named piece can be stood on / jumped from
;;  - 'goal - the named piece, if touched, causes the level to
;;            End The Game In Victory
;;
;; An InitialPosition is an (initial-position ID Point), an assertion specifying
;; not only the *existence* but also the initial position (in World coordinates)
;; of the named gamepiece.
;;
;; A LevelSize is a (level-size Vec), an assertion describing the right-hand and
;; bottom edges of the level canvas (in World coordinates).


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
;; Moveable -> Physics: (massive ID)
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
;; SceneManager

(define window-projection (compile-projection (at-meta (?! (window ? ?)))))
(define scroll-offset-projection (compile-projection (scroll-offset (?!))))

(define (spawn-scene-manager)
  (struct scene-manager-state (size offset) #:prefab)

  (define (update-window-size s p)
    (define-values (added removed) (patch-project/set/single p window-projection))
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
;; Player

(define player-id 'player)

(define (spawn-player-avatar)
  (struct player-state (x y hit-points) #:prefab)
  (define initial-player-state (player-state 50 50 1))
  (define icon character-cat-girl)
  (define icon-width (/ (image-width icon) 2))
  (define icon-height (/ (image-height icon) 2))

  (define (sprite-update s)
    (update-sprites #:meta-level game-level
     (simple-sprite 0
                    (- (player-state-x s) (/ icon-width 2))
                    (- (player-state-y s) (* 13/16 icon-height))
                    icon-width
                    icon-height
                    icon)))

  (spawn (lambda (e s)
           (match-define (player-state x y hit-points) s)
           (match e
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
         (sprite-update initial-player-state)
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LevelSpawner

(define (spawn-level . actions)
  (spawn-world
   (spawn-player-avatar)
   actions))

(define (spawn-numbered-level level-number)
  (match level-number
    [0 (spawn-level (spawn (lambda (e s) #f)
                           (void)
                           (update-sprites #:meta-level game-level
                                           (simple-sprite 0 50 50 50 50
                                                          (rectangle 50 50 "solid" "purple")))))]))

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
