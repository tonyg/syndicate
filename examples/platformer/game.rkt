#lang prospect

(require racket/set)
(require racket/match)
(require prospect/drivers/timer)
(require plot/utils) ;; for vector utilities
(require prospect-gl/2d)

#|

Layers:

- External I/O
  as arranged by prospect-gl/2d
  including keyboard events, interface to rendering, and frame timing

- Ground
  corresponds to computer itself
  device drivers
  applications (e.g. in this instance, the game)

- Game
  running application
  per-game state, such as score and count-of-deaths
  process which spawns levels
  regular frame ticker

- Level
  model of the game world
  actors represent entities in the world, mostly
  misc actors do physicsish things

## Common Data Definitions

A Vec is a (vector Number Number)
A Point is a (vector Number Number)
(See vector functions in plot/utils)

## Ground Layer Protocols

 - Scene Management
    - assertion: ScrollOffset
    - role: SceneManager
        Displays the scene backdrop and adjusts display coordinates via ScrollOffset.

A ScrollOffset is a (scroll-offset Vec), indicating the vector to *subtract*
from world coordinates to get device coordinates.

## Game Layer Protocols

 - Scoring
    - message: AddToScore
    - assertion: CurrentScore
    - role: ScoreKeeper
        Maintains the score as private state.
        Publishes the score using a CurrentScore.
        Responds to AddToScore by updating the score.

 - Level Spawning
    - assertion: LevelRunning
    - message: LevelCompleted
    - role: LevelSpawner
        Maintains the current level number as private state.
        Spawns a new Level when required.
        Monitors LevelRunning - when it drops, the level is over.
        Receives LevelCompleted messages. If LevelRunning drops without
        a LevelCompleted having arrived, the level ended in failure and
        should be restarted. If LevelComplete arrived before LevelRunning
        dropped, the level was completed successfully, and the next level
        should be presented.
    - role: Level
        Running level instance. Maintains LevelRunning while it's still
        going. Sends LevelCompleted if the player successfully completed
        the level.

An AddToScore is an (add-to-score Number), a message
which signals a need to add the given number to the player's
current score.

A CurrentScore is a (current-score Number), an assertion
indicating the player's current score.

A LevelRunning is a (level-running), an assertion indicating that the
current level is still in progress.

A LevelCompleted is a (level-completed), a message indicating that
the current level was *successfully* completed before it terminated.

## Level Layer Protocols

 - Movement and Physics
    - message: JumpRequest
    - assertion: Impulse
    - assertion: Velocity
    - assertion: Position
    - assertion: Massive
    - assertion: Attribute
    - assertion: InitialPosition
    - role: PhysicsEngine
        Maintains positions, velocities and accelerations of all GamePieces.
        Uses InitialPosition to place a piece at its creation.
        Publishes Velocity and Position to match.
        Listens to FrameDescription, using it to advance the simulation.
        Takes Impulses as the baseline for moving GamePieces around.
        For Massive GamePieces, applies gravitational acceleration.
        Computes collisions between GamePieces.
        Uses Attributed Aspects of GamePieces to decide what to do in response.
        Sometimes, this involves sending Damage.
        Responds to JumpRequest by checking whether the named piece is in a
        jumpable location, and sets its upward velocity negative if so.
        Consumer of LevelSize to figure out regions where, if a GamePiece
        crosses into them, Damage should be dealt to the piece.
        When the player touches a goal, sends LevelCompleted one layer out and
        then kills the world. When the player vanishes from the board, kills
        the world.
    - role: GamePiece
        Maintains private state. Asserts Impulse to move around, asserts Massive
        and Attribute as required, asserts InitialPosition to get things
        started. May issue JumpRequests at any time. Represents both the player,
        enemies, the goal(s), and platforms and blocks in the environment.
        Asserts a Sprite two layers out to render itself.

 - Player State
    - message: Damage
    - assertion: Health
    - role: Player
        Maintains hitpoints, which it reflects using Health.
        Responds to Damage.
        When hitpoints drop low enough, removes the player from the board.

 - World State
    - assertion: LevelSize
    - role: DisplayControl
        Maintains a LevelSize assertion.
        Observes the Position of the player, and computes and maintains a
        ScrollOffset two layers out, to match.

An ID is a Symbol; the special symbol 'player indicates the player's avatar.
Gensyms from (gensym 'enemy) name enemies, etc.

A JumpRequest is a (jump-request ID), a message indicating a *request* to jump,
not necessarily honoured by the physics engine.

An Impulse is an (impulse ID Vec), an assertion indicating a contribution to
the net *requested* velocity of the given gamepiece.

A Velocity is a (velocity ID Vec), an assertion describing the net *actual*
velocity of the named gamepiece.

A Position is a (position ID Point), an assertion describing the current actual
position of the named gamepiece.

A Massive is a (massive ID), an assertion noting that the named gamepiece
should be subject to the effects of gravity.

An Attribute is an (attribute ID Aspect), an assertion describing some aspect of
the named gamepiece

An Aspect is either
 - 'player - the named piece is a player avatar
 - 'enemy - the named piece is an enemy
 - 'solid - the named piece can be stood on / jumped from
 - 'goal - the named piece, if touched, causes the level to
           End The Game In Victory

An InitialPosition is an (initial-position ID Point), an assertion specifying
not only the *existence* but also the initial position (in World coordinates)
of the named gamepiece.

A Damage is a (damage ID Number), a message indicating an event that should
consume the given number of health points of the named gamepiece.

A Health is a (health ID Number), an assertion describing the current hitpoints
of the named gamepiece.

A LevelSize is a (level-size Vec), an assertion describing the right-hand and
bottom edges of the level canvas (in World coordinates).

-----------
Interaction Diagrams (to be refactored into the description later)

================================================================================

title Jump Sequence

Player -> Physics: (jump 'player)
note right of Physics: Considers the request.
note right of Physics: Denied -- Player is not on a surface.

Player -> Physics: (jump 'player)
note right of Physics: Considers the request.
note right of Physics: Accepted.
note right of Physics: Updates velocity, position
Physics -> Subscribers: (vel 'player ...)
Physics -> Subscribers: (pos 'player ...)


================================================================================

title Display Control Updates

Physics -> DisplayCtl: (pos 'player ...)
note right of DisplayCtl: Compares player pos to level size
DisplayCtl -> Subscribers: (at-meta (at-meta (scroll-offset ...)))

================================================================================

title Movement Sequence

Moveable -> Physics: (massive ID)
Moveable -> Physics: (attr ID ...)
Moveable -> Physics: (impulse ID vec)
note right of Physics: Processes simulation normally
Physics -> Subscribers: (pos ID ...)
Physics -> Subscribers: (vel ID ...)

================================================================================

title Keyboard Interpretation

Keyboard -> Player: (press right-arrow)
Player -->> Physics: assert (impulse ID (vec DX 0))

note right of Physics: Processes simulation normally

Keyboard -> Player: (press left-arrow)
Player -->> Physics: assert (impulse ID (vec 0 0))

Keyboard -> Player: (release right-arrow)
Player -->> Physics: assert (impulse ID (vec -DX 0))

Keyboard -> Player: (press space)
Player -> Physics: (jump)


|#

;;---------------------------------------------------------------------------
;; Keyboard and Display

;; A KeyStateChangeEvent is either
;;  - (key-press KeyEvent) ;; from 2htdp
;;  - (key-release KeyEvent)
;; signalling a key state change.
(struct key-press (key) #:prefab)
(struct key-release (key) #:prefab)

;; A ScreenSize is a (screen-size Vec), indicating the size of the device.
(struct screen-size (vec) #:prefab)

;; The canvas here both delivers keyboard events and serves as a
;; display medium.
(define game-canvas%
  (class canvas%
    (init-field key-handler)
    (super-new)
    (define/override (on-char event)
      (match (send event get-key-code)
        ['release (key-handler (key-release (send event get-key-release-code)))]
        [other    (key-handler (key-press other))]))))

;; Construct, show and return a game-canvas%.
;; Keypresses will result in ground-messages.
(define (make-frame width height)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "Prospect Platformer"]
                       [width width]
                       [height height]))
    (define canvas
      (new game-canvas%
           [parent frame]
           [key-handler send-ground-message]))
    (send canvas focus)
    (send frame show #t)
    canvas))

;; -> KeyboardIntegrator
(define (spawn-keyboard-driver)
  (spawn (lambda (e s)
           ...)
         (void)
         (sub (
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spawn-timer-driver)
(spawn-keyboard-driver)
;;(spawn-display-driver)

(let ((canvas (make-frame 600 400)))
  ;; Retrieve the actual displayed size of the canvas, which differs
  ;; from the requested frame size because of window chrome etc.
  (define the-screen-size
    (let-values (((x-max y-max) (send canvas get-client-size)))
      (screen-size (vector x-max y-max))))
  (define the-dc (send canvas get-dc))
  ;;
  ;; So equipped, we may spawn the renderer.
  (spawn-renderer the-screen-size the-dc))
