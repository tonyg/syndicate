#lang prospect

(require racket
         "./geometry.rkt"
         "./periodic_timer.rkt")

#|

Layers:

- External Events
  key press/release (interrupts from the outside world)
  explain timer

- Ground
  corresponds to computer itself
  device drivers
  applications (?)

- Game
  running application
  per-game state, such as score and count-of-deaths
  process which spawns levels
  regular frame ticker

- Level
  model of the game world
  actors represent entities in the world, mostly
  misc actors do physicsish things
  there is physics actor, which could become an "active" network

## Common Data Definitions

(struct vec (x y))
(struct point (x y)) ; C

A Vec is a (vec Number Number)
A Point is a (point Number Number)

## Ground Layer Protocols

 - Rendering
    - assertion: Sprite
    - assertion: ScrollOffset
    - assertion: ScreenSize
    - role: Renderer
        Responds to FrameDescription by drawing the current set of Sprites,
        adjusted via the current ScrollOffset, in a window sized by ScreenSize.

 - Keyboard State
    - message: KeyStateChangeEvent (external event)
    - assertion: KeyDown
    - role: KeyboardIntegrator
        Receives KeyStateChangeEvent from outer space, summing them to
        maintain collections of KeyDown assertions.

 - Timer
    (standard system driver & protocol)

 - Frame Counting
    - message: FrameDescription
    - role: FrameCounter
        Arranges for, and responds to, system timer events, issuing
        FrameDescriptions at regular intervals (a constant compiled-in
        frames-per-second value).

A Sprite is a (sprite (Nat -> Pict) Number Number Nat)
where sprite-renderer produces a pict, given a frame number
      sprite-x is the X coordinate in world coordinates for sprite's top-left
      sprite-y is the Y coordinate in world coordinates for sprite's top-left
      sprite-layer is the layer number for the sprite, more negative for closer
        to the camera.

A ScrollOffset is a (scroll-offset Vec), indicating the vector to *subtract*
from world coordinates to get device coordinates.

A ScreenSize is a (screen-size Vec), indicating the size of the device.

The coordinate systems:
 World coordinates = the large virtual canvas, origin at top-left,
   x increases right and y increases down
 Device coordinates = the window's coordinates, mapped to world coords
   via scroll offset & clipping
 In both, layer numbers are more positive further away from the camera.

A KeyStateChangeEvent is either
 - (key-press KeyEvent) ;; from 2htdp
 - (key-release KeyEvent)
signalling a key state change.

A KeyDown is a (key-down KeyEvent), an assertion indicating that the
named key is currently being held down.

A FrameDescription is a (frame Nat), a message (!)
indicating a boundary between frames; that is, that frame number frame-number
is just about to begin.

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
 - 'player -- the named piece is a player avatar
 - 'enemy - the named piece is an enemy
 - 'furniture - the named piece can be stood on / jumped from
 - 'gold-star - the named piece, if touched, causes the level to
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

(struct velocity (id vect) #:prefab)
(struct damage (id pts) #:prefab)
(struct attr (id
