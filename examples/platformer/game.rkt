#lang prospect

(require racket
         "./geometry.rkt"
         "./periodic_timer.rkt")

#|

Layers:
- External Events
  * key press/release
- Ground
corresponds to computer itself - device drivers and applications
  
- Game
running application
per-game state, such as score and count-of-deaths
process which spawns levels

- Level
model of the game world
actors represent entities in the world, mostly
misc actors do physicsish things



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




title Display Control Updates

Physics -> DisplayCtl: (pos 'player ...)
note right of DisplayCtl: Compares player pos to level size
DisplayCtl -> Subscribers: (at-meta (at-meta (scroll-offset ...)))








|#