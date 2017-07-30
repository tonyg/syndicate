#lang syndicate/actor

(require/activate syndicate/reload)

(spawn-reloader "config.rkt")
(spawn-reloader "session.rkt")
(spawn-reloader "channel.rkt")
