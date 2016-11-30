#lang syndicate/actor

(require/activate syndicate/reload)

(spawn-reloader "config.rkt")
(spawn-reloader "trust.rkt")
(spawn-reloader "api.rkt")
(spawn-reloader "script-compiler.rkt")
(spawn-reloader "static-content.rkt")
(spawn-reloader "account.rkt")
(spawn-reloader "pages.rkt")
