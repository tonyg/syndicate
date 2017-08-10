#lang syndicate

(require/activate syndicate/reload)
(require/activate syndicate/supervise)
(require/activate syndicate/drivers/config)

(require "protocol.rkt")

(spawn #:name 'config
       (stop-when-reloaded)

       (assert (ircd-motd (list "Hello, world!")))

       (assert (ircd-listener 6667)))
