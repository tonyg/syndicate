#lang syndicate/actor

(require racket/cmdline)
(require racket/port)

(require/activate syndicate/reload)
(require/activate syndicate/supervise)
(require/activate syndicate/drivers/config)
(require/activate syndicate/drivers/web)
(require/activate syndicate/drivers/smtp)

(require "protocol.rkt")

(command-line #:program "webchat"

              #:once-each
              ["--baseurl" baseurl "Specify the base URL for the server"
               (actor #:name (list 'command-line-baseurl baseurl)
                      (stop-when-reloaded)
                      (assert (config 'command-line (list 'baseurl baseurl))))]
              ["--listen" port "Specify HTTP listener port"
               (actor #:name (list 'command-line-listen port)
                      (stop-when-reloaded)
                      (assert (config 'command-line (list 'listen (string->number port)))))]

              #:multi
              [("-o" "--option") key vals "Specify a single configuration option"
               (actor #:name (list 'config-option key vals)
                      (stop-when-reloaded)
                      (assert (config 'command-line
                                      (cons (string->symbol key)
                                            (port->list read (open-input-string vals))))))]
              [("-f" "--config-file") filename "Specify a configuration file to load"
               (spawn-configuration filename filename
                                    #:hook (lambda () (stop-when-reloaded)))])

(actor #:name 'main
       (stop-when-reloaded)

       (during (config _ (list 'baseurl $u)) (assert (server-baseurl u)))
       (during (config _ (list 'listen $p))  (assert (web-virtual-host "http" _ p)))

       (during/actor (config _ (list 'load $module-path))
         #:actor supervise/actor
         #:name (list 'load module-path)
         (reloader-mixin* module-path))

       (during (config _ (list 'smtp $h $u $p $m))
         (assert (smtp-account-config 'smtp-service h #:user u #:password p #:ssl-mode m))))
