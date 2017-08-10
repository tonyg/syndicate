#lang syndicate

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
               (spawn #:name (list 'command-line-baseurl baseurl)
                      (stop-when-reloaded)
                      (assert (config 'command-line (list 'baseurl baseurl))))]
              ["--listen" port "Specify HTTP listener port"
               (spawn #:name (list 'command-line-listen port)
                      (stop-when-reloaded)
                      (assert (config 'command-line (list 'listen (string->number port)))))]

              #:multi
              [("-o" "--option") key vals "Specify a single configuration option"
               (spawn #:name (list 'config-option key vals)
                      (stop-when-reloaded)
                      (assert (config 'command-line
                                      (cons (string->symbol key)
                                            (port->list read (open-input-string vals))))))]
              [("-f" "--config-file") filename "Specify a configuration file to load"
               (spawn-configuration filename filename
                                    #:hook (lambda () (stop-when-reloaded)))])

(spawn #:name 'main
       (stop-when-reloaded)

       (during (config _ (list 'baseurl $u)) (assert (server-baseurl u)))
       (during (config _ (list 'listen $p))  (assert (web-virtual-host "http" _ p)))

       (during/spawn (config _ (list 'load $module-path))
         #:spawn supervise/spawn
         #:name (list 'load module-path)
         (reloader-mixin* module-path))

       (during (config _ (list 'smtp $h $u $p $m))
         (match h
           [(regexp #px"(.*):(.*)" (list _ host port))
            (assert (smtp-account-config 'smtp-service host #:port (string->number port)
                                         #:user u #:password p #:ssl-mode m))]
           [_
            (assert (smtp-account-config 'smtp-service h #:user u #:password p #:ssl-mode m))])))
