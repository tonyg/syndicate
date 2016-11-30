#lang syndicate/actor

(require racket/file)
(require racket/port)
(require racket/system)

(require/activate syndicate/reload)
(require/activate syndicate/drivers/filesystem)
(require/activate syndicate/drivers/web)

(actor #:name 'script-compiler
       (stop-when-reloaded)
       (define source-filename "../htdocs/webchat.syndicate.js")
       (define target-filename "webchat.js")
       (during/actor (file-content source-filename file->bytes $bs)
         #:name (list 'compiled source-filename)
         (define compiled (with-output-to-bytes
                            (lambda () (system* "../../../js/bin/syndicatec" source-filename))))
         (log-info "Finished compiling ~s" target-filename)
         (on (web-request-get (id req) _ (,target-filename ()))
             (web-respond/bytes! id
                                 #:header (web-response-header
                                           #:headers (list (cons 'content-type
                                                                 "application/javascript")))
                                 compiled))))
