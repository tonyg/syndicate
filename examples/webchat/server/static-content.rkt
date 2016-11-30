#lang syndicate/actor

(require racket/file)
(require racket/runtime-path)
(require net/url)
(require web-server/dispatchers/filesystem-map)
(require web-server/private/mime-types)

(require "protocol.rkt")

(require/activate syndicate/reload)
(require/activate syndicate/drivers/filesystem)
(require/activate syndicate/drivers/web)

(begin-for-declarations
  (define-runtime-path htdocs-path "../htdocs")
  (define-runtime-path templates-path "../htdocs/templates")
  (define path->mime-type (make-path->mime-type "/etc/mime.types")))

(actor #:name 'static-content-server
       (stop-when-reloaded)
       (define url->path (make-url->path htdocs-path))
       (on (web-request-get (id req) _ ,_)
           (define-values (path path-pieces)
             (url->path (resource->url (web-request-header-resource req))))
           (when (file-exists? path)
             (web-respond/bytes! id
                                 #:header (web-response-header #:mime-type (path->mime-type path))
                                 (file->bytes path)))))

(actor #:name 'template-server
       (stop-when-reloaded)
       (define url->path (make-url->path templates-path))
       (during (api _ (observe (ui-template $name _)))
         (define-values (path path-pieces) (url->path (string->url name)))
         (log-info "Observation of ~v" path)
         (during (file-content path file->string $data)
           (assert (api _ (ui-template name data))))))
