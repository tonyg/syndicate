#lang syndicate

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
  (define-runtime-path syndicate-js-dist-path "../../../js/dist")
  (define path->mime-type (make-path->mime-type "/etc/mime.types")))

(spawn #:name 'static-content-server
       (stop-when-reloaded)
       (define static-paths (list htdocs-path syndicate-js-dist-path))
       (define url->path-fns (map make-url->path static-paths))
       (define (url->existing-static-path u)
         (for/or [(url->path (in-list url->path-fns))]
           (define-values (path path-pieces) (url->path u))
           (and (file-exists? path) path)))
       (on (web-request-get (id req) _ ,_)
           (define path (url->existing-static-path
                         (resource->url (web-request-header-resource req))))
           (when path
             (web-respond/bytes! id
                                 #:header (web-response-header #:mime-type (path->mime-type path))
                                 (file->bytes path)))))

(spawn #:name 'template-server
       (stop-when-reloaded)
       (define url->path (make-url->path templates-path))
       (during (api _ (observe (ui-template $name _)))
         (define-values (path path-pieces) (url->path (string->url name)))
         (on-start (log-info "Start observation of ~v" path))
         (on-stop (log-info "Stop observation of ~v" path))
         (during (file-content path file->string $data)
           (assert (api _ (ui-template name data))))))
