#lang syndicate/actor
;; Crude steps toward reloadable Syndicate modules

(provide (except-out (struct-out reload-pending) reload-pending)
         (rename-out [reload-pending <reload-pending>])
         (rename-out [make-reload-pending reload-pending])

         stop-when-reloaded
         spawn-reloader
         spawn-reloader*)

(define-logger syndicate/reload)

(require (for-syntax racket/base))
(require racket/rerequire)

(require/activate syndicate/drivers/filesystem)

(struct reload-pending (filename) #:prefab) ;; ASSERTION

(define-syntax (make-reload-pending stx)
  (syntax-case stx ()
    [(SELF)
     (quasisyntax/loc stx
       (reload-pending '#,(path->string (syntax-source #'SELF))))]))

(define-syntax (stop-when-reloaded stx)
  (syntax-case stx ()
    [(_ body ...)
     (quasisyntax/loc stx
       (stop-when (asserted (reload-pending '#,(path->string (syntax-source stx))))
                  body ...))]))

(define-syntax-rule (spawn-reloader module-path)
  (spawn-reloader* 'module-path))

(define (spawn-reloader* module-path)
  (define mpi (module-path-index-join module-path #f))
  (define rpath (module-path-index-resolve mpi))
  (define path (let ((p (resolved-module-path-name rpath)))
                 (if (pair? p) (car p) p)))
  (if (path? path)
      (spawn-reloader** module-path (path->string path))
      (log-syndicate/reload-error "Could not process module-path ~v" module-path)))

(define counter
  (let ((count 0))
    (lambda (_pathstr)
      (begin0 count
        (set! count (+ count 1))))))

(define (spawn-reloader** module-path pathstr)
  (actor #:name (list 'reloader pathstr)
         (field [reloading? #f])
         (define (reload!)
           (when (not (reloading?))
             (reloading? #t)
             (react (field [obstacles-cleared? #f])
                    (define/query-value obstacles-exist? #f (observe (reload-pending pathstr)) #t
                      #:on-add (log-syndicate/reload-info "waiting to reload ~v" pathstr)
                      #:on-remove (obstacles-cleared? #t))
                    (assert #:when (obstacles-exist?) (reload-pending pathstr))
                    (on-start (flush!)
                              (obstacles-cleared? (not (obstacles-exist?))))
                    (stop-when (rising-edge (obstacles-cleared?))
                               (flush!) ;; Wait one turn for effects of newly-cleared obstacles
                               (log-syndicate/reload-info "(re)loading ~v" pathstr)
                               (dynamic-rerequire module-path)
                               (schedule-actions!
                                ((dynamic-require `(submod ,module-path syndicate-main)
                                                  'activate!)))
                               (reloading? #f)))))

         (field [previous-version #f])
         (define/query-value latest-version 'unknown (file-content pathstr counter $p) p)
         (begin/dataflow
           (when (and (not (eq? (latest-version) 'unknown))
                      (not (equal? (latest-version) (previous-version))))
             (if (latest-version)
                 (reload!)
                 (log-syndicate/reload-warning "Module ~v does not exist" pathstr))
             (previous-version (latest-version))))))
