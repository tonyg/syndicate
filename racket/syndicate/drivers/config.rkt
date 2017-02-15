#lang syndicate/actor
;; Monitor configuration files.

(provide (struct-out config)
         spawn-configuration
         define/query-config
         config-ref)

(define-logger syndicate/drivers/config)

(require racket/file)
(require/activate syndicate/drivers/filesystem)

;; (config Any Any)
(struct config (scope item) #:prefab)

(define (spawn-configuration scope path #:hook [hook void])
  (spawn #:name (list 'configuration-monitor scope path)
         (hook)
         (during (file-content path file->list $items)
           (cond
             [(not items)
              (log-syndicate/drivers/config-warning "config ~s is missing" path)]
             [else
              (log-syndicate/drivers/config-info "loading config ~s" path)
              (for [(item items)]
                (log-syndicate/drivers/config-info "config ~s: ~s" path item)
                (assert (config scope item)))]))))

(define-syntax define/query-config
  (syntax-rules ()
    [(_ scope id default)
     (define/query-config id scope id default)]
    [(_ id scope key default)
     (define/query-value id default (config scope (list 'key $val)) val)]))

(define (config-ref #:scope [scope ?] key default)
  (immediate-query (query-value default (config scope (list key $val)) val)))
