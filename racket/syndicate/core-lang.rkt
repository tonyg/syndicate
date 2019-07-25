#lang racket/base

(require (for-syntax racket/base syntax/kerncase))
(require (for-syntax syntax/parse))
(require (for-syntax (only-in racket/list make-list)))

(require racket/match)
(require "main.rkt")
(require (submod "actor.rkt" for-module-begin))
(require "store.rkt")

(provide (rename-out [module-begin #%module-begin])
         activate
         require/activate
         current-ground-dataspace
         begin-for-declarations
	 (except-out (all-from-out racket/base) #%module-begin sleep)
	 (all-from-out racket/match)
	 (all-from-out "main.rkt")
	 (for-syntax (all-from-out racket/base)))

(define-syntax (activate stx)
  (syntax-case stx ()
    [(_ module-path ...)
     #'(begin
         (let ()
           (local-require (submod module-path syndicate-main))
           (activate!))
         ...)]))

(define-syntax (require/activate stx)
  (syntax-case stx ()
    [(_ module-path ...)
     #'(begin
         (require module-path ...)
         (activate module-path ...))]))

(define-syntax-rule (begin-for-declarations decl ...)
  (begin decl ...))

(define current-ground-dataspace (make-parameter #f))

(define-syntax (module-begin stx)
  (unless (eq? (syntax-local-context) 'module-begin)
    (raise-syntax-error #f "allowed only around a module body" stx))
  (syntax-case stx ()
    [(_ forms ...)
     ;; the inclusion of (module+ syndicate-main) is because it seems that the appearance order
     ;; of module+ forms determines the mutual visibility. So syndicate-main is ensured to be the
     ;; first module+ and consequently the main submodule can require it.
     #'(#%module-begin
        (syndicate-module () ((module+ syndicate-main)
                              (module+ main (current-ground-dataspace run-ground))
                              forms ...)))]))

;; Identifier -> Bool
;; Is the identifier a form that shouldn't capture actor actions?
;; note the absence of define-values
(define-for-syntax (kernel-id? x)
  (ormap (lambda (i) (free-identifier=? x i))
         (syntax->list #'(require
                          provide
                          define-values
                          define-syntaxes
                          begin-for-syntax
                          module
                          module*
                          module+
                          #%require
                          #%provide
                          #%declare
                          begin-for-declarations))))

(define-syntax (syndicate-module stx)
  (syntax-parse stx
    [(_ (action-ids ...) (form forms ...))
     (define expanded (local-expand  #'form
                                     'module
                                     (append (list #'module+
                                                   #'begin-for-declarations)
                                             (kernel-form-identifier-list))))
     (syntax-parse expanded
       #:literals (begin define-values)
       [(begin more-forms ...)
        #'(syndicate-module (action-ids ...) (more-forms ... forms ...))]
       [(define-values (x:id ...) e)
        #:with action-id (car (generate-temporaries (list #'form)))
        #:with (tmp ...) (generate-temporaries #'(x ...))
        #`(begin
            (define-values (tmp ...) (values #,@(make-list (length (syntax->list #'(x ...))) #'#f)))
            (define action-id
              (capture-actor-actions
               (lambda () (set!-values (tmp ...) e))))
            (define-values (x ...) (values tmp ...))
            (syndicate-module (action-ids ... action-id) (forms ...)))]
       [(head rest ...)
        (cond
          [(kernel-id? #'head)
           #`(begin #,expanded (syndicate-module (action-ids ...) (forms ...)))]
          [else
           (with-syntax ([action-id (car (generate-temporaries (list #'form)))])
             #`(begin (define action-id (capture-actor-actions (lambda () #,expanded)))
                      (syndicate-module (action-ids ... action-id) (forms ...))))])]
       [non-pair-syntax
        #'(begin form (syndicate-module (action-ids ...) (forms ...)))])]
    [(_ (action-ids ...) ())
     (let ([final-stx
            #`(begin (module+ syndicate-main
                       (provide boot-actions activate!)
                       (define activated? #f)
                       (define boot-actions (list action-ids ...))
                       (define (activate!)
                         (when (not activated?)
                           (set! activated? #t)
                           boot-actions)))
                     (module+ main
                       (require (submod ".." syndicate-main))
                       ((current-ground-dataspace) (activate!))))])
	       ;;(pretty-print (syntax->datum final-stx))
	       final-stx)]))
