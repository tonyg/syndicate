#lang racket/base

(require (for-syntax racket/base syntax/kerncase))

(require racket/match)
(require "main.rkt")

(provide (rename-out [module-begin #%module-begin])
         activate
         require/activate
	 (except-out (all-from-out racket/base) #%module-begin)
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

(define-syntax (module-begin stx)
  (unless (eq? (syntax-local-context) 'module-begin)
    (raise-syntax-error #f "allowed only around a module body" stx))
  (syntax-case stx ()
    [(_ forms ...)
     (let ()
       (define (accumulate-actions action-ids final-forms forms)
	 (if (null? forms)
	     (let ((final-stx
		    #`(#%module-begin #,@(reverse final-forms)
                                      (module+ syndicate-main
                                        (provide boot-actions activate!)
                                        (define activated? #f)
                                        (define boot-actions (list #,@(reverse action-ids)))
                                        (define (activate!)
                                          (when (not activated?)
                                            (set! activated? #t)
                                            boot-actions)))
                                      (module+ main
                                        (require (submod ".." syndicate-main))
                                        (run-ground (activate!))))))
	       ;;(pretty-print (syntax->datum final-stx))
	       final-stx)
	     (syntax-case (local-expand (car forms)
					'module
                                        (kernel-form-identifier-list)) ()
	       [(head rest ...)
		(if (free-identifier=? #'head #'begin)
		    (accumulate-actions action-ids
					final-forms
					(append (syntax->list #'(rest ...)) (cdr forms)))
		    (if (ormap (lambda (i) (free-identifier=? #'head i))
			       (syntax->list #'(define-values define-syntaxes begin-for-syntax
						 module module*
						 #%module-begin
						 #%require #%provide)))
			(accumulate-actions action-ids
					    (cons (car forms) final-forms)
					    (cdr forms))
			(accumulate-action (car forms) action-ids final-forms (cdr forms))))]
	       [non-pair-syntax
		(accumulate-action (car forms) action-ids final-forms (cdr forms))])))
       (define (accumulate-action action action-ids final-forms remaining-forms)
	 (define temp (car (generate-temporaries (list action))))
	 (accumulate-actions (cons temp action-ids)
			     (cons #`(define #,temp #,action) final-forms)
			     remaining-forms))
       (accumulate-actions '() '() (syntax->list #'(forms ...))))]))
