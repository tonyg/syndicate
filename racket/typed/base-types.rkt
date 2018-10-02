#lang turnstile

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require (for-syntax turnstile/examples/util/filter-maximal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define ((all-args-are variance) stx)
    (syntax-parse stx
      [(_ t ...)
       (make-list (stx-length #'(t ...)) variance)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-binding-type Role #:arity >= 0 #:bvs = 1)
(define-type-constructor Shares #:arity = 1)
(define-type-constructor Sends #:arity = 1)
(define-type-constructor Reacts #:arity >= 1)
(define-type-constructor Know #:arity = 1)
(define-type-constructor ¬Know #:arity = 1)
(define-type-constructor Stop #:arity >= 1)
(define-type-constructor Message #:arity = 1
  #:arg-variances (all-args-are covariant))
(define-type-constructor Field #:arity = 1)
(define-type-constructor Bind #:arity = 1)
(define-base-types OnStart OnStop OnDataflow MakesField)
(define-for-syntax field-prop-name 'fields)

(define-type-constructor Tuple #:arity >= 0
  #:arg-variances (all-args-are covariant))
(define-type-constructor Observe #:arity = 1
  #:arg-variances (all-args-are covariant))
(define-type-constructor Inbound #:arity = 1
  #:arg-variances (all-args-are covariant))
(define-type-constructor Outbound #:arity = 1
  #:arg-variances (all-args-are covariant))
(define-type-constructor Actor #:arity = 1)
(define-type-constructor AssertionSet #:arity = 1
  #:arg-variances (all-args-are covariant))
(define-type-constructor Patch #:arity = 2
  #:arg-variances (all-args-are covariant))

(define-type-constructor → #:arity > 0)
;; for describing the RHS
;; a value and a description of the effects
(define-type-constructor Computation #:arity = 4)
(define-type-constructor Value #:arity = 1)
(define-type-constructor Endpoints #:arity >= 0)
(define-type-constructor Roles #:arity >= 0)
(define-type-constructor Spawns #:arity >= 0)


(define-base-types Discard ★/t FacetName)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type-constructor U* #:arity >= 0)

(define-for-syntax (prune+sort tys)
  (stx-sort 
   (filter-maximal 
    (stx->list tys)
    typecheck?)))
  
(define-syntax (U stx)
  (syntax-parse stx
    [(_ . tys)
     ;; canonicalize by expanding to U*, with only (sorted and pruned) leaf tys
     #:with ((~or (~U* ty1- ...) ty2-) ...) (stx-map (current-type-eval) #'tys)
     #:with tys- (prune+sort #'(ty1- ... ... ty2- ...))
     (if (= 1 (stx-length #'tys-))
         (stx-car #'tys-)
         (syntax/loc stx (U* . tys-)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; τ.norm in 1st case causes "not valid type" error when referring to ⊥ in another file.
;; however, this version expands the type at every reference, incurring a potentially large
;; overhead---2x in the case of book-club.rkt
;; (copied from ext-stlc example)
(define-syntax define-type-alias
  (syntax-parser
    [(_ alias:id τ)
     #'(define-syntax- alias
         (make-variable-like-transformer #'τ))]
    [(_ (f:id x:id ...) ty)
     #'(define-syntax- (f stx)
         (syntax-parse stx
           [(_ x ...)
            #:with τ:any-type #'ty
            #'τ.norm]))]))

(define-type-alias ⊥ (U*))