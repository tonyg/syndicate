#lang turnstile

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(require "effects.rkt")
(require (prefix-in syndicate: (only-in syndicate/actor-lang field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax DEBUG-BINDINGS? #f)

(define-for-syntax (int-def-ctx-bind-type-rename x x- t ctx)
  (when DEBUG-BINDINGS?
    (printf "adding to context ~a\n" (syntax-debug-info x)))
  (syntax-local-bind-syntaxes (list x-) #f ctx)
  (syntax-local-bind-syntaxes (list x)
                              #`(make-rename-transformer
                                 (add-orig (assign-type #'#,x- #'#,t #:wrap? #f) #'#,x))
                              ctx))

(define-for-syntax (add-bindings-to-ctx e- def-ctx)
  (syntax-parse e-
        #:literals (erased field/intermediate define/intermediate begin-)
        [(erased (field/intermediate (x:id x-:id τ e-) ...))
         (for ([orig-name (in-syntax #'(x ... ))]
               [new-name (in-syntax #'(x- ...))]
               [field-ty (in-syntax #'(τ ...))])
           (int-def-ctx-bind-type-rename orig-name new-name field-ty def-ctx))]
        [(erased (define/intermediate x:id x-:id τ e-))
         (int-def-ctx-bind-type-rename #'x #'x- #'τ def-ctx)]
        #;[(erased (begin- e ...))
         (for ([e (in-syntax #'(e ...))])
           (add-bindings-to-ctx e def-ctx))]
        [_ (void)]))

(define-for-syntax (display-ctx-bindings ctx)
  (printf "context:\n")
  (for ([x (in-list (internal-definition-context-binding-identifiers ctx))])
    (printf ">>~a\n" (syntax-debug-info x))))

;; -> (Values e-... (Listof Type) (Listof EndpointEffects) (Listof FacetEffects) (Listof SpawnEffects))
;; recognizes local binding forms
;; (field/intermediate [x e] ...
;; (define/intermediate x x- τ e)
(define-for-syntax (walk/bind e...
                              [def-ctx (syntax-local-make-definition-context)]
                              [unique (gensym 'walk/bind)])
  (define-values (rev-e-... rev-τ... ep-effects facet-effects spawn-effects)
    (let loop ([e... (syntax->list e...)]
               [rev-e-... '()]
               [rev-τ... '()]
               [ep-effects '()]
               [facet-effects '()]
               [spawn-effects '()])
      (match e...
        ['()
         (values rev-e-... rev-τ... ep-effects facet-effects spawn-effects)]
        [(cons e more)
         (when (and DEBUG-BINDINGS?
                    (identifier? e))
           (display-ctx-bindings def-ctx)
           (printf "expanding ~a\n" (syntax-debug-info e)))
         (define e- (local-expand e (list unique) (list #'erased #'begin) def-ctx))
         (syntax-parse e-
           #:literals (begin)
           [(begin e ...)
            (loop (append (syntax->list #'(e ...)) more)
                  rev-e-...
                  rev-τ...
                  ep-effects
                  facet-effects
                  spawn-effects)]
           [_
            (define τ (syntax-property e- ':))
            (define-values (ep-effs f-effs s-effs)
              (values (syntax->list (get-effect e- 'ep))
                      (syntax->list (get-effect e- 'f))
                      (syntax->list (get-effect e- 's))))
            (add-bindings-to-ctx e- def-ctx)
            (loop more
                  (cons e- rev-e-...)
                  (cons τ rev-τ...)
                  (append ep-effs ep-effects)
                  (append f-effs facet-effects)
                  (append s-effs spawn-effects))])])))
  (values (reverse rev-e-...)
          (reverse rev-τ...)
          ep-effects
          facet-effects
          spawn-effects))

(define-syntax (field/intermediate stx)
  (syntax-parse stx
    [(_ [x:id x-:id τ e-] ...)
     #'(syndicate:field [x- e-] ...)]))

(define-syntax (define/intermediate stx)
  (syntax-parse stx
    [(_ x:id x-:id τ e)
     #:with x+ (add-orig (assign-type #'x- #'τ #:wrap? #f) #'x)
     ;; including a syntax binding for x allows for module-top-level references
     ;; (where walk/bind won't replace further uses) and subsequent provides
     #'(begin-
         (define-syntax x (make-variable-like-transformer #'x+))
         (define- x+ e))]))

;; copied from ext-stlc
(define-typed-syntax define
  [(_ x:id (~datum :) τ:type e:expr) ≫
   [⊢ e ≫ e- ⇐ τ.norm]
   #:fail-unless (pure? #'e-) "expression must be pure"
   #:with x- (generate-temporary #'x)
   #:with x+ (syntax-local-identifier-as-binding #'x)
   --------
   [⊢ (define/intermediate x+ x- τ.norm e-) (⇒ : ★/t)]]
  [(_ x:id e) ≫
   ;This won't work with mutually recursive definitions
   [⊢ e ≫ e- ⇒ τ]
   #:fail-unless (pure? #'e-) "expression must be pure"
   #:with x- (generate-temporary #'x)
   #:with x+ (syntax-local-identifier-as-binding #'x)
   --------
   [⊢ (define/intermediate x+ x- τ e-) (⇒ : ★/t)]])

(define-typed-syntax begin
  [(_ e_unit ... e) ≫
   #:do [(define-values (e-... τ... ep-effs f-effs s-effs) (walk/bind #'(e_unit ... e)))]
   #:with τ (last τ...)
   --------
   [⊢ (begin- #,@e-...) (⇒ : τ)
      (⇒ ep (#,@ep-effs))
      (⇒ f (#,@f-effs))
      (⇒ s (#,@s-effs))]])