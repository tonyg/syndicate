#lang turnstile

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect Checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for looking at the "effects"
(begin-for-syntax
  (define-syntax ~effs
    (pattern-expander
     (syntax-parser
       [(_ eff:id ...)
        #:with tmp (generate-temporary 'effss)
        #'(~and tmp
                (~parse (eff ...) (stx-or #'tmp #'())))])))

  (define (stx-truth? a)
    (and a (not (and (syntax? a) (false? (syntax-e a))))))
  (define (stx-or a b)
    (cond [(stx-truth? a) a]
          [else b])))

;; DesugaredSyntax EffectName -> (Syntaxof Effect ...)
(define-for-syntax (get-effect e- eff)
  (or (syntax-property e- eff) #'()))

;; DesugaredSyntax EffectName -> Bool
(define-for-syntax (effect-free? e- eff)
  (define prop (syntax-property e- eff))
  (or (false? prop) (stx-null? prop)))

;; DesugaredSyntax -> Bool
(define-for-syntax (pure? e-)
  (for/and ([key (in-list '(ep f s))])
    (effect-free? e- key)))

;; (SyntaxOf DesugaredSyntax ...) -> Bool
(define-for-syntax (all-pure? es)
  (stx-andmap pure? es))