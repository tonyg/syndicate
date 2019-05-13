#lang turnstile

(provide for/fold
         for/list
         for/set
         for/sum)

(require "core-types.rkt")
(require "sequence.rkt")
(require (only-in "list.rkt" List ~List))
(require (only-in "set.rkt" Set ~Set))
(require (only-in "hash.rkt" Hash ~Hash))
(require (only-in "prim.rkt" Bool + #%datum))
(require (only-in "core-expressions.rkt" let))

(require (postfix-in - (only-in racket/set
                                for/set
                                in-set)))

(begin-for-syntax
  (define-splicing-syntax-class iter-clause
    #:attributes (parend)
    #:datum-literals (:)
    (pattern [x:id seq:expr]
             #:attr parend #'[x seq])
    (pattern [x:id : τ:type seq:expr]
             #:attr parend #'[x : τ seq])
    (pattern [(k:id v:id) hash-seq:expr]
             #:attr parend #'[(k v) hash-seq])
    (pattern (~seq #:when pred:expr)
             #:attr parend #'(#:when pred))
    (pattern (~seq #:unless pred:expr)
             #:attr parend #'(#:unless pred))
    (pattern (~seq #:break pred:expr)
             #:attr parend #'(#:break pred))))

;; a Binding is a (SyntaxList Id Id Type), i.e. #'(x x- τ-x)
(begin-for-syntax
  (struct loop-clause (exp bindings) #:transparent)
  (struct directive (kw exp) #:transparent))


;; (SyntaxListOf LoopClause) -> (Syntax LoopClause- (Binding ...))
(define-for-syntax (analyze-for-clauses clauses)
  (define-values (br binds)
    (for/fold ([body-rev '()]
               [bindings '()])
              ([clause (in-syntax clauses)])
      (match (analyze-for-clause clause bindings)
        [(loop-clause exp bs)
         (values (cons exp body-rev)
                 (append bindings bs))]
        [(directive kw exp)
         (values (list* exp kw body-rev)
                 bindings)])))
  #`(#,(reverse br)
     #,binds))

;; iter-clause (Listof Binding) -> (U iter-clause directive)
(define-for-syntax (analyze-for-clause clause ctx)
  (define/with-syntax ([y y- τ-y] ...) ctx)
  (syntax-parse clause
    #:datum-literals (:)
    [[x:id seq:expr]
     #:and (~typecheck
             [[y ≫ y-- : τ-y] ... ⊢ seq ≫ seq- (⇒ : τ-seq)])
     #:fail-unless (pure? #'seq-) "pure"
     #:with x- (generate-temporary #'x)
     #:do [(define-values (seq-- τ-elems) (make-sequence #'seq- #'τ-seq))]
     (loop-clause (substs #'(y- ...) #'(y-- ...)
                          #`[x- #,seq--]
                          free-identifier=?)
                  (list #`(x x- #,τ-elems)))]
    [[x:id : τ:type seq:expr]
     #:with seq+ (add-expected-type #'seq #'τ.norm)
     #:do [(match-define (list seq- (list (list x- τ-elems)))
             (analyze-for-clause (syntax/loc clause [x seq+])))]
     #:fail-unless (<: τ-elems #'τ.norm) "unexpected type"
     (loop-clause #`[#,x- #,seq-]
                  (list #`(x #,x- τ.norm)))]
    [[(k:id v:id) hash-seq:expr]
     #:and (~typecheck
            [[y ≫ y-- : τ-y] ... ⊢ hash-seq ≫ hash-seq- (⇒ : (~Hash K V))])
     #:fail-unless (pure? #'hash-seq-) "pure"
     #:with (k- v-) (generate-temporaries #'(k v))
     (loop-clause (substs #'(y- ...) #'(y-- ...)
                          #`[(k- v-) (in-hash- hash-seq-)]
                          free-identifier=?)
                  (list #'(k k- K) #'(v v- V)))]
    [(dir:keyword pred)
     #:and (~typecheck
            [[y ≫ y-- : τ-y] ... ⊢ pred ≫ pred- (⇐ : Bool)])
     #:fail-unless (pure? #'pred-) "pure"
     (directive #'dir (substs #'(y- ...) #'(y-- ...)
                              #'pred-
                              free-identifier=?))]))

;; Expression Type -> (Values Expression Type)
;; Determine what kind of sequence we're dealing with;
;; if it's not already in Sequence form, wrap the expression in the appropriate in-* form
;; also figure out what the type of elements are to associate with the loop variable
;; hashes handled separately
(define-for-syntax (make-sequence e τ)
  (syntax-parse τ
    [(~Sequence t)
     (values e #'t)]
    [(~List t)
     (values #`(in-list- #,e) #'t)]
    [(~Set t)
     (values #`(in-set- #,e) #'t)]
    [_
     (type-error #:src e
                 #:msg "not an iterable type: ~a" τ)]))


(define-typed-syntax for/fold
  [(for/fold ([acc:id (~optional (~datum :)) τ-acc init])
             (clause:iter-clause
              ...)
     e-body ...+) ≫
  [⊢ init ≫ init- (⇐ : τ-acc)]
  #:fail-unless (pure? #'init-) "expression must be pure"
  #:with (clauses- ([x x- τ] ...)) (analyze-for-clauses #'(clause.parend ...))
  [[x ≫ x-- : τ] ...
   [acc ≫ acc- : τ-acc] ⊢ (begin e-body ...) ≫ e-body-
                           (⇐ : τ-acc)
                           (⇒ ν-ep (~effs τ-ep ...))
                           (⇒ ν-s (~effs τ-s ...))
                           (⇒ ν-f (~effs τ-f ...))]
  #:with e-body-- (substs #'(x- ...) #'(x-- ...) #'e-body- free-identifier=?)
  -------------------------------------------------------
  [⊢ (for/fold- ([acc- init-])
                clauses-
                e-body--)
     (⇒ : τ-acc)
     (⇒ ν-ep (τ-ep ...))
     (⇒ ν-s (τ-s ...))
     (⇒ ν-f (τ-f ...))]]
  [(for/fold ([acc:id init])
             clauses
     e-body ...+) ≫
   [⊢ init ≫ _ (⇒ : τ-acc)]
   ---------------------------------------------------
   [≻ (for/fold ([acc τ-acc init])
                clauses
        e-body ...)]])

(define-typed-syntax (for/list (clause:iter-clause ...)
                       e-body ...+) ≫
  #:with (clauses- ([x x- τ] ...)) (analyze-for-clauses #'(clause.parend ...))
  [[x ≫ x-- : τ] ... ⊢ (begin e-body ...) ≫ e-body-
                           (⇒ : τ-body)
                           (⇒ ν-ep (~effs τ-ep ...))
                           (⇒ ν-s (~effs τ-s ...))
                           (⇒ ν-f (~effs τ-f ...))]
  #:with e-body-- (substs #'(x- ...) #'(x-- ...) #'e-body- free-identifier=?)
  ----------------------------------------------------------------------
  [⊢ (for/list- clauses-
                e-body--) (⇒ : (List τ-body))
                          (⇒ ν-ep (τ-ep ...))
                          (⇒ ν-s (τ-s ...))
                          (⇒ ν-f (τ-f ...))])

(define-typed-syntax (for/set (clause:iter-clause ...)
                       e-body ...+) ≫
  #:with (clauses- ([x x- τ] ...)) (analyze-for-clauses #'(clause.parend ...))
  [[x ≫ x-- : τ] ... ⊢ (begin e-body ...) ≫ e-body-
                           (⇒ : τ-body)
                           (⇒ ν-ep (~effs τ-ep ...))
                           (⇒ ν-s (~effs τ-s ...))
                           (⇒ ν-f (~effs τ-f ...))]
  #:with e-body-- (substs #'(x- ...) #'(x-- ...) #'e-body- free-identifier=?)
  ----------------------------------------------------------------------
  [⊢ (for/set- clauses-
                e-body--) (⇒ : (Set τ-body))
                          (⇒ ν-ep (τ-ep ...))
                          (⇒ ν-s (τ-s ...))
                          (⇒ ν-f (τ-f ...))])

(define-typed-syntax (for/sum (clause ...)
                       e-body ...+) ≫
  ----------------------------------------------------------------------
  [≻ (for/fold ([acc 0])
               (clause ...)
       (+ acc (let () e-body ...)))])
