#lang racket

(provide for-trie/list
         for-trie/set
         for-trie/patch
         for-trie/fold
         for-trie)

(require "core.rkt"
         (only-in "actor.rkt" analyze-pattern)
         (for-syntax racket/syntax)
         (for-syntax syntax/strip-context)
         (for-syntax racket/match))

(begin-for-syntax
  ; Pattern-Syntax Syntax ->
  ; (SyntaxOf TempVar TempVar Projection-Pattern Match-Pattern)
  (define (helper pat-stx outer-stx)
    (match-define (list temp1 temp2) (generate-temporaries #'(tmp1 tmp2)))
    (define-values (proj-stx pat match-pat bindings)
      (analyze-pattern outer-stx pat-stx))
    (list temp1 temp2 proj-stx bindings)))

;; trie projection symbol -> (U set exn:fail?)
;; tries to project the trie. If the resulting trie would be infinite, raise an
;; error, using the third argument to describe the pattern being projected.
;; If the resulting trie is finite, return it as a set.
(define (project-finite t proj pat)
  (define s? (trie-project/set t (compile-projection proj)))
  (unless s?
    (error "pattern projection created infinite trie:" pat))
  s?)

(begin-for-syntax
  (define (build-fold stx ctx)
    (syntax-case stx ()
      [(_ ([acc-id acc-init] ...)
          ()
          body ...)
       #'(begin body ...)]
      [(_ ([acc-id acc-init] ...)
          ((pat_0 trie_0)
           clauses ...)
          body ...)
       (begin
         (match-define (list set-tmp loop-tmp proj-stx match-pat)
           (helper #'pat_0 ctx))
         (with-syntax ([new-acc (generate-temporary 'acc)])
           #`(let ([#,set-tmp (project-finite trie_0 #,proj-stx 'pat_0)])
               (for/fold/derived #,ctx ([acc-id acc-init]
                                        ...)
                 ([loop-tmp (in-set #,set-tmp)])
                 (match loop-tmp
                   [(list #,@match-pat)
                    #,(build-fold
                       #`(_ ([acc-id acc-id]
                             ...)
                            (clauses ...)
                            body ...)
                       ctx)]
                   [_ (values acc-id ...)])))))]
      [(_ ([acc-id acc-init] ...)
          (#:where pred clauses ...)
          body ...)
       #`(if pred
             #,(build-fold #'(_ ([acc-id acc-init] ...) (clauses ...) body ...)
                           ctx)
             (values acc-id ...))])))

(define-syntax (for-trie/fold stx)
  (build-fold stx stx))

(define-syntax (make-fold stx)
  (syntax-case stx ()
    [(_ name folder initial)
     #'(define-syntax (name stx)
         (syntax-case stx ()
           [(_ (clauses (... ...)) body (... ...))
            (with-syntax ([loop #'(for-trie/fold ([acc initial])
                                                 (clauses (... ...))
                                    (folder (begin body (... ...)) acc))])
              (build-fold #'loop stx))]))]))

(make-fold for-trie/list cons empty)

(define (set-folder x acc)
  (set-add acc x))

(make-fold for-trie/set set-folder (set))

(make-fold for-trie/patch patch-seq empty-patch)

(define (ret-second a b) b)

(make-fold for-trie-inner ret-second #f)

(define-syntax (for-trie stx)
  (syntax-case stx ()
    [(_ (clauses ...) body ...)
     (with-syntax ([loop #'(for-trie/fold ([acc (void)])
                                          (clauses ...)
                             (begin body ... acc))])
       (build-fold #'loop stx))]))

(module+ test
  (require rackunit)
  
  (require "route.rkt")
  
  (define (make-trie . vs)
    (for/fold ([acc (trie-empty)])
              ([v (in-list vs)])
      (trie-union acc (pattern->trie 'a v))))

  (struct foo (bar zot) #:prefab)

  ;; This test should pass OK, since we're ignoring all the infinite
  ;; dimensions, and just projecting out a finite one.
  (check-equal? (for-trie/set ([(observe (foo $bar _))
                                (make-trie (observe (foo 1 'a))
                                           (observe (foo 2 'b))
                                           (observe (foo 3 ?)))])
                  bar)
                (set 1 2 3))

  (check-equal? (for-trie/list ([$x (make-trie 1 2 3 4)]
                                #:where (even? x))
                  (+ x 1))
                '(3 5))
  
  (check-equal? (for-trie/set ([$x (make-trie 1 2 3 4)]
                               #:where (even? x))
                  (+ x 1))
                (set 3 5))
  (check-equal? (for-trie/set ([(cons $x _) (make-trie 1 2 (list 0)
                                                       (list 1 2 3)
                                                       (cons 'x 'y)
                                                       (cons 3 4)
                                                       (cons 'a 'b)
                                                       "x" 'foo)])
                  x)
                (set 'x 3 'a))
  (check-equal? (for-trie/fold ([acc 0])
                               ([$x (make-trie 1 2 3 4)]
                                #:where (even? x))
                  (+ acc x))
                6)
  (check-equal? (for-trie/fold ([acc 0])
                               ([$x (make-trie 1 2 3 4)]
                                [x (make-trie 0 1 2 4)]
                                #:where (even? x))
                  (+ acc x))
                6)
  (let-values ([(acc1 acc2)
                (for-trie/fold ([acc1 0]
                                [acc2 0])
                               ([(cons $x $y) (make-trie (cons 1 2)
                                                         (cons 3 8)
                                                         (cons 9 7))])
                  (values (+ acc1 x)
                          (+ acc2 y)))])
    (check-equal? acc1 13)
    (check-equal? acc2 17))
  (check-equal? (for-trie/set ([$x (make-trie 1 2 3)]
                               [$y (make-trie 4 5 6)])
                  (cons x y))
                (set (cons 1 4) (cons 1 5) (cons 1 6)
                     (cons 2 4) (cons 2 5) (cons 2 6)
                     (cons 3 4) (cons 3 5) (cons 3 6)))
  (let ([p (for-trie/patch ([$x (make-trie 1 2 3 4)])
             (retract x))])
    (check-equal? (trie-project/set (patch-removed p) (compile-projection (?!)))
                  (set '(1) '(2) '(3) '(4))))
  (check-equal? (for-trie/set ([$x (make-trie 1 2 3)]
                               [(cons x 3) (make-trie (cons 'x 'y)
                                                      (cons 5 5)
                                                      (cons 2 4)
                                                      (cons 3 3)
                                                      (cons 4 3))])
                  (cons x 4))
                (set (cons 3 4)))
  (check-equal? (for-trie/set ([(cons $x $x) (make-trie 'a 'b
                                                        (cons 'x 'y)
                                                        (cons 2 3)
                                                        3 4
                                                        'x
                                                        (cons 1 1)
                                                        "abc"
                                                        (cons 'x 'x))])
                  x)
                (set 1 'x))
  (check-equal? (for-trie/set ([$x (make-trie 1 2 3)])
                  (void)
                  x)
                (set 1 2 3))
  (check-equal? (for-trie/fold ([acc 0])
                               ([$x (make-trie 1 2 3)])
                  (void)
                  (+ acc x))
                6)
  ;; projecting an infinite set out of an infinite trie raisies an error
  (check-exn (lambda (e) (and (exn:fail? e) (not (exn:fail:contract? e))))
             (lambda ()
               (for-trie/list ([$x (pattern->trie 'x (projection->pattern ?))])
                 x)))
  ;; projecting something finite out is ok
  (check-equal? (for-trie/list ([1 (pattern->trie 'x (projection->pattern ?))])
                  1)
                (list 1))
  (let ([a-set (mutable-set)])
    ;; for-trie results in (void)
    (check-equal? (for-trie ([$x (make-trie 1 2 3 4)])
                    (set-add! a-set x))
                  (void))
    ;; for-trie runs body for effects
    (check-equal? a-set (mutable-set 1 2 3 4))))