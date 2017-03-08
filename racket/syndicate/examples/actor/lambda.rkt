#lang racket/base ;; !

(require racket/match)
(require racket/set)
(require racket/syntax)

(module+ test (require rackunit))

(require syndicate/actor)
(require syndicate/ground)
(require syndicate/trie)
(require syndicate/patch)
(require syndicate/protocol/standard-relay)

(define (fv term)
  (let walk ((term term))
    (match term
      [`(lambda (,var ...) ,body)
       (set-subtract (walk body) (list->seteq var))]
      [`(,rator ,rand ...)
       (apply set-union (seteq) (map walk (cons rator rand)))]
      [(? symbol? x)
       (seteq x)]
      [_
       (seteq)])))

(module+ test
  (check-equal? (fv '(lambda (x) x)) (seteq))
  (check-equal? (fv '(lambda (x) y)) (seteq 'y))
  (check-equal? (fv '((lambda (x) x) z)) (seteq 'z))
  (check-equal? (fv '(lambda (y) ((lambda (x) y) z))) (seteq 'z))
  (check-equal? (fv '(w z)) (seteq 'w 'z))
  (check-equal? (fv '123) (seteq))
  (check-equal? (fv '"hi") (seteq))
  (check-equal? (fv '((lambda (x) x) 123)) (seteq))
  (check-equal? (fv '((lambda (x) (y x)) 123)) (seteq 'y))
  )

(define (compile top-term)
  (define actors '())

  (define (emit-actor! a)
    (set! actors (cons a actors)))

  (define ($ v)
    (format-symbol "$~a" v))

  (define (compile-lambda-body n fvs vars body)
    `(spawn (during (observe (rpc (list (list ',n ,@(map $ fvs)) ,@(map $ vars)) _))
              (on-start (react ,(compile-term body
                                              (lambda (v)
                                                `(assert (rpc (list (list ',n ,@fvs) ,@vars)
                                                              ,v)))))))))

  (define (gensym/intern base)
    (string->symbol (symbol->string (gensym base))))

  (define (compile-term term k)
    (match term
      [`(lambda (,var ...) ,body)
       (define n (gensym/intern 'clo)) ;; could get away with plain gensym here?
       (define fvs (set->list (set-subtract (fv body) (list->seteq var)))) ;; arbitrary order
       (emit-actor! (compile-lambda-body n fvs var body))
       (k `(list ',n ,@fvs))]
      [`(,rator0 ,rand0 ...)
       (compile-term rator0
                     (lambda (rator)
                       (let ca ((rands rand0) (acc-rev '()))
                         (match rands
                           ['()
                            (define v (gensym/intern 'v))
                            `(stop-when (asserted (rpc (list ,rator ,@(reverse acc-rev)) ,($ v)))
                                        (react ,(k v)))]
                           [(cons r rest)
                            (compile-term r (lambda (rv) (ca rest (cons rv acc-rev))))]))))]
      [(? symbol? x)
       (k x)]
      [lit
       (k lit)]))

  (emit-actor! `(spawn ,(compile-term top-term (lambda (v) `(assert (outbound (answer ,v)))))))

  (reverse actors))

(define (primitive-handlers)
  (list `(spawn (during (observe (rpc (list + $a $b) _))
                  (assert (rpc (list + a b) (+ a b)))))))

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define (eval-compiled forms)
  (local-require racket/pretty)
  (struct answer (v) #:prefab)
  (define program `(let ()
                     (struct rpc (q a) #:prefab)
                     (struct answer (v) #:prefab)
                     (values ;;time
                      (run-ground
                       ,@(primitive-handlers)
                       ,@forms))))
  ;; (pretty-print program)
  (trie-project/set/single (eval program ns) (answer (?!))))

(module+ test
  (check-equal? (eval-compiled (compile `((lambda (x) x) 123))) (set 123))
  (check-equal? (eval-compiled (compile `(+ 123 234))) (set 357))
  (check-equal? (eval-compiled (compile `((lambda (x y) (+ x y)) 123 234))) (set 357))
  (check-equal? (eval-compiled (compile `((lambda (x y) (+ x y)) ((lambda (v) (+ v 1)) 122) 234))) (set 357))
  (check-equal? (eval-compiled (compile `((lambda (inc)
                                            (+ (inc 2) (inc 3)))
                                          (lambda (v) (+ v 1)))))
                (set 7))
  )
