#lang racket

(require syndicate/monolithic)
(require syndicate/trie)
(require racket/set)

(module+ test
  (require rackunit))

(define mt-scn (scn trie-empty))

;; an `exp` is either
;; ('begin exp ...) or
;; ('let (var exp) exp) or
;; ('if exp exp exp) or
;; ('send! exp) or
;; ('react O ...) or
;; ('actor ('react O ...)) or
;; ('set! var exp) or
;; ('read var) or
;; ('list exp ...)
;; (primop exp ...) or
;; atom

;; a `val` is either
;; atom or
;; ('list val ...)

;; `primop` is one of
;; + * / - and or not equal? null? car cdr printf

;; an `O` (endpoint) is either
;; ('field var exp) or
;; ('assert exp) or
;; ('on E exp ...) or
;; ('stop-when E exp ...) or
;; ('on-start exp ...)

;; a `facet` is ('react O ...)
;; an `actor` is ('actor facet)

;; an E is either
;; ('asserted pat) or
;; ('retracted pat) or
;; (message pat)

;; a pat is either
;; $var or
;; _ or
;; exp or
;; ('observe pat)
;; ('list pat ...)

;; a Γ is a (Listof Binding)
;; a Binding is (binding var val)
(struct binding (id v) #:transparent)

;; a σ is either
;; (Hashof var val) or
;; (store-concat σ σ)
(struct store-concat (σ1 σ2) #:transparent)
;; σ1 is the "parent" to σ2 (local)

;; a FacetTree is (facet-tree '(react O ...) Γ σ (Listof FacetTree))
(struct facet-tree (stx env sto children) #:transparent)

;; an ActorState is (actor-state π FacetTree)
(struct actor-state (π ft) #:transparent)

;; a π is a trie
(define π-union assertion-set-union)

;; a Program is a (Listof actor)

(define mt-Γ (list))
(define mt-σ (hash))

;; env-lookup : Γ var -> val
;; or throws an error for unbound variables
(define (env-lookup Γ id)
  (match Γ
    ['() (error 'env-lookup "unbound variable: ~v" id)]
    [(cons (binding x v) rest)
     (if (equal? id x)
         v
         (env-lookup rest id))]))

;; extend-env : Γ var val -> Γ
(define (extend-env Γ id v)
  (cons (binding id v) Γ))

;; update-sto : σ var val -> σ
;; update the value of var in the store, if present.
;; otherwise throw an error
(define (update-sto σ id v)
  (let search ([σ σ]
               [k-succ identity]
               [k-fail (lambda () (error 'update-sto "unbound field: ~v" id))])
    (match σ
      [(store-concat σ1 σ2)
       (search σ2
               (lambda (new-σ2) (k-succ (store-concat σ1 new-σ2)))
               (lambda () (search σ1
                                  (lambda (new-σ1) (k-succ (store-concat new-σ1 σ2)))
                                  k-fail)))]
      [_
       (if (hash-has-key? σ id)
           (k-succ (hash-set σ id v))
           (k-fail))])))

(module+ test
  (let* ([s1 (make-store '(balance . 100))]
         [s2 (store-concat mt-σ s1)]
         [s3 (store-concat s1 mt-σ)])
    (check-equal? (update-sto s2 'balance 50)
                  (store-concat mt-σ (make-store '(balance . 50))))
    (check-equal? (update-sto s3 'balance 50)
                  (store-concat (make-store '(balance . 50)) mt-σ))))

(define (primop? x)
  (member x '(+ - * / - and or not equal? null? car cdr printf)))

;; apply-primop : primop (Listof val) -> val
(define (apply-primop op args)
  (match* (op args)
    [('+ `(,v1 ,v2))
     (+ v1 v2)]
    [('- `(,v1 ,v2))
     (- v1 v2)]
    [('* `(,v1 ,v2))
     (* v1 v2)]
    [('/ `(,v1 ,v2))
     (/ v1 v2)]
    [('and `(,v1 ,v2))
     (and v1 v2)]
    [('or `(,v1 ,v2))
     (and v1 v2)]
    [('equal? `(,v1 ,v2))
     (equal? v1 v2)]
    [('not `(,v))
     (not v)]
    [('null? '(list))
     #t]
    [('null? _)
     #f]
    [('car `(list ,e ,es ...))
     e]
    [('cdr `(list ,e ,es ...))
     es]
    [('printf args)
     (apply printf args)]
    [(_ _)
     (error 'apply-primop "invalid primitive application: ~v ~v" op args)]))
    

;; sto-fetch : σ var -> val
;; retrieve the value of field var.
;; if not present throw an error
(define (sto-fetch σ id)
  (let search ([σ σ]
               [k (lambda () (error 'update-sto "unbound field: ~v" id))])
    (match σ
      [(store-concat σ1 σ2)
       (search σ2 (lambda () (search σ1 k)))]
      [_
       (if (hash-has-key? σ id)
           (hash-ref σ id)
           (k))])))

;; make-store : (Listof (cons var val)) -> σ
(define (make-store . vs)
  (make-immutable-hash vs))

;; boot-facet : facet Γ σ -> (Values σ (Listof Action) FacetTree)
(define (boot-facet f Γ σ)
  (define initial-sto (initial-store f Γ σ))
  (match-define-values ((store-concat parent-sto facet-sto) as fs)
                       (eval-start-actions f Γ (store-concat σ initial-sto)))
  (values parent-sto as (facet-tree f Γ facet-sto fs)))

;; initial-store : facet Γ σ -> σ
;; only bad people would put effects here.
(define (initial-store f Γ σ)
  (match-define `(react ,O ...) f)
  (define locations
    (for/fold ([locations (list)])
            ([o (in-list O)])
    (match o
      [`(field ,id ,exp)
       (define-values (v s as fs) (eval-exp exp Γ σ))
       (cons (cons id v) locations)]
      [_ locations])))
  (apply make-store locations))

;; eval-start-actions : facet Γ σ -> (Values σ (Listof Action) (Listof FacetTree))
(define (eval-start-actions f Γ σ)
  (match-define `(react ,O ...) f)
  (for/fold ([sto σ]
             [as (list)]
             [facets (list)])
            ([o (in-list O)])
    (match o
      [`(on-start ,exp ...)
       (define-values (new-sto more-as more-facets) (eval-exp* exp Γ σ))
       (values new-sto (append as more-as) (append facets more-facets))]
      [_ (values sto as facets)])))

;; eval-exp : exp Γ σ -> (Values val σ (Listof Action) (Listof FacetTree))
(define (eval-exp e Γ σ)
  (match e
    [`(react ,O ...)
     (define-values (new-sto as ft) (boot-facet e Γ σ))
     (values (void) new-sto as (list ft))]
    [`(actor (react ,O ...))
     ;; don't pass in parent store
     (define-values (_ as ft) (boot-facet (second e) Γ mt-σ))
     (define assertions (ft-assertions ft mt-σ))
     (define spawn-action (spawn actor-behavior
                                 (actor-state trie-empty ft)
                                 (cons (scn assertions) as)))
     (values (void) σ (list spawn-action) (list))]
    [(? symbol? id)
     (let ([v (env-lookup Γ id)])
       (values v σ (list) (list)))]
    [`(begin ,es ...)
     (for/fold ([v (void)]
                [σ σ]
                [as (list)]
                [facets (list)])
               ([e (in-list es)])
       (define-values (v new-sto more-as more-facets) (eval-exp e Γ σ))
       (values v new-sto (append as more-as) (append facets more-facets)))]
    [`(list ,es ...)
     (define-values (rev-vs new-sto as facets)
       (for/fold ([rev-vs (list)]
                  [σ σ]
                  [as (list)]
                  [facets (list)])
                 ([e (in-list es)])
         (define-values (v new-sto more-as more-facets) (eval-exp e Γ σ))
         (values (cons v rev-vs) new-sto (append as more-as) (append facets more-facets))))
     (values (cons 'list (reverse rev-vs)) new-sto as facets)]
    [`(let (,x ,exp) ,body-exp)
     (define-values (v new-sto as facets) (eval-exp exp Γ σ))
     (define new-Γ (extend-env Γ x v))
     (define-values (result-v final-sto more-as more-facets) (eval-exp body-exp new-Γ new-sto))
     (values result-v final-sto (append as more-as) (append facets more-facets))]
    [`(if ,pred-exp ,then-exp ,else-exp)
     (define-values (pred-v pred-sto as facets) (eval-exp pred-exp Γ σ))
     (define-values (result-v final-sto more-as more-facets)
       (if pred-v
           (eval-exp then-exp Γ pred-sto)
           (eval-exp else-exp Γ pred-sto)))
     (values result-v final-sto (append as more-as) (append facets more-facets))]
    [`(send! ,exp)
     (define-values (v new-sto as facets) (eval-exp exp Γ σ))
     (values (void) new-sto (append as (list (message v))) facets)]
    [`(set! ,id ,exp)
     (define-values (v new-sto as facets) (eval-exp exp Γ σ))
     (define result-sto (update-sto new-sto id v))
     (values (void) result-sto as facets)]
    [`(read ,id)
     (define v (sto-fetch σ id))
     (values v σ (list) (list))]
    [`(,primop ,exp ..1)
     #:when (primop? primop)
     (define-values (args sto as facets)
       (for/fold ([vs (list)]
                  [σ σ]
                  [as (list)]
                  [facets (list)])
                 ([e (in-list exp)])
         (define-values (v new-sto more-as more-facets) (eval-exp e Γ σ))
         (values (cons v vs) new-sto (append as more-as) (append facets more-facets))))
     (define v (apply-primop primop (reverse args)))
     (values v sto as facets)]
    [x (values x σ (list) (list))]))

(module+ test
  ;; sequencing result
  (let-values ([(v s as f) (eval-exp `(begin 1 2 3) mt-Γ mt-σ)])
    (check-equal? v 3))
  ;; variable lookup
  (let-values ([(v s as f) (eval-exp 'x (list (binding 'x 'hello)
                                           (binding 'y 'bye)
                                           (binding 'x 'world))
                                  mt-σ)])
    (check-equal? v 'hello))
  ;; variable binding
  (let-values ([(v s as f) (eval-exp '(let (y 12) 'cake) mt-Γ mt-σ)])
    (check-equal? v ''cake))
  (let-values ([(v s as f) (eval-exp '(let (y 12) y) mt-Γ mt-σ)])
    (check-equal? v 12))
  ;; if
  (let-values ([(v s as f) (eval-exp '(if #f 5 6) mt-Γ mt-σ)])
    (check-equal? v 6))
  (let-values ([(v s as f) (eval-exp '(if #t 5 6) mt-Γ mt-σ)])
    (check-equal? v 5))
  ;; send!
  (let-values ([(v s as f) (eval-exp '(send! 5) mt-Γ mt-σ)])
    (check-equal? as (list (message 5)))
    (check-true (void? v)))
  ;; set!
  (let-values ([(v s as f) (eval-exp '(set! x 12) mt-Γ (make-store '(x . 'hello)))])
    (check-true (void? v))
    (check-equal? (hash-ref s 'x) 12))
  (let-values ([(v s as f) (eval-exp '(begin (set! x (+ 1 (read x)))
                                               (set! x (+ 1 (read x)))
                                               (set! x (+ 1 (read x)))
                                               (set! x (+ 1 (read x)))
                                               (read x))
                                       mt-Γ (make-store '(x . 0)))])
    (check-equal? v 4))
  ;; store read
  (let-values ([(v s as f) (eval-exp '(read x) mt-Γ (make-store '(y . 5) '(x . 'hello)))])
    (check-equal? v ''hello))
  (let-values ([(v s as f) (eval-exp '(+ (- 5 1) (/ 4 (if (not #t) 1 2))) mt-Γ mt-σ)])
    (check-equal? v 6)))

;; dollar-id? : any -> bool
;; test if the input is a symbol whose first character is $
(define (dollar-id? s)
  (and (symbol? s)
       (char=? (string-ref (symbol->string s) 0) #\$)))

;; undollar s : dollar-id? -> var
(define (undollar s)
  (string->symbol (substring (symbol->string s) 1)))

;; eval-pat : pat Γ σ -> meta-pattern
;; if you put effects in your pattern then you deserve bad things
(define (eval-pat pat Γ σ)
  (match pat
    [`(list ,pats ...)
     (cons 'list
           (for/list ([p (in-list pats)])
             (eval-pat p Γ σ)))]
    [`(observe ,pat)
     (observe (eval-pat pat Γ σ))]
    [(? dollar-id? s)
     (?!)]
    ['_ ?]
    [exp
     (define-values (v s as fs) (eval-exp exp Γ σ))
     v]))

;; pat-bindings : pat -> (Listof var)
(define (pat-bindings pat)
  (match pat
    [`(list ,pats ...)
     (flatten (for/list ([p (in-list pats)])
                (pat-bindings p)))]
    [`(observe ,pat)
     (pat-bindings pat)]
    [(? dollar-id? s)
     (list (undollar s))]
    [_ (list)]))

(module+ test
  (check-equal? (pat-bindings '(list hello $x world 5))
                (list 'x)))

;; pat-matches : pat Γ σ π -> (Listof Γ)
;; evaluate the pattern and then project matching assertions
;; out of the given trie.
(define (pat-matches pat Γ σ π)
  (define concrete-pat (eval-pat pat Γ σ))
  (define bindings (pat-bindings pat))
  (define s? (trie-project/set #:take (projection-arity concrete-pat) π concrete-pat))
  (unless s? (error 'pat-matches "pattern resulted in an infinite set: ~v" pat))
  (for/list ([captures (in-set s?)])
    (map binding bindings captures)))

;; E-pat : E -> pat
(define E-pat second)

;; occurrences : E Event π Γ σ -> (Listof Γ)
(define (occurrences E e π-old Γ σ)
  (define pat (E-pat E))
  (match* (E e)
    [(`(message ,_) (message v))
     (pat-matches pat Γ σ (assertion v))]
    [(`(asserted ,_) (scn π-new))
     (define candidates (trie-subtract π-new π-old))
     (pat-matches pat Γ σ candidates)]
    [(`(retracted ,_) (scn π-new))
     (define candidates (trie-subtract π-old π-new))
     (pat-matches pat Γ σ candidates)]
    [(_ _) (list)]))

(module+ test
  (check-equal? (occurrences `(asserted 5) (scn (assertion 5)) trie-empty mt-Γ mt-σ)
                (list (list)))
  (check-equal? (occurrences `(retracted 5) (scn (assertion 5)) trie-empty mt-Γ mt-σ)
                (list))
  (check-equal? (occurrences `(retracted 5) (scn trie-empty) (assertion 5) mt-Γ mt-σ)
                (list (list)))
  (check-equal? (occurrences `(asserted 5) (message 5) trie-empty mt-Γ mt-σ)
                (list))
  (check-equal? (occurrences `(asserted (list 'price $x))
                             (scn (assertion '(list 'price 12)))
                             (assertion '(list 'price 5))
                             mt-Γ mt-σ)
                (list (list (binding 'x 12))))
  (check-equal? (list->set
                 (occurrences `(asserted (list 'price $x))
                              (scn (π-union (assertion '(list 'price 12)) (assertion '(list 'price 16))))
                              (assertion '(list 'price 5))
                              mt-Γ mt-σ))
                (set (list (binding 'x 12)) (list (binding 'x 16)))))

;; subscription : E Γ σ -> π
(define (subscription E Γ σ)
  ;; projection->pattern to convert captures to wildcards
  (assertion (projection->pattern (observe (eval-pat (E-pat E) Γ σ)))))

;; eval-exp* : (Listof exp) Γ σ -> (Values σ (Listof Action) (Listof FacetTree))
;; evaluate a sequence of expressions for effect
(define (eval-exp* exps Γ σ)
  (for/fold ([sto σ]
             [as (list)]
             [facets (list)])
            ([e (in-list exps)])
    (define-values (v new-sto more-as more-facets) (eval-exp e Γ σ))
    (values new-sto (append as more-as) (append facets more-facets))))

;; A Continue is (continue σ (Listof Action) (Listof FacetTree))
(struct continue (sto as fs) #:transparent)
;; A Stop is (stop σ (Listof Action))
(struct stop (sto as) #:transparent)

;; run-facet : facet π σ Γ Event -> (U Continue Stop)
(define (run-facet f π-old σ Γ e)
  (match-define `(react ,O ...) f)
  (for/fold ([s (continue σ (list) (list))])
            ([o (in-list O)])
    (match s
      [(stop _ _) s]
      [(continue σ as facets)
       (match (run-endpoint o π-old σ Γ e)
         [(stop σ as)
           ;; ok to discard previous as here I guess
          (stop σ as)]
         [(continue new-sto more-as more-facets)
          (continue new-sto (append as more-as) (append facets more-facets))])])))
      

;; run-endpoint : O π σ Γ Event -> (U Continue Stop)
(define (run-endpoint O π-old σ Γ e)
  (match O
    [`(field ,_ ,_)
     (continue σ (list) (list))]
    [`(on-start ,exp ...)
     (continue σ (list) (list))]
    [`(assert ,exp)
     (continue σ (list) (list))]
    [`(stop-when ,E ,exps ...)
     (define bindings (occurrences E e π-old Γ σ))
     (cond
       [(empty? bindings)
        (continue σ (list) (list))]
       [else
        (define-values (sto as)
          (for/fold ([sto σ]
                     [as (list)])
                    ([captures (in-list bindings)])
            (define extended-env (append captures Γ))
            ;; seems like we should ignore new facets here?
            (define-values (new-sto more-as facets) (eval-exp* exps extended-env sto))
            (values new-sto (append as more-as))))
        (stop sto as)])]
    [`(on ,E ,exps ...)
     (define bindings (occurrences E e π-old Γ σ))
     (cond
       [(empty? bindings)
        (continue σ (list) (list))]
       [else
        (define-values (sto as facets)
          (for/fold ([sto σ]
                     [as (list)]
                     [facets (list)])
                    ([captures (in-list bindings)])
            (define extended-env (append captures Γ))
            (define-values (new-sto more-as more-facets) (eval-exp* exps extended-env sto))
            (values new-sto (append as more-as) (append facets more-facets))))
        (continue sto as facets)])]))

;; endpoint-assertions : O Γ σ -> π
;; IGNORE effects from such expressions (yadda yadda evil yadda yadda)
(define (endpoint-assertions O Γ σ)
  (match O
    [`(field ,_ ,_)
     trie-empty]
    [`(on-start ,exp ...)
     trie-empty]
    [`(assert ,exp)
     (define-values (v new-sto as facets) (eval-exp exp Γ σ))
     (assertion v)]
    [`(stop-when ,E ,exps ...)
     (subscription E Γ σ)]
    [`(on ,E ,exps ...)
     (subscription E Γ σ)]))

;; facet-assertions : facet Γ σ -> π
(define (facet-assertions f Γ σ)
  (match-define `(react ,O ...) f)
  (for/fold ([π trie-empty])
            ([o (in-list O)])
    (π-union π (endpoint-assertions o Γ σ))))


;; an OK is (ok σ FacetTree (ListofAction))
(struct ok (sto ft as) #:transparent)
;; run-facets : FacetTree π σ Event -> (U OK (Listof Action))
(define (run-facets ft π parent-sto e)
  (match-define (facet-tree stx env sto children) ft)
  (define facet-sto (store-concat parent-sto sto))
  ;; I'm really not confident about the way the stores are being handled here
  (match (run-facet stx π facet-sto env e)
    [(continue new-sto as new-facets)
     (define-values (final-sto final-as new-children)
       (for/fold ([sto new-sto]
                  [as as]
                  [new-children (list)])
                 ([ft (in-list (append children new-facets))])
         (match (run-facets ft π sto e)
           [(ok new-sto new-ft more-as)
            (values new-sto
                    (append as more-as)
                    ;; n^2 but let's keep the order the same
                    (append new-children (list new-ft)))]
           [more-as
            (values sto
                    (append as more-as)
                    new-children)])))
     (match-define (store-concat new-parent-sto new-facet-sto) final-sto)
     (ok new-parent-sto (facet-tree stx env new-facet-sto new-children) final-as)]
    [(stop _ as)
     as]))

;; ft-assertions : FacetTree σ -> π
(define (ft-assertions ft σ)
  (match-define (facet-tree stx env sto children) ft)
  (define extended-sto (store-concat σ sto))
  (for/fold ([π (facet-assertions stx env extended-sto)])
            ([f (in-list children)])
    (π-union π (ft-assertions f env extended-sto))))

;; actor-behavior : ActorState Event -> Transition
;; leaf behavior function
(define (actor-behavior e s)
  (cond
    [e
     (with-handlers ([exn:fail? (lambda (e) (quit #:exception e (list)))])
       (match-define (actor-state π-old ft) s)
       (match (run-facets ft π-old mt-σ e)
         [(ok _ ft as)
          (define assertions (ft-assertions ft mt-σ))
          (define next-π (if (scn? e) (scn-trie e) π-old))
          (transition (actor-state next-π ft)
                      (cons (scn assertions) as))]
         [as
          (quit as)]))]
    [else #f]))

;; run : Program -> Syndicate
(define (run p)
  (define boot-actions
    (for/list ([boot (in-list p)])
      (match-define-values (v _ (list s) fs) (eval-exp boot mt-Γ mt-σ))
      s))
  (run-ground boot-actions))

(define test-program
  `((actor (react (on-start (printf "hello,world\n"))))))

(define test-program2
  `(
    (actor (react (on (asserted 5)
                      (printf "wat\n"))))
    (actor (react (assert 5)))))

(define ping-pong
  `(
    (actor (react (on (message 'ping)
                      (printf "ping\n")
                      (send! 'pong))))
    (actor (react (on (message 'pong)
                      (printf "pong\n")
                      (send! 'ping))
                  (on-start (send! 'ping))))))

(define bank-account
  `(
    (actor (react (field balance 0)
                  (assert (list 'account (read balance)))
                  (on (message (list 'deposit $amount))
                      (set! balance (+ (read balance) amount)))))

    (actor (react (on (asserted (list 'account $balance))
               (printf "Balance changed to ~a\n" balance))))

    (actor (react (stop-when (asserted (observe (list 'deposit _)))
                              (send! (list 'deposit +100))
                              (send! (list 'deposit -30)))))))

(run bank-account)