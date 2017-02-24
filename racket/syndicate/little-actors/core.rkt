#lang racket

(require syndicate/monolithic)
(require syndicate/trie)
(require racket/set)
(require syndicate/upside-down)
(require syndicate/monitor)
(require racket/async-channel)
(require (for-syntax syntax/parse))
(require rackunit)
(require racket/engine)

(define mt-scn (scn trie-empty))

;; an `exp` is either
;; ('lambda (var ...) exp) or
;; (exp exp ...) or
;; ('begin exp ...) or
;; ('let (var exp) exp) or
;; ('if exp exp exp) or
;; ('send! exp) or
;; ('react O ...) or
;; ('actor ('react O ...)) or
;; ('dataspace actor ...) or
;; ('outbound exp) or
;; ('inbound exp) or
;; ('set! var exp) or
;; ('read var) or
;; ('list exp ...) or
;; (primop exp ...) or
;; atom

;; a `val` is either
;; atom or
;; ('list val ...) or
;; (outbound val) or
;; (inbound val) or
;; (closure Γ ('lambda (var ...) exp))
(struct closure (env fun) #:transparent)

;; `primop` is one of
;; + * / - and or not equal? null? car cdr printf

;; an `O` (endpoint) is either
;; ('field var exp) or
;; ('assert exp) or
;; ('on E exp ...) or
;; ('stop-when E exp ...) or
;; ('on-start exp ...) or
;; ('on-stop exp ...)

;; a `facet` is ('react O ...)
;; an `actor` is
;; ('actor facet) or
;; ('dataspace actor ...)

;; an E is either
;; ('asserted pat) or
;; ('retracted pat) or
;; (message pat)

;; a pat is either
;; $var or
;; _ or
;; exp or
;; ('observe pat) or
;; ('inbound pat) or
;; ('outbound pat) or
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

;; A (Continue A) is (continue A σ (Listof Action) (Listof FacetTree))
(struct continue (v sto as fs) #:transparent)
;; A Stop is (stop σ (Listof Action))
(struct stop (sto as) #:transparent)
;; A (Result A) is a Stop or (Continue A)

;; result-bind : Result (Any σ Any ... -> Result) Any ... -> Result
(define (result-bind r f . extra-args)
  (match r
    [(continue v σ as fs)
     (match (apply f (cons v (cons σ extra-args)))
       [(continue next-v next-σ more-as more-fs)
        (continue next-v next-σ (append as more-as) (append fs more-fs))]
       [s s])]
    [s s]))

;; result-map : (Result A) (A -> B) -> (Result B)
(define (result-map f r)
  (match r
    [(continue v σ as fs)
     (continue (f v) σ as fs)]
    [s s]))

;; sequence-steps : Any σ (Listof (Any σ -> Result)) -> Result
(define (sequence-steps v σ steps)
  (for/fold ([r (inj-result v σ)])
            ([s (in-list steps)])
    (result-bind r s)))

;; inj-result : A σ -> (Continue A)
(define (inj-result v σ)
  (continue v σ (list) (list)))

;; for-steps : A σ (Sequenceof B) (A σ B -> (Result A)) -> (Result A)
(define (for-steps v σ seq f)
  (for/fold ([r (inj-result v σ)])
            ([x seq])
    (result-bind r f x)))

;; boot-facet : facet Γ σ -> (Values σ (Listof Action) FacetTree)
(define (boot-facet f Γ σ)
  (define initial-sto (initial-store f Γ σ))
  (match-define (continue _ (store-concat parent-sto facet-sto) as fs)
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
         (match-define (continue v _ _ _) (eval-exp exp Γ σ))
         (cons (cons id v) locations)]
        [_ locations])))
  (apply make-store locations))

;; eval-start-actions : facet Γ σ -> (Continue #f)
(define (eval-start-actions f Γ σ)
  (match-define `(react ,O ...) f)
  (for-steps #f σ (in-list O)
    (lambda (_ σ o)
      (match o
        [`(on-start ,exp ...)
         (eval-exp* exp Γ σ)]
        [_
         (inj-result #f σ)]))))

;; boot-actor : actor Γ -> Action
(define (boot-actor a Γ)
  (match a
    [`(actor ,facet)
     (define-values (_ as ft) (boot-facet facet Γ mt-σ))
     (define assertions (ft-assertions ft mt-σ))
     (spawn-upside-down
      (actor actor-behavior
             (actor-state trie-empty ft)
             (cons (scn assertions) as)))]
    [`(dataspace ,as ...)
     (define boot-actions (for/list ([a (in-list as)])
                            (boot-actor a Γ)))
     (spawn-dataspace boot-actions)]))

;; eval-exp : exp Γ σ -> (Continue val)
(define (eval-exp e Γ σ)
  (match e
    [`(react ,O ...)
     (define-values (new-sto as ft) (boot-facet e Γ σ))
     (continue (void) new-sto as (list ft))]
    [`(actor ,_)
     ;; don't pass in parent store
     (define spawn-action (boot-actor e Γ))
     (continue (void) σ (list spawn-action) (list))]
    [`(dataspace ,actors ...)
     (continue (void) σ (list (boot-actor e Γ)) (list))]
    [`(outbound ,exp)
     (match-define (continue v new-sto as facets) (eval-exp exp Γ σ))
     (continue (outbound v) σ as facets)]
    [`(inbound ,exp)
     (match-define (continue v new-sto as facets) (eval-exp exp Γ σ))
     (continue (inbound v) σ as facets)]
    [(? symbol? id)
     (let ([v (env-lookup Γ id)])
       (continue v σ (list) (list)))]
    [`(lambda (,vars ...) ,exp)
     (define c (closure Γ e))
     (continue c σ (list) (list))]
    [`(begin ,es ...)
     (for-steps (void) σ (in-list es)
       (lambda (v σ e) (eval-exp e Γ σ)))]
    [`(list ,es ...)
     (define res (for-steps (list) σ (in-list es)
                   (lambda (rev-vs σ e)
                     (result-map (lambda (v) (cons v rev-vs))
                                 (eval-exp e Γ σ)))))
     (result-map (lambda (rev-vs) (cons 'list (reverse rev-vs)))
                 res)]
    [`(let (,x ,exp) ,body-exp)
     (result-bind (eval-exp exp Γ σ)
                  (lambda (v new-sto)
                    (define new-Γ (extend-env Γ x v))
                    (eval-exp body-exp new-Γ new-sto)))]
    [`(if ,pred-exp ,then-exp ,else-exp)
     (result-bind (eval-exp pred-exp Γ σ)
                  (lambda (v new-sto)
                    (if v
                        (eval-exp then-exp Γ new-sto)
                        (eval-exp else-exp Γ new-sto))))]
    [`(send! ,exp)
     (match-define (continue v new-sto as facets) (eval-exp exp Γ σ))
     (continue (void) new-sto (append as (list (message v))) facets)]
    [`(set! ,id ,exp)
     (match-define (continue v new-sto as facets) (eval-exp exp Γ σ))
     (define result-sto (update-sto new-sto id v))
     (continue (void) result-sto as facets)]
    [`(read ,id)
     (define v (sto-fetch σ id))
     (continue v σ (list) (list))]
    [`(,primop ,exp ..1)
     #:when (primop? primop)
     (result-bind (eval-exp* exp Γ σ)
                  (lambda (arg-vs new-sto)
                    (inj-result (apply-primop primop arg-vs) σ)))]
    [`(,f-exp ,exps ...)
     (result-bind (eval-exp f-exp Γ σ)
                  (lambda (f-v new-sto)
                    (unless (closure? f-v) (error 'eval-exp "tried to apply non-function ~v" f-v))
                    (result-bind (eval-exp* exps Γ σ)
                                 (lambda (arg-vs final-sto)
                                   (match-define (closure clo-env `(lambda (,vars ...) ,body-exp)) f-v)
                                   (unless (= (length arg-vs) (length vars))
                                     (error 'eval-exp "wrong number of arguments; expected ~v, got ~v" (length vars) (length arg-vs)))
                                   (define new-env (append (map binding vars arg-vs) clo-env))
                                   (eval-exp body-exp new-env final-sto)))))]
    [x (continue x σ (list) (list))]))

(module+ test
  ;; sequencing result
  (match-let ([(continue v s as f) (eval-exp `(begin 1 2 3) mt-Γ mt-σ)])
    (check-equal? v 3))
  ;; variable lookup
  (match-let ([(continue v s as f) (eval-exp 'x (list (binding 'x "hello")
                                                      (binding 'y "bye")
                                                      (binding 'x "world"))
                                             mt-σ)])
    (check-equal? v "hello"))
  ;; variable binding
  (match-let ([(continue v s as f) (eval-exp '(let (y 12) "cake") mt-Γ mt-σ)])
    (check-equal? v "cake"))
  (match-let ([(continue v s as f) (eval-exp '(let (y 12) y) mt-Γ mt-σ)])
    (check-equal? v 12))
  ;; if
  (match-let ([(continue v s as f) (eval-exp '(if #f 5 6) mt-Γ mt-σ)])
    (check-equal? v 6))
  (match-let ([(continue v s as f) (eval-exp '(if #t 5 6) mt-Γ mt-σ)])
    (check-equal? v 5))
  ;; send!
  (match-let ([(continue v s as f) (eval-exp '(send! 5) mt-Γ mt-σ)])
    (check-equal? as (list (message 5)))
    (check-true (void? v)))
  ;; set!
  (match-let ([(continue v s as f) (eval-exp '(set! x 12) mt-Γ (make-store '(x . "hello")))])
    (check-true (void? v))
    (check-equal? (hash-ref s 'x) 12))
  (match-let ([(continue v s as f) (eval-exp '(begin (set! x (+ 1 (read x)))
                                                     (set! x (+ 1 (read x)))
                                                     (set! x (+ 1 (read x)))
                                                     (set! x (+ 1 (read x)))
                                                     (read x))
                                             mt-Γ (make-store '(x . 0)))])
    (check-equal? v 4))
  ;; store read
  (match-let ([(continue v s as f) (eval-exp '(read x) mt-Γ (make-store '(y . 5) '(x . "hello")))])
    (check-equal? v "hello"))
  (match-let ([(continue v s as f) (eval-exp '(+ (- 5 1) (/ 4 (if (not #t) 1 2))) mt-Γ mt-σ)])
    (check-equal? v 6))
  ;; lambda
  (match-let ([(continue v s as f) (eval-exp '(let (f (lambda (x) (+ x 1))) (f 3)) mt-Γ mt-σ)])
    (check-equal? v 4))
  (match-let ([(continue v s as f) (eval-exp '(let (z 12)
                                                (let (f (lambda (x) (+ x z)))
                                                  (let (z 4)
                                                    (f 3)))) mt-Γ mt-σ)])
    (check-equal? v 15))
  (match-let ([(continue v s as f) (eval-exp '(let (f (lambda () (actor (react (assert 5)))))
                                                (f)) mt-Γ mt-σ)])
    (check-false (empty? as))))

;; dollar-id? : any -> bool
;; test if the input is a symbol whose first character is $
(define (dollar-id? s)
  (and (symbol? s)
       (char=? (string-ref (symbol->string s) 0) #\$)))

;; undollar s : dollar-id? -> var
(define (undollar s)
  (string->symbol (substring (symbol->string s) 1)))

;; eval-pat : pat Γ σ -> meta-pattern
;; technically this results in a Projection because it includes captures
;; if you put effects in your pattern then you deserve bad things
(define (eval-pat pat Γ σ)
  (match pat
    [`(list ,pats ...)
     (cons 'list
           (for/list ([p (in-list pats)])
             (eval-pat p Γ σ)))]
    [`(observe ,pat)
     (observe (eval-pat pat Γ σ))]
    [`(inbound ,pat)
     (inbound (eval-pat pat Γ σ))]
    [`(outbound ,pat)
     (outbound (eval-pat pat Γ σ))]
    [(? dollar-id? s)
     (?!)]
    ['_ ?]
    [exp
     (match-define (continue v _ _ _) (eval-exp exp Γ σ))
     v]))

;; pat-bindings : pat -> (Listof var)
(define (pat-bindings pat)
  (match pat
    [`(list ,pats ...)
     (flatten (for/list ([p (in-list pats)])
                (pat-bindings p)))]
    [`(observe ,pat)
     (pat-bindings pat)]
    [`(inbound ,pat)
     (pat-bindings pat)]
    [`(outbound ,pat)
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
  (check-equal? (occurrences `(asserted (list "price" $x))
                             (scn (assertion '(list "price" 12)))
                             (assertion '(list "price" 5))
                             mt-Γ mt-σ)
                (list (list (binding 'x 12))))
  (check-equal? (list->set
                 (occurrences `(asserted (list "price" $x))
                              (scn (π-union (assertion '(list "price" 12)) (assertion '(list "price" 16))))
                              (assertion '(list "price" 5))
                              mt-Γ mt-σ))
                (set (list (binding 'x 12)) (list (binding 'x 16)))))

;; subscription : E Γ σ -> π
(define (subscription E Γ σ)
  ;; projection->pattern to convert captures to wildcards
  (assertion (projection->pattern (observe (eval-pat (E-pat E) Γ σ)))))

;; eval-exp* : (Listof exp) Γ σ -> (Result (Listof Values))
;; evaluate a sequence of expressions
(define (eval-exp* exps Γ σ)
  (for-steps (list) σ (in-list exps)
    (lambda (vs σ e)
      (result-map (lambda (v) (append vs (list v)))
                  (eval-exp e Γ σ)))))

;; run-facet : facet π σ Γ Event -> Result
(define (run-facet f π-old σ Γ e)
  (match-define `(react ,O ...) f)
  (for-steps #f σ (in-list O)
    (lambda (_ σ o)
      (run-endpoint o π-old σ Γ e))))      

;; run-endpoint : O π σ Γ Event -> Result
;; determine the effects of an endpoint in response to an event
(define (run-endpoint O π-old σ Γ e)
  (match O
    ;; event-insensitive endpoints
    [`(field ,_ ,_)
     (inj-result #f σ)]
    [`(on-start ,exp ...)
     (inj-result #f σ)]
    [`(on-stop ,exp ...)
     (inj-result #f σ)]
    [`(assert ,exp)
     (inj-result #f σ)]
    ;; event sensitive
    [`(stop-when ,E ,exps ...)
     (define bindings (occurrences E e π-old Γ σ))
     (cond
       [(empty? bindings)
        (inj-result #f σ)]
       [else
        (match-define (continue _ sto as _)
          (for-steps #f σ (in-list bindings)
            (lambda (_ σ captures)
              (define extended-env (append captures Γ))
              (eval-exp* exps extended-env σ))))
        (stop sto as)])]
    [`(on ,E ,exps ...)
     (define bindings (occurrences E e π-old Γ σ))
     (cond
       [(empty? bindings)
        (inj-result #f σ)]
       [else
        (for-steps #f σ (in-list bindings)
          (lambda (_ sto captures)
            (define extended-env (append captures Γ))
            (eval-exp* exps extended-env sto)))])]))

;; endpoint-assertions : O Γ σ -> π
;; IGNORE effects from such expressions (yadda yadda evil yadda yadda)
(define (endpoint-assertions O Γ σ)
  (match O
    [`(field ,_ ,_)
     trie-empty]
    [`(on-start ,exp ...)
     trie-empty]
    [`(on-stop ,exp ...)
     trie-empty]
    [`(assert ,exp)
     (match-define (continue v _ _ _) (eval-exp exp Γ σ))
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

;; shutdown-facet : facet σ -> Stop
;; run each on-stop endpoint of a facet
(define (shutdown-facet f Γ σ)
  (match-define `(react ,O ...) f)
  (for/fold ([s (stop σ (list))])
            ([o (in-list O)])
    (match-define (stop σ as) s)
    (match o
      [`(on-stop ,exps ...)
       (match-define (continue _ next-sto more-as _) (eval-exp* exps Γ σ))
       (stop next-sto (append as more-as))]
      [_ s])))
             
;; shutdown-facet-tree : FacetTree σ -> Stop
(define (shutdown-facet-tree ft parent-sto)
  (match-define (facet-tree stx Γ sto children) ft)
  (define facet-sto (store-concat parent-sto sto))
  (match-define (stop (store-concat new-parent-sto _) as)
    (for/fold ([s (shutdown-facet stx Γ facet-sto)])
              ([f (in-list children)])
      (match-define (stop σ as) s)
      (match-define (stop next-sto more-as) (shutdown-facet-tree f σ))
      (stop next-sto (append as more-as))))
  (stop new-parent-sto as))


;; an OK is (ok σ FacetTree (ListofAction))
(struct ok (sto ft as) #:transparent)
;; run-facets : FacetTree π σ Event -> (U OK Stop)
(define (run-facets ft π parent-sto e)
  (match-define (facet-tree stx env sto children) ft)
  (define facet-sto (store-concat parent-sto sto))
  ;; I'm really not confident about the way the stores are being handled here
  (match (run-facet stx π facet-sto env e)
    [(continue _ new-sto as new-facets)
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
           [(stop new-sto more-as)
            (values new-sto
                    (append as more-as)
                    new-children)])))
     (match-define (store-concat new-parent-sto new-facet-sto) final-sto)
     (ok new-parent-sto (facet-tree stx env new-facet-sto new-children) final-as)]
    [(stop (store-concat new-parent-sto new-facet-sto) as)
     (match-define (stop final-parent-sto more-as)
       (shutdown-facet-tree (facet-tree stx env new-facet-sto children)
                            new-parent-sto))
     (stop final-parent-sto (append as more-as))]))

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
     (with-handlers ([exn:fail? (lambda (e) (eprintf "exception: ~v\n" e) (quit #:exception e (list)))])
       (match-define (actor-state π-old ft) s)
       (match (run-facets ft π-old mt-σ e)
         [(ok _ ft as)
          (define assertions (ft-assertions ft mt-σ))
          (define next-π (if (scn? e) (scn-trie e) π-old))
          (transition (actor-state next-π ft)
                      (cons (scn assertions) as))]
         [(stop _ as)
          (quit as)]))]
    [else #f]))

;; run : Program -> Syndicate
(define (run p)
  (define boot-actions
    (for/list ([boot (in-list p)])
      (boot-actor boot mt-Γ)))
  (run-ground boot-actions))

;; Actor AsyncChannel Program -> Boolean
;; trace-actor is the first actor spawned inside the program's ground dataspace
;; chan is a channel used by the trace-actor to signal a completed trace, by
;; sending a non-falsey value
(define (run-with-tracing trace-actor chan p #:timeout [timeout never-evt])
  (define boot-actions
    (for/list ([boot (in-list p)])
      (boot-actor boot mt-Γ)))
  (define cust (make-custodian))
  (define syndicate-thread
    (thread (lambda ()
              (engine-run timeout
                          (engine (lambda (x) (run-ground (cons trace-actor boot-actions))))))))
  (define result
    (sync (handle-evt chan
                      (lambda (val) #t))
          (handle-evt syndicate-thread
                      (lambda (val)
                        ;; it's possible one of the final events in the
                        ;; dataspace resulted in an accepting trace and the
                        ;; thread ended at the same time, so the scheduler
                        ;; picked this event. Double check the channel for this
                        ;; case.
                        (async-channel-try-get chan)))))
  (kill-thread syndicate-thread)
  result)

(define-syntax (run-with-trace stx)
  (define-splicing-syntax-class opt-timeout
    #:attributes (timeout)
    (pattern (~seq #:timeout to:expr)
             #:attr timeout #'to)
    (pattern (~seq)
             #:attr timeout #'never-evt))
  (syntax-parse stx
    #:datum-literals (trace)
    [(_ (trace item:expr ...)
        program:expr
        ot:opt-timeout)
     #'(let ([chan (make-async-channel)])
         (run-with-tracing (trace-actor (trace item ...)
                                     (lambda () (async-channel-put chan #t)))
                        chan
                        program
                        #:timeout ot.timeout))]))

(define-syntax (test-trace stx)
  (syntax-parse stx
    [(_ any ...)
     (syntax/loc stx
       (check-true (run-with-trace any ...)))]))

(define test-program
  `((actor (react (on-start (printf "hello,world\n"))))))

(define test-program2
  `(
    (actor (react (on (asserted 5)
                      (printf "wat\n"))))
    (actor (react (assert 5)))))

(test-trace (trace (assertion-added (observe 5))
                   (assertion-added 5))
            test-program2)


(define ping-pong
  `(
    (actor (react (on (message "ping")
                      (printf "ping\n")
                      (send! "pong"))))
    (actor (react (on (message "pong")
                      (printf "pong\n")
                      (send! "ping"))
                  (on-start (send! "ping"))))))

(test-trace (trace (message "ping")
                   (message "pong")
                   (message "ping")
                   (message "pong")
                   (message "ping")
                   (message "pong")
                   (message "ping")
                   (message "pong"))
            ping-pong)

(define bank-account
  `(
    (actor (react (field balance 0)
                  (assert (list "account" (read balance)))
                  (on (message (list "deposit" $amount))
                      (set! balance (+ (read balance) amount)))))

    (actor (react (on (asserted (list "account" $balance))
                      (printf "Balance changed to ~a\n" balance))
                  (stop-when (asserted (list "account" 70))
                             (printf "bye\n"))
                  (on-stop (printf "good.\n"))))

    (actor (react (stop-when (asserted (observe (list "deposit" _)))
                              (send! (list "deposit" +100))
                              (send! (list "deposit" -30)))))))

(test-trace (trace (assertion-added '(list "account" 0))
                   (and (assertion-added '(list "account" 100)) (assertion-removed '(list "account" 0)))
                   (and (assertion-added '(list "account" 70)) (assertion-removed '(list "account" 100))))
            bank-account
            #:timeout 5000)

(define multi-level-ex
  '(
    (actor (react (on (asserted "hello")
                      (printf "goodbye"))))
    (dataspace (actor (react (assert (outbound "hello")))))))

;; this fails because upside-down doesn't handle relaying right
#;(test-trace (trace (assertion-added "hello"))
            multi-level-ex)

(define ff
  '(
    (actor (react (on (message 5)
                      (printf "5\n"))
                  (on (asserted (observe 12))
                      (printf "12\n"))
                  (on (asserted (observe 16))
                      (printf "16\n"))))
    (actor (react (on (asserted 12))
                  (on-start (send! 5))
                  (on (asserted 16))
                  (on-start (send! 5))))))

(define stop-when-priority
  '(
    (actor (react (on (message "hello")
                      (send! "hey")
                      (printf "MHM.\n"))
           (stop-when (message "hello")
                      (printf "NO.\n"))))

    (actor (react (on-start (send! "hello"))
                  (on (message "hey")
                      (printf "oh.\n"))))))

(define competing-stop-whens
  '(
    (actor (react (stop-when (asserted "hello")
                             (printf "hello\n"))
                  (on (asserted "howdy")
                      (printf "howdy-do\n"))
                  (stop-when (asserted "howdy")
                             (printf "howdy\n"))))

    (actor (react (assert "hello")
                  (assert "howdy")))
    ))

#;(run competing-stop-whens)
