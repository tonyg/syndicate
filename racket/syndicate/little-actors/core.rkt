#lang racket

(provide run
         run-with
         run-with-trace)

(require (except-in syndicate/monolithic subscription))
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
;; var or
;; primop or
;; ('begin exp ...) or
;; ('let (var exp) exp) or
;; ('if exp exp exp) or
;; ('send! exp) or
;; ('react O ...) or
;; ('spawn O ...) or
;; ('dataspace actor ...) or
;; ('observe exp) or
;; ('outbound exp) or
;; ('inbound exp) or
;; ('list exp ...) or
;; atom

;; a `val` is either
;; atom or
;; ('list val ...) or
;; (outbound val) or
;; (inbound val) or
;; (observe val) or
;; σ Any ... -> Continue val

;; `primop` is one of
;; + * / - and or not equal? null? car cdr printf

;; an `O` (endpoint) is either
;; ('field [var exp] ...) or
;; ('assert exp) or
;; ('on E exp ...) or
;; ('stop-when E exp ...) or
;; ('on-start exp ...) or
;; ('on-stop exp ...)

;; a `facet` is ('react O ...)

;; an `actor` is
;; ('spawn O ...) or
;; ('dataspace actor ...)

;; an `E` is either
;; ('asserted pat) or
;; ('retracted pat) or
;; ('message pat)

;; a `pat` is either
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

;; a FacetTree is (facet-tree facet Γ σ (Listof FacetTree))
(struct facet-tree (stx env sto children) #:transparent)

;; an ActorState is (actor-state π (Listof FacetTree))
(struct actor-state (π fs) #:transparent)

;; a π is a trie
(define π-union assertion-set-union)

;; a Program is a (Listof actor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures for Accumulating Effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A (Continue A) is (continue A σ (Listof Action) (Listof FacetTree))
(struct continue (v sto as fs) #:transparent)
;; A Stop is (stop σ (Listof Action) (Listof FacetTree))
(struct stop (sto as fs) #:transparent)
;; A (Result A) is a Stop or (Continue A)

;; result-bind : Result (Any σ Any ... -> Result) Any ... -> Result
(define (result-bind r f . extra-args)
  (match r
    [(continue v σ as fs)
     (match (apply f v σ extra-args)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facets and Endpoints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run-all-facets : FacetTree π σ Event -> (Result #f)
(define (run-all-facets ft π parent-sto e)
  ;; π σ Event (Listof FacetTree) -> (Values σ (Listof Action) (Listof FacetTree))
  (define (iterate-over-children π-old σ e children)
    (for/fold ([σ σ]
               [as '()]
               [new-children '()])
              ([ft (in-list children)])
      (match (run-all-facets ft π-old σ e)
        [(continue _ new-sto new-ft more-as)
         (values new-sto
                 (append as more-as)
                 ;; n^2 but let's keep the order the same
                 (append new-children (list new-ft)))]
        [(stop new-sto more-as more-fs)
         (define facet-knowledge-scn (if (scn? e) e (scn π-old)))
         (define-values (final-sto final-as boot-children)
           (iterate-over-children trie-empty new-sto facet-knowledge-scn more-fs))
         (values final-sto
                 (append as more-as final-as)
                 (append new-children boot-children))])))
  (match-define (facet-tree stx env sto children) ft)
  (define facet-sto (store-concat parent-sto sto))
  ;; I'm really not confident about the way the stores are being handled here
  (match (run-facet stx π facet-sto env e)
    [(continue _ facet-sto2 as new-facets)
     (define-values (facet-sto3 more-as new-children)
       (iterate-over-children π facet-sto2 e children))
     (define facet-knowledge-scn (if (scn? e) e (scn π)))
     (define-values (final-sto final-as boot-children)
       (iterate-over-children trie-empty facet-sto3 facet-knowledge-scn new-facets))
     (match-define (store-concat new-parent-sto new-facet-sto) final-sto)
     (continue #f new-parent-sto (facet-tree stx env new-facet-sto (append new-children boot-children)) (append as more-as final-as))]
    [(stop (store-concat new-parent-sto new-facet-sto) as fs)
     ;; BUG lose facets created during on-stop
     (match-define (stop final-parent-sto more-as more-fs)
       (shutdown-facet-tree (facet-tree stx env new-facet-sto children)
                            new-parent-sto))
     (stop final-parent-sto (append as more-as) (append fs more-fs))]))

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
    [`(field ,_)
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
        (match-define (continue _ sto as fs)
          (for-steps #f σ (in-list bindings)
            (lambda (_ σ captures)
              (define extended-env (append captures Γ))
              (eval-exp* exps extended-env σ))))
        (stop sto as fs)])]
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
    [`(field ,_)
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
  (for/fold ([s (stop σ (list) (list))])
            ([o (in-list O)])
    (match-define (stop σ as fs) s)
    (match o
      [`(on-stop ,exps ...)
       (match-define (continue _ next-sto more-as more-fs) (eval-exp* exps Γ σ))
       (stop next-sto (append as more-as) (append fs more-fs))]
      [_ s])))
             
;; shutdown-facet-tree : FacetTree σ -> Stop
(define (shutdown-facet-tree ft parent-sto)
  (match-define (facet-tree stx Γ sto children) ft)
  (define facet-sto (store-concat parent-sto sto))
  (match-define (stop (store-concat new-parent-sto _) as fs)
    (for/fold ([s (shutdown-facet stx Γ facet-sto)])
              ([f (in-list children)])
      (match-define (stop σ as fs) s)
      ;; DECISION: bubble up new facets from nested facets
      (match-define (stop next-sto more-as more-fs) (shutdown-facet-tree f σ))
      (stop next-sto (append as more-as) (append fs more-fs))))
  (stop new-parent-sto as fs))

;; ft-assertions : FacetTree Γ σ -> π
(define (ft-assertions ft Γ σ)
  (match-define (facet-tree stx env sto children) ft)
  (define extended-sto (store-concat σ sto))
  (define extended-env (append Γ env))
  (for/fold ([π (facet-assertions stx extended-env extended-sto)])
            ([f (in-list children)])
    (π-union π (ft-assertions f extended-env extended-sto))))

;; actor-behavior : Event ActorState -> Transition
;; leaf behavior function
(define (actor-behavior e s)
  (when e
    (with-handlers ([exn:fail? (lambda (e) (eprintf "exception: ~v\n" e) (quit #:exception e (list)))])
      (match-define (actor-state π-old fts) s)
      (define-values (actions next-fts)
        (for/fold ([as '()]
                   [new-fts '()])
                  ([ft (in-list fts)])
          (match (run-all-facets ft π-old mt-σ e)
            [(continue _ _ ft more-as)
             (values (append as more-as)
                     ;; reverses the order
                     (cons ft new-fts))]
            [(stop _ more-as fs)
             (values (append as more-as)
                     (append new-fts fs))])))
      (cond
        [(empty? next-fts)
         (quit actions)]
        [else
         (define assertions
           (for/fold ([t trie-empty])
                     ([ft (in-list next-fts)])
             (trie-union t (ft-assertions ft mt-Γ mt-σ))))
         (define next-π (if (scn? e) (scn-trie e) π-old))
         (transition (actor-state next-π next-fts)
                     (cons (scn assertions) actions))]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluating Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eval-exp : exp Γ σ -> (Continue val)
(define (eval-exp e Γ σ)
  (match e
    [`(react ,O ...)
     (define-values (new-sto as ft) (boot-facet e Γ σ))
     (continue (void) new-sto as (list ft))]
    [`(spawn ,O ...)
     ;; don't pass in parent store
     (define spawn-action (boot-actor e Γ))
     (continue (void) σ (list spawn-action) (list))]
    [`(dataspace ,actors ...)
     (continue (void) σ (list (boot-actor e Γ)) (list))]
    [`(observe ,exp)
     (match-define (continue v new-sto as facets) (eval-exp exp Γ σ))
     (continue (observe v) σ as facets)]
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
     (define f
       (lambda (new-σ . actuals)
         (define extended-env (append (map binding vars actuals) Γ))
         (unless (= (length vars) (length actuals))
           (error 'eval-exp "wrong number of arguments; expected ~v, got ~v" (length vars) (length actuals)))
         (eval-exp exp extended-env new-σ)))
     (continue f σ (list) (list))]
    [`(begin ,es ...)
     (for-steps (void) σ (in-list es)
       (lambda (v σ e) (eval-exp e Γ σ)))]
    [`(list ,es ...)
     (define res (for-steps (list) σ (in-list es)
                   (lambda (rev-vs σ e)
                     (result-map (lambda (v) (cons v rev-vs))
                                 (eval-exp e Γ σ)))))
     (result-map (lambda (rev-vs) (reverse rev-vs))
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
    [`(,primop ,exp ..1)
     #:when (primop? primop)
     (result-bind (eval-exp* exp Γ σ)
                  (lambda (arg-vs new-sto)
                    (inj-result (apply-primop primop arg-vs) σ)))]
    [`(,f-exp ,exps ...)
     (result-bind (eval-exp f-exp Γ σ)
                  (lambda (f-v new-sto)
                    (unless (procedure? f-v) (error 'eval-exp "tried to apply non-function ~v" f-v))
                    (result-bind (eval-exp* exps Γ σ)
                                 (lambda (arg-vs final-sto)
                                   (apply f-v final-sto arg-vs)))))]
    ;; TODO add a predicate
    ;; atom?
    [x (continue x σ (list) (list))]))

;; eval-exp* : (Listof exp) Γ σ -> (Result (Listof Values))
;; evaluate a sequence of expressions
(define (eval-exp* exps Γ σ)
  (for-steps (list) σ (in-list exps)
    (lambda (vs σ e)
      (result-map (lambda (v) (append vs (list v)))
                  (eval-exp e Γ σ)))))

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
  ;; field set
  (match-let ([(continue v s as f) (eval-exp '(x 12)
                                             (list (binding 'x (field-function 'x)))
                                             (make-store '(x . "hello")))])
    (check-true (void? v))
    (check-equal? (hash-ref s 'x) 12))
  (match-let ([(continue v s as f) (eval-exp '(begin (x (+ 1 (x)))
                                                     (x (+ 1 (x)))
                                                     (x (+ 1 (x)))
                                                     (x (+ 1 (x)))
                                                     (x))
                                             (list (binding 'x (field-function 'x)))
                                             (make-store '(x . 0)))])
    (check-equal? v 4))
  ;; field read
  (match-let ([(continue v s as f) (eval-exp '(x)
                                             (list (binding 'x (field-function 'x)))
                                             (make-store '(y . 5) '(x . "hello")))])
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
  (match-let ([(continue v s as f) (eval-exp '(let (f (lambda () (spawn (assert 5))))
                                                (f)) mt-Γ mt-σ)])
    (check-false (empty? as))))

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

;; boot-facet : facet Γ σ -> (Values σ (Listof Action) FacetTree)
(define (boot-facet f Γ σ)
  (define-values (initial-sto field-bindings) (initial-store f Γ σ))
  (define extended-env (append field-bindings Γ))
  (match-define (continue _ (store-concat parent-sto facet-sto) as fs)
    (eval-start-actions f extended-env (store-concat σ initial-sto)))
  (values parent-sto as (facet-tree f extended-env facet-sto fs)))

;; initial-store : facet Γ σ -> (Values σ Γ)
;; returns the new store and bindings for the field ids
;; only bad people would put effects here.
(define (initial-store f Γ σ)
  (match-define `(react ,O ...) f)
  (define-values (locations bindings)
    (for/fold ([locations (list)]
               [bindings mt-Γ])
              ([o (in-list O)])
      (match o
        [`(field (,ids ,exps) ...)
         (for/fold ([locations locations]
                    [bindings bindings])
                   ([id (in-list ids)]
                    [exp (in-list exps)])
           (match-define (continue v _ _ _) (eval-exp exp Γ σ))
           (values (cons (cons id v) locations)
                   (cons (binding id (field-function id))
                         bindings)))]
        [_ (values locations bindings)])))
  (values (apply make-store locations)
          bindings))

;; (case-> [σ -> (Continue val)]
;;         [σ val -> (Continue val)]
;; This is the function field identifiers are bound to
;; read or update the store based on whether an argument (beyond the store)
(define (field-function id)
  (case-lambda [(σ) (inj-result (sto-fetch σ id) σ)]
               [(σ v) (inj-result (void) (update-sto σ id v))]))

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
  (with-handlers ([exn:fail? (lambda (e)
                               (eprintf "booting actor died with: ~v\n" e)
                               #f)])
    (match a
      [`(spawn ,O ...)
       (define facet (cons 'react O))
       (define-values (_ as ft) (boot-facet facet Γ mt-σ))
       (define assertions (ft-assertions ft mt-Γ mt-σ))
       (spawn-upside-down
        (actor actor-behavior
               (actor-state trie-empty (list ft))
               (cons (scn assertions) as)))]
      [`(dataspace ,as ...)
       (define boot-actions (for/list ([a (in-list as)]) (boot-actor a Γ)))
       ;; note the recursive upside-down wrapping of dataspaces--
       ;; the upside-down-relay is needed for things to line up properly
       (spawn-upside-down
        (dataspace-actor (cons upside-down-relay boot-actions)))])))

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
     (for/list ([p (in-list pats)])
       (eval-pat p Γ σ))]
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
                             (scn (assertion '("price" 12)))
                             (assertion '("price" 5))
                             mt-Γ mt-σ)
                (list (list (binding 'x 12))))
  (check-equal? (list->set
                 (occurrences `(asserted (list "price" $x))
                              (scn (π-union (assertion '("price" 12)) (assertion '("price" 16))))
                              (assertion '("price" 5))
                              mt-Γ mt-σ))
                (set (list (binding 'x 12)) (list (binding 'x 16)))))

;; subscription : E Γ σ -> π
(define (subscription E Γ σ)
  ;; projection->pattern to convert captures to wildcards
  (assertion (projection->pattern (observe (eval-pat (E-pat E) Γ σ)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environments and Store Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; make-store : (Listof (cons var val)) -> σ
(define (make-store . vs)
  (make-immutable-hash vs))

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

;; sto-fetch : σ var -> val
;; retrieve the value of field var.
;; if not present throw an error
(define (sto-fetch σ id)
  (let search ([σ σ]
               [k (lambda () (error 'sto-fetch "unbound field: ~v" id))])
    (match σ
      [(store-concat σ1 σ2)
       (search σ2 (lambda () (search σ1 k)))]
      [_
       (if (hash-has-key? σ id)
           (hash-ref σ id)
           (k))])))

(module+ test
  (let* ([s1 (make-store '(balance . 100))]
         [s2 (store-concat mt-σ s1)]
         [s3 (store-concat s1 mt-σ)])
    (check-equal? (update-sto s2 'balance 50)
                  (store-concat mt-σ (make-store '(balance . 50))))
    (check-equal? (update-sto s3 'balance 50)
                  (store-concat (make-store '(balance . 50)) mt-σ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whole Programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : Program -> Syndicate
(define (run p)
  (define boot-actions
    (for/list ([boot (in-list p)])
      (boot-actor boot mt-Γ)))
  (run-ground (cons upside-down-relay boot-actions)))

;; Actor Program -> Syndicate
(define (run-with regular-actor p)
  (define boot-actions
    (for/list ([boot (in-list p)])
      (boot-actor boot mt-Γ)))
  (run-ground regular-actor boot-actions))
  

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
                          (engine (lambda (x) (run-ground (cons trace-actor (cons upside-down-relay boot-actions)))))))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ff
  '(
    (spawn (on (message 5)
               (printf "5\n"))
           (on (asserted (observe 12))
               (printf "12\n"))
           (on (asserted (observe 16))
               (printf "16\n")))
    (spawn (on (asserted 12))
           (on-start (send! 5))
           (on (asserted 16))
           (on-start (send! 5)))))

(define stop-when-priority
  '(
    (spawn (on (message "hello")
               (send! "hey")
               (printf "MHM.\n"))
           (stop-when (message "hello")
                      (printf "NO.\n")))

    (spawn (on-start (send! "hello"))
           (on (message "hey")
               (printf "oh.\n")))))

(define competing-stop-whens
  '(
    (spawn (stop-when (asserted "hello")
                      (printf "hello\n"))
           (on (asserted "howdy")
               (printf "howdy-do\n"))
           (stop-when (asserted "howdy")
                      (printf "howdy\n")))

    (spawn (assert "hello")
           (assert "howdy"))
    ))

;; should this work?
(define store-passing
  '(
    (spawn (field [x 10])
           (on (message "spawn")
               (spawn (field [y (+ 1 (x))])
                      (on (message "read y")
                          (send! (list "y" (y))))))
           (on (message "read x")
               (send! (list "x" (x)))))
    (spawn (on-start (send! "spawn"))
           (on (asserted (observe "read y"))
               (send! "read y")
               (send! "read x")))
    (spawn (on (message (list "y" $y))
               (printf "y = ~v\n" y))
           (on (message (list "x" $x))
               (printf "x = ~v\n" x)))))


(module+ test
  (define do-new-facets-run-immediately
    '(
      (spawn (on (message "hello")
                 (react (on (message "hello")
                            (send! "I am here")))))
      (spawn (on-start (send! "hello")))))
  (check-false (run-with-trace (trace (message "I am here"))
                               do-new-facets-run-immediately)))

(module+ test
  ;; this should bring down the actor *but not* the entire program
  (define escaping-field
    '((spawn (field [x #f])
             (on-start (react (field [y 10])
                              (on-start (x (lambda (v) (y v)))))
                       ((x) 5)
                       (send! "success!")))))
  (check-false (run-with-trace (trace (message "success!"))
                               escaping-field))
  (check-not-exn (lambda () (run escaping-field))))
