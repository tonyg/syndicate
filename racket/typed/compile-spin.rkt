#lang racket

(require "proto.rkt")

(module+ test
  (require rackunit)
  (require "test-utils.rkt"))

;; a SpinProgram is a
;;   (sprog [Assignment [Listof SpinProcess]])
(struct sprog [assignment procs] #:transparent)


;; a SpinProcess is a
;;   (sproc SName [Setof SName] Assignment [Setof SpinState])
(struct sproc [name state-names init states] #:transparent)

;; an Assignment is a [Hashof SVar SValue]

;; a SName is a Symbol that is a legal variable name in Spin

;; a SVar is a
;;   (svar SName SType)
(struct svar [name ty] #:transparent)

;; a SValue is one of
;;   - Int
;;   - Bool
;;   - SName
;; and must be a valid Spin literal

;; a SType is one of
;;  - 'SInt
;;  - 'SBool
;;  - 'mtype
(define SInt 'SInt)
(define SBool 'SBool)
(define mtype 'mtype)

;; a SpinState is a
;;   (sstate SName [Sequenceof SBranch])
(struct sstate [name branches] #:transparent)

;; a SBranch is a
;;   (sbranch D+ SName [Listof SAction])
(struct sbranch [event dest actions] #:transparent)

;; a SAction is one of
;;   - (assert ?)
;;   - (retract ?)
;;   - (send ?)
;;   - (incorporate D+)
;;   - (transition-to SName)
(struct assert [ty] #:transparent)
(struct retract [ty] #:transparent)
;; send defined in proto.rkt
(struct incorporate [evt] #:transparent)
(struct transition-to [dest] #:transparent)

;; each process has a local variable that determines its current state
(define CURRENT-STATE (svar 'current mtype))

;; TODO - think about how to handle subtype relationship between assertions

;; a NameEnvironment is a [Hashof τ SName]

;; [Sequenceof RoleGraph] -> SpinProgram
(define (program->spin rgs)
  (define assertion-tys (all-assertions rgs))
  (define event-tys (all-events rgs))
  (define name-env (make-name-env (set-union assertion-tys event-tys)))
  (define globals (initial-assertion-vars-for assertion-tys name-env))
  (define procs (for/list ([rg rgs]) (rg->spin rg name-env)))
  (sprog globals procs))

;; RoleGraph -> SpinProcess
(define (rg->spin rg name-env #:name [name (gensym 'proc)])
  (match-define (role-graph st0 states) rg)
  (define all-events (all-event-types (in-hash-values states)))
  (define state-renaming (make-state-rename (hash-keys states)))
  (define states- (for/list ([st (in-hash-values states)])
                    (state->spin st states name-env state-renaming)))
  (define st0- (hash-ref state-renaming st0))
  (define assignment (local-variables-for st0- all-events name-env))
  ;; TODO - states for mtype decl
  (sproc name (hash-values-set state-renaming) assignment (list->set states-)))

;; State [Sequenceof State] NameEnvironment [Hashof StateName SName] -> SpinState
(define (state->spin st states name-env state-env)
  (match-define (state name transitions assertions) st)
  (define name- (hash-ref state-env name))
  (define branches (for*/list ([(D+ txns) (in-hash transitions)]
                               [txn (in-set txns)])
                     (match-define (transition effs dest) txn)
                     (match-define (state _ _ dest-assertions) (hash-ref states dest))
                     (define dest- (hash-ref state-env dest))
                     (branch-on D+ assertions dest- dest-assertions effs name-env)))
  (sstate name- branches))

;; [Setof τ] -> NameEnvironment
(define (make-name-env tys)
  (let loop ([name-depth 3])
    (when (> name-depth 10)
      (raise-argument-error 'make-name-env "types able to be named" tys))
    (define renaming
      (for/hash ([ty (in-set tys)])
        (values ty
                (type->id ty #:depth name-depth))))
    (define names (hash-values-set renaming))
    (cond
      [(equal? (set-count names) (set-count tys))
       renaming]
      [else
       (loop (add1 name-depth))])))

;; SName -> SName
(define (assertions-var-name s)
  (string->symbol (format "~a_assertions" s)))

;; SName -> SName
(define (active-var-name s)
  (string->symbol (format "know_~a" s)))

;; [Setof τ] NameEnvironment -> Assignment
(define (initial-assertion-vars-for assertion-tys name-env)
  (for/hash ([τ (in-set assertion-tys)])
    (values (svar (hash-ref name-env τ) SInt)
            0)))

;; [Sequenceof RoleGraph] -> [Setof τ]
(define (all-assertions rgs)
  ;; RoleGraph -> (Setof τ)
  (define (all-assertions-of rg)
    (for*/set ([st (in-hash-values (role-graph-states rg))]
               [τ (in-set (state-assertions st))])
      τ))
  (for/fold ([as (set)])
            ([rg rgs])
    (set-union as (all-assertions-of rg))))

;; [Sequenceof RoleGraph] -> [Setof τ]
(define (all-events rgs)
  ;; RoleGraph -> (Setof τ)
  (define (all-events-of rg)
    (all-event-types (hash-values (role-graph-states rg))))
  (for/fold ([as (set)])
            ([rg rgs])
    (set-union as (all-events-of rg)))
  )

;; [Sequenceof State] -> ?
(define (all-event-types states)
  (for*/set ([st states]
             [D+ (in-hash-keys (state-transitions st))])
    (match D+
      [(or (Asserted τ) (Retracted τ))
       τ]
      [(Message τ)
       (raise-argument-error 'all-event-types "messages not supported yet" D+)]
      [_
       (raise-argument-error 'all-event-types "internal events not allowed" D+)])))

;; StateName [Setof τ] NameEnvironment -> Assignment
(define (local-variables-for st0 all-events name-env)
  (define assign
    (for/hash ([evt (in-set all-events)])
      (values (svar (active-var-name (hash-ref name-env evt))
                    SBool)
              #f)))
  (hash-set assign CURRENT-STATE st0))

;; D+ [Setof τ] SName [Setof τ] [Listof TransitionEffect] NameEnvironment -> SBranch
(define (branch-on D+ curr-assertions dest dest-assertions effs name-env)
  (define new-assertions (set-subtract dest-assertions curr-assertions))
  (define retractions (set-subtract curr-assertions dest-assertions))
  (define (lookup ty) (hash-ref name-env ty))
  (define asserts (set-map new-assertions (compose assert lookup)))
  (define retracts (set-map retractions (compose retract lookup)))
  (unless (empty? effs)
    (raise-argument-error 'branch-on "messages not supported" effs))
  (define renamed-evt (rename-event D+ name-env))
  (sbranch renamed-evt dest (list* (transition-to dest)
                                   (incorporate renamed-evt)
                                   (append asserts retracts effs))))

;; D+ NameEnvironment -> D+
(define (rename-event D+ name-env)
  (match D+
    [(Asserted τ)
     (Asserted (hash-ref name-env τ))]
    [(Retracted τ)
     (Retracted (hash-ref name-env τ))]
    [(Message τ)
     (raise-argument-error 'rename-event "messages not implemented yet" D+)]))

(module+ test
  (test-case
      "sanity: compile book seller type"
    (define/timeout seller-rg (compile seller-actual))
    (define name-env (hash
                      (Observe (Observe (Struct 'BookQuoteT (list (Base 'String) (Mk⋆)))))
                      'Obs_Obs_BookQuoteT
                      (Observe (Struct 'BookQuoteT (list (Base 'String) (Mk⋆))))
                      'Obs_BookQuoteT_String_star
                      (Struct 'BookQuoteT (list (Base 'String) (U (list (Base 'Int) (Base 'Int)))))
                      'BookQuoteT_String_U_Int_Int))
    (define/timeout seller-spin (rg->spin seller-rg name-env))
    (check-true (sproc? seller-spin))))

(define tab-level (make-parameter 0))

(define TAB-WIDTH 2)

(define (indent)
  (display (make-string (* TAB-WIDTH (tab-level)) #\space)))

(define-syntax-rule (with-indent bdy ...)
  (parameterize ([tab-level (add1 (tab-level))])
    bdy ...))

(define SPIN_ID_RX #rx"[a-zA-Z][a-zA-Z0-9_]*")
(define SPIN_ID_TRAILING_CHAR #rx"[a-zA-Z0-9_]+")

;; (U Symbol String) -> Bool
(define (spin-id? s)
  (when (symbol? s)
    (set! s (symbol->string s)))
  (regexp-match? SPIN_ID_RX s))

;; (U Symbol String) -> SName
(define (make-spin-id s)
  (when (symbol? s)
    (set! s (symbol->string s)))
  (define with_legal_prefix (string-append "ty_" s))
  (match (regexp-match* SPIN_ID_TRAILING_CHAR with_legal_prefix)
    ['("ty_")
     (raise-argument-error 'make-spin-id "unable to make spin id" s)]
    [(cons fst rst)
     (define match-str (apply string-append fst rst))
     (define without-added-prefix (substring match-str 3))
     (if (spin-id? without-added-prefix)
         (string->symbol without-added-prefix)
         (string->symbol match-str))]))

;; τ -> SName
(define (type->id ty #:depth [depth 3])
  (define ctors (type-constructors ty depth))
  (define rough-name (string-join (map symbol->string ctors) "_"))
  (make-spin-id rough-name))

;; [Listof StateName] -> [Hashof StateName SName]
(define (make-state-rename state-names)
  (let loop ([prefix 3])
    (define renaming (for/hash ([nm (in-list state-names)])
                       (values nm
                               (state-name->spin-id nm #:prefix prefix))))
    (define distinct-names (hash-values-set renaming))
    (cond
      [(equal? (set-count distinct-names) (set-count state-names))
       renaming]
      [(> prefix 20)
       (raise-argument-error 'make-state-rename "able to make renaming" state-names)]
      [else
       (loop (add1 prefix))])))

;; StateName -> SName
(define (state-name->spin-id nm #:prefix [prefix 3])
  (define (take-prefix s) (substring s 0 prefix))
  (define rough-name (string-join (set-map nm (compose take-prefix symbol->string)) "_"))
  (make-spin-id rough-name))

;; τ -> [Listof Symbol]
(define (type-constructors ty depth)
  (cond
    [(zero? depth) '()]
    [else
     (match ty
       [(Struct name tys)
        ;; TODO - consider camel-casing struct name
        (cons name (append-map (λ (ty) (type-constructors ty (sub1 depth))) tys))]
       [(Observe ty)
        (cons 'Obs (type-constructors ty (sub1 depth)))]
       [(U tys)
        (cons 'U (append-map (λ (ty) (type-constructors ty (sub1 depth))) tys))]
       [(Base name)
        (list name)]
       [(List _)
        (list 'List)]
       [(Set _)
        (list 'Set)]
       [(Hash _ _)
        (list 'Hash)]
       [(== ⋆)
        (list 'star)]
       [(internal-label _ _)
        (raise-argument-error 'type-constructors "internal events not supported" ty)])]))

(module+ test
  (test-case
      "type-constructors basics"
    (define bi (Struct 'BookInterestT (list (Base 'String) (Base 'String) (Base 'Bool))))
    (check-equal? (type-constructors bi 1)
                  '(BookInterestT))
    (check-equal? (type-constructors bi 2)
                  '(BookInterestT String String Bool))
    (check-equal? (type-constructors bi 3)
                  '(BookInterestT String String Bool))
    (check-equal? (type-constructors (Observe bi) 3)
                  '(Obs BookInterestT String String Bool)))
  (test-case
      "type->id basics"
    (define bi (Struct 'BookInterestT (list (Base 'String) (Base 'String) (Base 'Bool))))
    (check-equal? (type->id bi)
                  'BookInterestT_String_String_Bool)
    (check-equal? (type->id (Observe bi))
                  'Obs_BookInterestT_String_String_Bool)
    (check-equal? (type->id (Struct 'hi-mom '()))
                  'himom)
    (check-equal? (type->id bi #:depth 1)
                  'BookInterestT)
    (check-exn exn:fail?
               (lambda () (type->id (Struct '--- '())))
               "unable to make spin id")))

;; SpinThang -> Void
(define (gen-spin spin)
  (match spin
    [(sprog assignment procs)
     #f]
    [(sproc name state_names init states)
     ;; TODO - need to make sure name is a spin id
     (indent) (printf "active proctype ~a() {\n" name)
     (with-indent
       (gen-assignment init)
       (indent) (displayln "do")
       (with-indent
         (for ([st states])
           (gen-spin st)))
       (indent) (displayln "od;")
       )
     (indent) (displayln "}")]
    [(sstate name branches)
     (indent) (printf ":: ~a == ~a ->\n" (svar-name CURRENT-STATE) name)
     (with-indent
       (indent) (displayln "if")
       (with-indent
         (for ([branch branches])
           (gen-spin branch)))
       (indent) (displayln "fi;"))]
    [(sbranch event dest actions)
     (indent) (printf ":: ~a ->\n" (predicate-for event))
     ;; TODO - make the body atomic
     (with-indent
       (for ([act actions])
         (gen-spin act)))]
    [(svar name ty)
     ;; TODO - not sure if needed - handled by `declare-var` below
     #f]
    [(assert x)
     (indent) (printf "ASSERT(~a);\n" x)]
    [(retract x)
     (indent) (printf "RETRACT(~a);\n" x)]
    [(send x)
     (raise-argument-error 'gen-spin "message sending not supported yet" spin)]
    [(incorporate evt)
     (indent) (update-for evt)]
    [(transition-to dest)
     (indent) (printf "~a = ~a;\n" (svar-name CURRENT-STATE) dest)]))

;; Assignment -> Void
(define (gen-assignment assign)
  (for ([(var val) (in-hash assign)])
    (indent) (printf "~a = ~a;\n"
                     (declare-var var)
                     (spin-val->string val))))

;; SVar -> Void
(define (declare-var var)
  (match-define (svar name ty) var)
  (format "~a ~a" (spin-type->string ty) name))

;; SValue -> String
(define (spin-val->string v)
  (cond
    [(boolean? v)
     (if v "true" "false")]
    [(exact-integer? v)
     (~a v)]
    [(symbol? v)
     (~a v)]
    ;; TODO - intermediate state has sets
    [(set? v)
     (~a v)]))

;; SType -> String
(define (spin-type->string ty)
  (match ty
    [(== SInt) "int"]
    [(== SBool) "bool"]
    [(== mtype) "mtype"]))

;; D+ -> String
(define (predicate-for event)
  (match event
    [(Asserted nm)
     (define assertion-var (assertions-var-name nm))
     (define active-var (active-var-name nm))
     (format "ASSERTED(~a) && !~a" assertion-var active-var)]
    [(Retracted nm)
     (define assertion-var (assertions-var-name nm))
     (define active-var (active-var-name nm))
     (format "RETRACTED(~a) && ~a" assertion-var active-var)]
    [(Message nm)
     (raise-argument-error 'predicate-for "message sending not supported yet" event)]))

;; D+ -> Void
(define (update-for event)
  (match event
    [(Asserted nm)
     (define active-var (active-var-name nm))
     (printf "~a = ~a;\n" active-var (spin-val->string #t))]
    [(Retracted nm)
     (define active-var (active-var-name nm))
     (printf "~a = ~a;\n" active-var (spin-val->string #f))]
    [(Message nm)
     (raise-argument-error 'predicate-for "message sending not supported yet" event)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Utils

;; [Hashof K V] -> [Setof V]
(define (hash-values-set h)
  (for/set ([x (in-hash-values h)])
    x))
