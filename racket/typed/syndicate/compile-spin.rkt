#lang racket

(provide run-spin compile+verify)

(require "proto.rkt")
(require "ltl.rkt")
(require racket/runtime-path)
(require syndicate/trace syndicate/trace/msd)
(require (prefix-in synd: (only-in syndicate/core assert retract message))
         (prefix-in synd: (only-in syndicate/patch patch-empty patch-seq)))

(require (only-in racket/hash hash-union))

(module+ test
  (require rackunit)
  (require "test-utils.rkt"))



;; a SpinProgram is a
;;   (sprog Assignment [Listof SpinProcess] MessageTable SpinLTL NameEnvironment)
(struct sprog [assignment procs spec msg-tabe name-env] #:transparent)

;; a SpinProcess is a
;;   (sproc SName [Setof SName] Assignment [Listof SAction] [Setof SpinState])
(struct sproc [name state-names locals init-actions states] #:transparent)

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
;;   - (add-message-interest ?)
;;   - (remove-message-interest ?)
;;   - (transition-to SName)
(struct assert [ty] #:prefab)
(struct retract [ty] #:prefab)
(struct add-message-interest [ty] #:prefab)
(struct remove-message-interest [ty] #:prefab)
;; send defined in proto.rkt
(struct incorporate [evt] #:transparent)
(struct transition-to [dest] #:transparent)

;; each process has a local variable that determines its current state
(define CURRENT-STATE (svar 'current mtype))

;; a NameEnvironment is a [Hashof τ SName]

;; a MessageTable is a [Hashof SName [Setof SName]]
;; mapping each possible message that can be sent to the names of each event
;; that message can match


;; a SpinLTL is a [LTL [U τ (Message τ)]]
;; where τ represents an assertion and (Message τ) a message


;; [Sequenceof RoleGraph] SpinLTL -> SpinProgram
(define (program->spin rgs [spec #t])
  (define assertion-tys (all-assertions rgs))
  (define message-tys (all-messages rgs))
  (define event-tys (all-events rgs))
  (define-values (spec-asserts spec-msgs) (spec-types spec))
  (define-values (message-evts assertion-evts) (set-partition Message? event-tys))
  (define msg-event-tys (list->set (set-map message-evts Message-ty)))
  (define msg-bodies (set-union message-tys msg-event-tys spec-msgs))
  (define event-subty# (make-event-map assertion-tys assertion-evts))
  (define all-assertion-tys (set-union assertion-tys assertion-evts spec-asserts))
  (define all-mentioned-tys (set-union all-assertion-tys msg-bodies))
  (define name-env (make-name-env all-mentioned-tys))
  (define procs (for/list ([rg rgs]) (rg->spin rg event-subty# name-env)))
  (define assertion-vars (initial-assertion-vars-for all-assertion-tys name-env))
  (define messages-vars (initial-message-vars-for msg-bodies name-env))
  (define mailbox-vars (initial-mailbox-vars msg-bodies (map sproc-name procs) name-env))
  (define msg-table (make-message-table message-tys msg-event-tys name-env))
  (define globals (hash-union assertion-vars messages-vars mailbox-vars))
  (define spec-spin (rename-ltl spec name-env))
  (sprog globals procs spec-spin msg-table name-env))

;; [Setof τ] [Setof τ] NameEnvironment -> MessageTable
(define (make-message-table message-tys msg-event-tys name-env)
  (define msg-subty# (make-event-map message-tys msg-event-tys))
  (define (lookup nm) (hash-ref name-env nm))
  (for/hash ([m (in-set message-tys)])
    (values (lookup m)
            (rename-all name-env (set-add (hash-ref msg-subty# m) m)))))

;; [Setof X] [Pred X] -> (Values [Setof X] [Setof X])
;; like partition on lists but for sets
(define (set-partition p s)
  (for/fold ([yays (set)]
             [nays (set)])
            ([x (in-set s)])
    (if (p x)
        (values (set-add yays x) nays)
        (values yays (set-add nays x)))))

;; RoleGraph [Hashof τ [Setof τ]] NameEnvironment -> SpinProcess
(define (rg->spin rg event-subty# name-env #:name [name (gensym 'proc)])
  (match-define (role-graph st0 states) rg)
  (define all-events (all-event-types (in-hash-values states)))
  (define state-renaming (make-state-rename (hash-keys states)))
  (define states- (for/list ([st (in-hash-values states)])
                    (state->spin st states event-subty# name-env state-renaming)))
  (define st0- (hash-ref state-renaming st0))
  ;; ergh the invariant for when I tack on _assertions to a name is getting tricksy
  (define st0-asserts (state-assertions (hash-ref states st0)))
  (define st0-msg-interests (message-transitions (state-transitions (hash-ref states st0))))
  (define initial-asserts (transition-assertions (set) st0-asserts event-subty# name-env))
  (define initial-msg-interests (transition-msg-interests (set) st0-msg-interests event-subty# name-env))
  (define init-acts (append initial-asserts initial-msg-interests))
  (define assignment (local-variables-for st0- all-events name-env))
  (sproc name (hash-values-set state-renaming) assignment init-acts (list->set states-)))

;; State [Sequenceof State] [Hashof τ [Setof τ]] NameEnvironment [Hashof StateName SName] -> SpinState
(define (state->spin st states event-subty# name-env state-env)
  (match-define (state name transitions assertions) st)
  (define name- (hash-ref state-env name))
  (define msg-txns (message-transitions transitions))
  (define branches (for*/list ([(D+ txns) (in-hash transitions)]
                               [txn (in-set txns)])
                     (match-define (transition effs dest) txn)
                     (match-define (state _ dest-txns dest-assertions) (hash-ref states dest))
                     (define dest- (hash-ref state-env dest))
                     (define dest-msg-txns (message-transitions dest-txns))
                     (branch-on D+ assertions msg-txns dest- dest-assertions dest-msg-txns effs event-subty# name-env)))
  (sstate name- branches))

;; (Hashof D+ _) -> (Setof τ)
(define (message-transitions transitions)
  (for/set ([D+ (in-hash-keys transitions)]
            #:when (Message? D+))
    (Message-ty D+)))

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

;; SName -> SName
(define (messages-var-name s)
  (string->symbol (format "~a_messages" s)))

;; SName SName -> SName
(define (msg-mailbox-var-name msg-ty proc-name)
  (string->symbol (format "~a_~a_mailbox" proc-name msg-ty)))

;; SName SName -> SName
(define (msg-interest-var-name msg-ty proc-name)
  (string->symbol (format "~a_~a_interest" proc-name msg-ty)))

;; [Setof τ] [Setof τ] -> [Hashof τ [Setof τ]]
(define (make-event-map assertion-tys event-tys)
  ;; TODO - potentially use non-empty intersection
  (for/hash ([a (in-set assertion-tys)])
    (values a
            (all-supertypes-of a event-tys))))

;; τ [Setof τ] -> [Setof τ]
(define (all-supertypes-of τ tys)
  (for*/set ([ty (in-set tys)]
             #:when (<:? τ ty))
    ty))

;; [Setof τ] [Hashof τ [Setof τ]]
(define (super-type-closure asserts event-subty#)
  (for*/set ([a (in-set asserts)]
             [supers (in-value (hash-ref event-subty# a))]
             [τ (in-set (set-add supers a))])
    τ))

;; [Setof τ] NameEnvironment -> Assignment
(define (initial-assertion-vars-for assertion-tys name-env)
  (for/hash ([τ (in-set assertion-tys)])
    (values (svar (assertions-var-name (hash-ref name-env τ)) SInt)
            0)))

;; [Setof τ] NameEnvironment -> Assignment
(define (initial-message-vars-for msg-bodies name-env)
  (for/hash ([τ (in-set msg-bodies)])
    (values (svar (messages-var-name (hash-ref name-env τ)) SInt)
            0)))

;; [Setof τ] [Listof SName] NameEnvironment -> Assignment
(define (initial-mailbox-vars msg-bodies proc-names name-env)
  (for*/fold ([assign (hash)])
             ([proc-name (in-list proc-names)]
              [msg-ty (in-set msg-bodies)])
    (define ty- (hash-ref name-env msg-ty))
    (define mailbox (svar (msg-mailbox-var-name ty- proc-name) SInt))
    (define interest (svar (msg-interest-var-name ty- proc-name) SBool))
    (hash-set (hash-set assign mailbox 0)
              interest #f)))

;; NameEnvironment [Setof τ] -> [Sequenceof SName]
(define (rename-all name-env asserts)
  (for/set ([a (in-set asserts)])
    (hash-ref name-env a)))

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
(define (all-messages rgs)
  ;; RoleGraph -> (Setof τ)
  (define (all-messages-of rg)
    (for*/set ([st (in-hash-values (role-graph-states rg))]
               [txns (in-hash-values (state-transitions st))]
               [txn (in-set txns)]
               [eff (in-list (transition-effs txn))]
               #:when (send? eff))
      (send-ty eff)))
  (for/fold ([ms (set)])
            ([rg rgs])
    (set-union ms (all-messages-of rg))))

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
       D+]
      [_
       (raise-argument-error 'all-event-types "internal events not allowed" D+)])))

;; SpinLTL -> (Values (Setof τ) (Setof τ))
;; extract the types of assertions and messages mentioned in a spec
(define (spec-types ltl)
  (let loop ([ltls (list ltl)]
             [asserts (set)]
             [msgs (set)])
    (match ltls
      ['()
       (values asserts msgs)]
      [(cons ltl more-ltls)
       (match ltl
         [(or (always more-ltl)
              (eventually more-ltl)
              (ltl-not more-ltl))
          (loop (cons more-ltl more-ltls) asserts msgs)]
         [(or (weak-until ltl1 ltl2)
              (strong-until ltl1 ltl2)
              (ltl-implies ltl1 ltl2)
              (ltl-and ltl1 ltl2)
              (ltl-or ltl1 ltl2))
          (loop (list* ltl1 ltl2 more-ltls) asserts msgs)]
         [(atomic (Message τ))
          (loop more-ltls asserts (set-add msgs τ))]
         [(atomic τ)
          (loop more-ltls (set-add asserts τ) msgs)]
         [_
          (loop more-ltls asserts msgs)])])))

;; SName [Setof [U τ D+] NameEnvironment -> Assignment
(define (local-variables-for st0 all-events name-env)
  (define assign
    (for/hash ([evt (in-set all-events)]
               #:unless (Message? evt))
      (values (svar (active-var-name (hash-ref name-env evt))
                    SBool)
              #f)))
  (hash-set assign CURRENT-STATE st0))

;; D+ [Setof τ] [Setof τ] SName [Setof τ] [Setof τ] [Listof TransitionEffect] [Hashof τ [Setof τ]] NameEnvironment -> SBranch
(define (branch-on D+ curr-assertions curr-msg-txns dest dest-assertions dest-msg-txns effs event-subty# name-env)
  (define assertion-updates (transition-assertions curr-assertions dest-assertions event-subty# name-env))
  (define msg-interest-updates (transition-msg-interests curr-msg-txns dest-msg-txns event-subty# name-env))
  (define effs- (rename-effects effs name-env))
  (define renamed-evt (rename-event D+ name-env))
  (sbranch renamed-evt dest (list* (transition-to dest)
                                   (incorporate renamed-evt)
                                   (append assertion-updates
                                           msg-interest-updates
                                           effs-))))

;; [Setof τ] [Setof τ] [Hashof τ [Setof τ]] NameEnvironment -> [Listof SAction]
(define (transition-assertions curr-assertions dest-assertions event-subty# name-env)
  (define new-assertions (super-type-closure (set-subtract dest-assertions curr-assertions) event-subty#))
  (define retractions (super-type-closure (set-subtract curr-assertions dest-assertions) event-subty#))
  (define (lookup ty) (hash-ref name-env ty))
  (define asserts (set-map new-assertions (compose assert lookup)))
  (define retracts (set-map retractions (compose retract lookup)))
  (append asserts retracts))

;; [Setof τ] [Setof τ] [Hashof τ [Setof τ]] NameEnvironment -> [Listof SAction]
(define (transition-msg-interests curr-msg-txns dest-msg-txns event-subty# name-env)
  ;; TODO - not sure if super-type-closure needed here
  (define new-interests (set-subtract dest-msg-txns curr-msg-txns))
  (define lost-interests (set-subtract curr-msg-txns dest-msg-txns))
  (define (lookup ty) (hash-ref name-env ty))
  (define add-interests (set-map new-interests (compose add-message-interest lookup)))
  (define remove-interests (set-map lost-interests (compose remove-message-interest lookup)))
  (append add-interests remove-interests))

;; [Listof TransitionEffect] NameEnvironment -> [Listof SAction]
(define (rename-effects effs name-env)
  (for/list ([eff (in-list effs)])
    (match eff
      [(send ty)
       (send (hash-ref name-env ty))]
      [_
       (raise-argument-error 'rename-effects "only send effects supported" eff)])))

;; D+ NameEnvironment -> D+
(define (rename-event D+ name-env)
  (match D+
    [(Asserted τ)
     (Asserted (hash-ref name-env τ))]
    [(Retracted τ)
     (Retracted (hash-ref name-env τ))]
    [(Message τ)
     (Message (hash-ref name-env τ))]))

;; SpinLTL -> [LTL SName]
(define (rename-ltl ltl name-env)
  (define (lookup x)
    (match x
      [(Message τ) (Message (hash-ref name-env τ))]
      [τ (hash-ref name-env τ)]))
  (map-atomic ltl lookup))

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
    (define event# (hash
                    (Observe (Observe (Struct 'BookQuoteT (list (Base 'String) (Mk⋆)))))
                    (set)
                    (Struct 'BookQuoteT (list (Base 'String) (U (list (Base 'Int) (Base 'Int)))))
                    (set)))
    (define/timeout seller-spin (rg->spin seller-rg event# name-env))
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

(define SPIN-KEYWORDS
  '(active assert atomic bit bool break byte chan d_step D_proctype do
		else empty enabled fi full goto hidden if init int len mtype nempty
    never nfull od of pc_value printf priority proctype provided run
		short skip timeout typedef unless unsigned xr xs))

;; Symbol -> Symbol
(define (unkeyword s)
  (if (member s SPIN-KEYWORDS)
      (gensym s)
      s))

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
         (unkeyword (string->symbol without-added-prefix))
         (unkeyword (string->symbol match-str)))]))

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
  (cond
    [(set-empty? nm)
     (gensym 'inert)]
    [else
     (define (take-prefix s) (substring s 0 (min prefix (string-length s))))
     (define rough-name (string-join (set-map nm (compose take-prefix symbol->string)) "_"))
     (make-spin-id rough-name)]))

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
       [(== ⋆)
        (list 'star)]
       [(Base name)
        (list name)]
       [(List _)
        (list 'List)]
       [(Set _)
        (list 'Set)]
       [(Hash _ _)
        (list 'Hash)]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code Generation

(define-runtime-path SPIN-PRELUDE-PATH "spin-prelude.pml")

(define SPIN-PRELUDE (file->string SPIN-PRELUDE-PATH))

;; SpinProgram FilePath -> Void
(define (gen-spin/to-file spin name)
  (with-output-to-file name
    (lambda () (gen-spin spin))
    #:mode 'text
    #:exists 'replace))

;; SpinProgram -> Void
(define (gen-spin prog)
  (match prog
    [(sprog assignment procs spec msg-table name-env)
     (display SPIN-PRELUDE)
     (gen-assignment assignment)
     (newline)
     (for ([p procs])
       (gen-spin-proc p name-env)
       (newline))
     (gen-msg-dispatcher msg-table (map sproc-name procs))
     (gen-spec "spec" (lambda () (gen-ltl spec)))
     (newline)
     (gen-sanity-ltl assignment)]))

;; SpinProcess NameEnvironment -> Void
(define (gen-spin-proc proc name-env)
  (match-define (sproc name state-names locals init-actions states) proc)
  (indent) (declare-mtype state-names)
  (indent) (printf "active proctype ~a() {\n" name)
  (with-indent
    (gen-assignment locals)
    (indent) (displayln "atomic {")
    (for ([a init-actions])
      (gen-spin-form a name-env name))
    (indent) (displayln "}")
    (indent) (printf "~a: do\n" (format-end-label name))
    (with-indent
      (for ([st states])
        (gen-spin-form st name-env name)))
    (indent) (displayln "od;"))
  (indent) (displayln "}"))

;; SpinThang NameEnvironment SName -> Void
(define (gen-spin-form spin name-env proc-name)
  (match spin
    [(sstate name branches)
     (indent) (printf ":: ~a == ~a ->\n" (svar-name CURRENT-STATE) name)
     (cond
       [(empty? branches)
        ;; no transitions out of this state
        (indent) (printf "~a: false;\n" (format-end-label name))]
       [else
        (with-indent
          (indent) (displayln "if")
          (with-indent
            (for ([branch branches])
              (gen-spin-form branch name-env proc-name)))
          (indent) (displayln "fi;"))])]
    [(sbranch event dest actions)
     (indent) (printf ":: ~a -> ~a\n" (predicate-for event proc-name) (embed-event-as-comment event name-env))
     (with-indent
       (indent) (displayln "atomic {")
       (with-indent
         (for ([act actions])
           (gen-spin-form act name-env proc-name)))
       (indent) (displayln "}"))]
    [(assert x)
     (indent) (printf "ASSERT(~a); ~a\n" x (embed-value-as-comment assert x name-env))]
    [(retract x)
     (indent) (printf "RETRACT(~a); ~a\n" x (embed-value-as-comment retract x name-env))]
    [(send x)
     (indent) (printf "SEND(~a); ~a\n" x (embed-value-as-comment send x name-env))]
    [(add-message-interest x)
     (define interest-var-nm (msg-interest-var-name x proc-name))
     (indent) (printf "~a = true;\n" interest-var-nm)]
    [(remove-message-interest x)
     (define interest-var-nm (msg-interest-var-name x proc-name))
     (indent) (printf "~a = false;\n" interest-var-nm)]
    [(incorporate evt)
     (indent) (update-for evt proc-name)]
    [(transition-to dest)
     (indent) (printf "~a = ~a;\n" (svar-name CURRENT-STATE) dest)]))

;; MessageTable [Listof SName] -> Void
(define (gen-msg-dispatcher msg-table proc-names)
  (unless (hash-empty? msg-table)
    (indent) (displayln "active proctype __msg_dispatcher__() {")
    (with-indent
      (indent) (displayln "end: do")
      (with-indent
        (for ([(sent-msg matching-evts) (in-hash msg-table)])
          (gen-msg-dispatch sent-msg matching-evts proc-names)))
      (indent) (displayln "od;"))
    (indent) (displayln "}")))

;; SName (Setof SName) [Listof SName] -> Void
(define (gen-msg-dispatch sent-msg matching-evts proc-names)
  (define mailbox-nm (messages-var-name sent-msg))
  (indent) (printf ":: ~a > 0 ->\n" mailbox-nm)
  (with-indent
    (indent) (displayln "atomic {")
    (with-indent
      (indent) (printf "~a--;\n" mailbox-nm)
      (for ([proc (in-list proc-names)])
        (dispatch-to matching-evts proc)))
    (indent) (displayln "}")))

;; [Setof SName] SName -> Void
(define (dispatch-to matching-evts proc)
  (indent) (displayln "if")
  (with-indent
    (for ([msg (in-set matching-evts)])
      (define mailbox-nm (msg-mailbox-var-name msg proc))
      (define interest-nm (msg-interest-var-name msg proc))
      (indent) (printf ":: ~a > 0 -> ~a++\n" interest-nm mailbox-nm))
    (indent) (displayln ":: else -> skip;"))
  (indent) (displayln "fi;"))

;; [Setof SName] -> Void
(define (declare-mtype state-names)
  (display "mtype = {")
  (display (string-join (set-map state-names symbol->string) ", "))
  (displayln "}"))

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
     (~a v)]))

;; SType -> String
(define (spin-type->string ty)
  (match ty
    [(== SInt) "int"]
    [(== SBool) "bool"]
    [(== mtype) "mtype"]))

;; D+ -> String
(define (predicate-for event proc-name)
  (match event
    [(Asserted nm)
     (define assertion-var nm)
     (define active-var (active-var-name nm))
     (format "ASSERTED(~a) && !~a" assertion-var active-var)]
    [(Retracted nm)
     (define assertion-var nm)
     (define active-var (active-var-name nm))
     (format "RETRACTED(~a) && ~a" assertion-var active-var)]
    [(Message nm)
     (define mailbox-var (msg-mailbox-var-name nm proc-name))
     (format "~a > 0" mailbox-var)]))

;; D+ -> Void
(define (update-for event proc-name)
  (match event
    [(Asserted nm)
     (define active-var (active-var-name nm))
     (printf "~a = ~a;\n" active-var (spin-val->string #t))]
    [(Retracted nm)
     (define active-var (active-var-name nm))
     (printf "~a = ~a;\n" active-var (spin-val->string #f))]
    [(Message nm)
     (define mailbox-var (msg-mailbox-var-name nm proc-name))
     (printf "~a--;\n" mailbox-var)]))

;; D+ NameEnvironment -> String
(define (embed-event-as-comment event name-env)
  (define-values (kons id)
    (match event
      [(Asserted nm) (values Asserted nm)]
      [(Retracted nm) (values Retracted nm)]
      [(Message nm) (values Message nm)]))
  (embed-value-as-comment kons id name-env))

;; (τ -> Any) SName NameEnvironment -> String
(define (embed-value-as-comment tag sname name-env)
  (define ty (reverse-lookup name-env sname))
  (format "/*~a*/" (tag ty)))

;; NameEnvironment SName -> τ
(define (reverse-lookup name-env sname)
  (for/first ([(k v) (in-hash name-env)]
              #:when (equal? v sname))
    k))

;; String -> String
;; format a suitable end label based on the process/state name
(define (format-end-label s)
  (format "end_~a" s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LTL

;; String {-> Void} -> Void
(define (gen-spec name mk-body)
  (indent) (printf "ltl ~a {\n" name)
  (with-indent
    (mk-body))
  (newline)
  (indent) (displayln "}"))

;; SpinLTL -> Void
;; SpinLTL isn't quite right, not types but identifiers
(define (gen-ltl ltl)
  (match ltl
    [(always p)
     (indent) (displayln "[](")
     (with-indent
       (gen-ltl p))
     (indent) (displayln ")")]
    [(eventually p)
     (indent) (displayln "<>(")
     (with-indent
       (gen-ltl p))
     (indent) (displayln ")")]
    [(weak-until p q)
     (gen-ltl-bin-op "W" p q)]
    [(strong-until p q)
     (gen-ltl-bin-op "U" p q)]
    [(ltl-implies p q)
     (gen-ltl-bin-op "->" p q)]
    [(ltl-and p q)
     (gen-ltl-bin-op "&&" p q)]
    [(ltl-or p q)
     (gen-ltl-bin-op "||" p q)]
    [(ltl-not p)
     (indent) (display "!(")
     (gen-ltl p)
     (displayln ")")]
    [(atomic x)
     (match x
       [(Message nm)
        (printf "(~a > 0)\n" (messages-var-name nm))]
       [nm
        (printf "ASSERTED(~a)\n" nm)])]
    [#t
     (display "true")]
    [#f
     (display "false")]))

;; String [LTL SName] [LTL SName] -> Void
(define (gen-ltl-bin-op name p q)
  (indent) (display "(") (gen-ltl p) (display ") ")
  (displayln name)
  (newline)
  (indent) (display "(") (gen-ltl q) (displayln ")"))

;; Assignment -> Void
;; SPIN sometimes errors (seemingly in the front end) if this is "too big." What
;; constitutes too big seems to change. At first setting the limit to 33 worked,
;; but then I lowered it again, so IDK. It gives an error message like:
;;   tl_spin: expected ')', saw 'predicate'
(define (gen-sanity-ltl assignment)
  (gen-spec "sanity"
            (lambda ()
              (indent) (displayln "[](")
              (with-indent
                (for ([assertion-var (in-hash-keys assignment)]
                      [i (in-range 14)])
                  (indent) (printf "~a >= 0 &&\n" (svar-name assertion-var)))
                (indent) (displayln "true"))
              (indent) (displayln ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking Spin

(define-runtime-path RUN-SPIN.EXE "run-spin.sh")
(define-runtime-path REPLAY-TRAIL.EXE "replay-trail.sh")

;; [LTL τ] [Listof Role] -> Bool
(define (compile+verify spec roles)
  (let/ec stop
    (define role-graphs
      (for/list ([r (in-list roles)])
        (define ans (compile/internal-events (compile r)))
        (when (detected-cycle? ans)
          (printf "detected cycle!\n")
          (describe-detected-cycle ans)
          (stop #f))
        ans))
    (run-spin (program->spin role-graphs spec))))

;; SpinThang String -> Bool
(define (run-spin spin [spec-name "spec"])
  (define tmp (make-temporary-file "typed-syndicate-spin~a.pml"))
  (gen-spin/to-file spin tmp)
  (define-values (script-completed? script-output script-err)
    (run-script RUN-SPIN.EXE (list tmp spec-name)))
  (define trail-file (format "~a.trail" (path->string tmp)))
  (define trail-exists? (file-exists? trail-file))
  (cond
    [(not script-completed?)
     (displayln "Error running SPIN; Output:")
     (display script-err)
     (display script-output)]
    [trail-exists?
       (displayln "Detected Trail File!")
       (copy-file tmp (build-path (current-directory) "model.pml") #t)
       (copy-file trail-file (build-path (current-directory) "model.pml.trail") #t)
       (analyze-spin-trail tmp)
       (delete-file trail-file)])
  (delete-file tmp)
  (and script-completed? (not trail-exists?)))

(define SPIN-REPORT-RX #px"(?m:^State-vector \\d+ byte, depth reached \\d+, errors: (\\d+)$)")

;; String -> Bool
;; True if the model satisfies the spec, false otherwise
(define (analyze-spin-output out)
  (define rxmatch (regexp-match SPIN-REPORT-RX out))
  (unless rxmatch
    (error 'analyze-spin-output "unable to parse spin output"))
  (define num-errors (string->number (second rxmatch)))
  (zero? num-errors))

#|
Examples:
  4:	proc  2 (proc824:1) model.pml:140 (state 2)	[ClubMemberT_String_assertions = (ClubMemberT_String_assertions+1)]
  <<<<<START OF CYCLE>>>>>
|#
(define TRAIL-LINE-RX #px"(?m:^\\s*<<<<<START OF CYCLE>>>>>|^\\s*\\d+:\\s*proc\\s*(\\d+)\\s*\\(.*\\) \\S+\\.pml:(\\d+))")

;; Path -> Void
;; assume the trail file exists in the same directory as the spin (model) file
(define (analyze-spin-trail spin-file)
  (define-values (_ out __) (run-script REPLAY-TRAIL.EXE (list spin-file)))
  #;(pretty-display out)
  (define pid/line-trace (regexp-match* TRAIL-LINE-RX out #:match-select cdr))
  (define model-lines (file->vector spin-file))
  (define trace (interpret-spin-trace pid/line-trace model-lines))
  (print-trace trace)
  (log-trace-msd trace))

;; String (Listof String) -> (Values Bool String String)
(define (run-script cmd args)
  (match-define (list stdo stdin pid stderr ctrl)
    (apply process* cmd args))
  (ctrl 'wait)
  (define script-completed? (equal? (ctrl 'status) 'done-ok))
  (define script-output (port->string stdo))
  (define script-err (port->string stderr))
  (close-output-port stdin)
  (values script-completed? script-output script-err))

;; a PID is a Nat

;; a TraceStep is one of
;;   - (trace-step PID TraceEvent)
;;   - 'start-of-cycle
(struct trace-step (pid evt) #:prefab)
(define START-OF-CYCLE 'start-of-cycle)

;; a TraceEvent is one of
;;   - (assert τ)
;;   - (retract τ)
;;   - (Asserted τ)
;;   - (Retracted τ)

;; (Listof (List String String)) (Vectorof String) -> (Listof TraceStep)
(define (interpret-spin-trace pid/line-trace model-lines)
  (define maybe-steps
    (for/list ([item (in-list pid/line-trace)])
      (match item
        ['(#f #f)
         START-OF-CYCLE]
        [(list pid-str line-no-str)
         (define line (vector-ref model-lines (sub1 (string->number line-no-str))))
         (define evt (extract-comment-value line))
         (and evt
              (trace-step (string->number pid-str) evt))]
        )))
  (filter values maybe-steps))

;; (Listof TraceStep) -> Void
(define (print-trace trace)
  (when (empty? trace)
    (printf "Starting state of program violates specification\n"))
  (for ([ts (in-list trace)])
    (match ts
      [(== START-OF-CYCLE)
       (printf "Start of Cycle (if this is the last step that means the final state is stuttered):\n")]
      [(trace-step pid evt)
       (match evt
         [(assert ty)
          (printf "Process ~a ASSERTS ~a\n" pid (τ->string ty))]
         [(retract ty)
          (printf "Process ~a RETRACTS ~a\n" pid (τ->string ty))]
         [(send ty)
          (printf "Process ~a SENDS ~a\n" pid (τ->string ty))]
         [(Asserted ty)
          (printf "Process ~a REACTS to the ASSERTION of ~a\n" pid (τ->string ty))]
         [(Retracted ty)
          (printf "Process ~a REACTS to the RETRACTION of ~a\n" pid (τ->string ty))]
         [(Message ty)
          (printf "Process ~a REACTS to the MESSAGE of ~a\n" pid (τ->string ty))])])))

;; (Listof TraceStep) -> Void
;; use syndicate's msd logger logging
(define (log-trace-msd trace)
  (start-tracing! "trace.msd")
  (define (end-turn! pid point patch messages)
    (let* ([p (trace-turn-end point pid #f)]
           [p (trace-actions-produced p pid (cons patch messages))]
           [p (trace-action-interpreted p pid patch)])
      p))
  (define-values (final-pid final-point final-patch final-messages)
    (for/fold ([current-actor #f]
               [point #f]
               [current-patch synd:patch-empty]
               [messages (list)])
              ([ts (in-list trace)])
      (match ts
        [(== START-OF-CYCLE)
         (values current-actor point current-patch messages)]
        [(trace-step pid evt)
         (define-values (next-point next-patch)
           (cond
             ;; either startup or the begin of a new actor's turn
             [(and current-actor (not (equal? pid current-actor)))
              (define p (end-turn! current-actor point current-patch messages))
              (values (trace-turn-begin p pid #f)
                      synd:patch-empty)]
             [else
              (values point current-patch)]))
         (match evt
           [(assert ty)
            (define p (synd:assert ty))
            (values pid next-point (synd:patch-seq next-patch p) messages)]
           [(retract ty)
            (define p (synd:retract ty))
            (values pid next-point (synd:patch-seq next-patch p) messages)]
           [(send ty)
            (define a (synd:message ty))
            (values pid next-point next-patch (cons a messages))]
           [(Asserted ty)
            #;(trace-event-consumed ??? ??? pid ???)
            (values pid next-point next-patch messages)]
           [(Retracted ty)
            #;(trace-event-consumed ??? ??? pid ???)
            (values pid next-point next-patch messages)]
           [(Message ty)
            #;(trace-event-consumed ??? ??? pid ???)
            (values pid next-point next-patch messages)])])))
  (end-turn! final-pid final-point final-patch final-messages))

(define COMMENT-RX #px"/\\*(.*)\\*/")

;; String -> (Maybe TraceEvent)
(define (extract-comment-value line)
  (define rxmatch (regexp-match COMMENT-RX line))
  (and rxmatch
       (with-input-from-string (second rxmatch) read)))

(module+ test
  (test-case
      "extracting values back out from spin model"
    (define evt-str "        :: ASSERTED(BookQuoteT_String_Int) && !know_BookQuoteT_String_Int -> /*#s(Asserted #s(Struct BookQuoteT (#s(Base String) #s(Base Int))))*/\n")
    (define assert-str "  ASSERT(Obs_Obs_BookInterestT); /*#s(assert #s(Observe #s(Observe #s(Struct BookInterestT (#s(Base String) #s(Mk⋆) #s(Mk⋆))))))*/\n")
    (define send-str "            SEND(FlipSwitchCmdT_Symbol); /*#s(send #s(Struct FlipSwitchCmdT (#s(Base Symbol))))*/\n")
    (check-equal? (extract-comment-value evt-str)
                  #s(Asserted #s(Struct BookQuoteT (#s(Base String) #s(Base Int)))))
    (check-equal? (extract-comment-value assert-str)
                  #s(assert #s(Observe #s(Observe #s(Struct BookInterestT (#s(Base String) #s(Mk⋆) #s(Mk⋆)))))))
    (check-equal? (extract-comment-value send-str)
                  #s(send #s(Struct FlipSwitchCmdT (#s(Base Symbol)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Utils

;; [Hashof K V] -> [Setof V]
(define (hash-values-set h)
  (for/set ([x (in-hash-values h)])
    x))

;; Path -> (Vecotrof String)
(define (file->vector path)
  (list->vector (file->lines path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Case

(module+ leader-and-seller
  (define leader-rg (compile (parse-T
    '(Role ; = react
      (get-quotes)
      (Reacts ; = on
       (Asserted (BookQuoteT String (Bind Int)))
       (Branch
        (Effs (Branch (Effs (Stop get-quotes)) (Effs)))
        (Effs
         (Role
          (poll-members)
          (Reacts
           (Asserted (BookInterestT String (Bind String) Discard))
           (Branch
            (Effs (Stop poll-members (Branch (Effs (Stop get-quotes)) (Effs))))
            (Effs))
           (Branch
            (Effs
             (Stop get-quotes (Role (announce) (Shares (BookOfTheMonthT String)))))
            (Effs)))
          (Reacts (Retracted (BookInterestT String (Bind String) Bool)))
          (Reacts (Asserted (BookInterestT String (Bind String) Bool)))
          (Reacts (Retracted (BookInterestT String (Bind String) Bool)))
          (Reacts (Asserted (BookInterestT String (Bind String) Bool)))))))
      (Reacts (Retracted (ClubMemberT (Bind String))))
      (Reacts (Asserted (ClubMemberT (Bind String))))))))
  (define seller-rg (compile seller-actual))
  (define member-rg (compile member-actual))
  (define bq (book-quote String ⋆))
  (define bi (book-interest String ⋆ ⋆))
  (define book-club-spec
    (&& (eventually (atomic bq))
        (always (ltl-implies (atomic (Observe bq))
                             (eventually (atomic bq))))
        (always (ltl-implies (atomic (Observe bi))
                             (eventually (atomic bi))))))
  (define book-club-spin (program->spin (list leader-rg seller-rg member-rg)
                                        book-club-spec))
  (gen-spin/to-file book-club-spin "gen-book-club.pml"))

(module+ flink
  (define (import r)
    (define r+ (parse-T r))
    (compile/internal-events (compile r+) #f))
  (define jm-rg (import job-manager-actual))
  (define tm-rg (import task-manager-ty))
  (define tr-rg (import task-runner-ty))
  (define flink-spin (program->spin (list tr-rg tm-rg jm-rg)))
  (gen-spin/to-file flink-spin "gen-flink.pml"))
