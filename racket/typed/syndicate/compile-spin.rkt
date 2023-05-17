#lang racket

(provide run-spin
         compile+verify
         print-trace
         (struct-out named))

(require "proto.rkt")
(require "ltl.rkt")
(require racket/runtime-path)
(require syndicate/trace syndicate/trace/msd)
(require (prefix-in synd: (only-in syndicate/core assert retract message))
         (prefix-in synd: (only-in syndicate/patch patch-empty patch-seq)))

(require (only-in racket/hash hash-union))
(require syntax/parse/define)

(module+ test
  (require rackunit)
  (require "test-utils.rkt"))



;; a SpinProgram is a
;;   (sprog Assignment
;;          [Listof SpinProcess]
;;          IOSpec
;;          SpinLTL
;;          MessageTable
;;          [Setof SName]
;;          EventMap
;;          EventMap
;;          NameEnvironment)
(struct sprog [assignment procs io spec msg-tabe assertion-tys event-upcast# event-downcast# name-env] #:transparent)

;; a (Named X) is a
;;   (named SName X)
(struct named (name v) #:transparent)

;; a RG is (U (Named RoleGraph) RoleGraph)

;; a SpinProcess is a
;;   (sproc SName
;;          [Setof SName]
;;          SName
;;          Assignment
;;          [Listof SAction]
;;          [Setof τ]
;;          [Setof SpinState])
(struct sproc [name state-names st0 locals init-actions down-asserts states] #:transparent)

;; an IOSpec is a (spin-io (Setof τ) (Setof τ))
(struct spin-io (msgs asserts) #:transparent)

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
;;   - (assert τ)
;;   - (retract τ)
;;   - (unlearn ?)
;;   - (send τ)
;;   - (incorporate D+)
;;   - (add-message-interest ?)
;;   - (remove-message-interest ?)
;;   - (transition-to SName)
(struct assert [ty] #:prefab)
(struct retract [ty] #:prefab)
(struct unlearn [ty] #:prefab)
(struct add-message-interest [ty] #:prefab)
(struct remove-message-interest [ty] #:prefab)
;; send defined in proto.rkt
(struct incorporate [evt] #:transparent)
(struct transition-to [dest] #:transparent)

;; each process has a local variable that determines its current state
(define CURRENT-STATE (svar 'current mtype))

;; a NameEnvironment is a [Hashof τ SName]

;; an EventMap is a [Hashof τ [Setof τ]]
;; mapping a type to either its relevant subtypes or supertypes

;; a MessageTable is a (message-table EventMap EventMap)
;; mapping each possible message to its relevant super and subtypes
(struct message-table (up# down#) #:transparent)


;; a SpinLTL is a [LTL [U τ (Message τ)]]
;; where τ represents an assertion and (Message τ) a message


;; [Sequenceof RG] SpinLTL (Setof τ) -> SpinProgram
(define (program->spin rgs [spec #t] [io (set)])
  (define-values (distinct-rgs state-rename) (ensure-distinct-facet-names rgs))
  (define role-graphs (map RG->rg distinct-rgs))
  (define assertion-tys (all-assertions role-graphs))
  (define message-tys (all-messages role-graphs))
  (define event-tys (all-events role-graphs))
  (define-values (spec-asserts spec-msgs) (spec-types spec))
  (define-values (msg-event-tys assertion-evts) (partition-messages event-tys))
  (define-values (io-msgs io-assertions) (partition-messages io))
  (define msg-bodies (set-union message-tys msg-event-tys spec-msgs io-msgs))
  (define made-assertions (set-union assertion-tys io-assertions))
  (define read-assertions (set-union assertion-evts spec-asserts))
  (define event-downcast# (make-event-downcast-map made-assertions read-assertions))
  (define extra-downcasted-asserts (apply set-union (set) (hash-values event-downcast#)))
  (define all-made-assertions (set-union made-assertions extra-downcasted-asserts))
  (define event-upcast# (make-event-upcast-map all-made-assertions read-assertions))
  (define all-assertion-tys (set-union all-made-assertions read-assertions))
  (define all-mentioned-tys (set-union all-assertion-tys msg-bodies))
  (define name-env (make-name-env all-mentioned-tys))
  (define procs
    (for/list ([rg distinct-rgs])
      (match rg
        [(named name graph)
         (rg->spin graph state-rename event-upcast# event-downcast# name-env #:name name)]
        [_
         (rg->spin rg state-rename event-upcast# event-downcast# name-env)])))
  (define assertion-vars (initial-assertion-vars-for all-assertion-tys name-env))
  (define assertion-nms (for/set ([τ (in-set all-assertion-tys)]) (hash-ref name-env τ)))
  (define messages-vars (initial-message-vars-for msg-bodies name-env))
  (define mailbox-vars (initial-mailbox-vars msg-bodies (map sproc-name procs) name-env))
  (define all-sent-messages (set-union message-tys io-msgs))
  (define all-read-messages (set-union msg-event-tys spec-msgs))
  (define msg-table (make-message-table all-sent-messages all-read-messages name-env))
  (define globals (hash-union assertion-vars messages-vars mailbox-vars))
  (define sio (io->spin-io io-msgs io-assertions name-env event-upcast#))
  (define spec-spin (rename-ltl spec name-env))
  (sprog globals procs sio spec-spin msg-table assertion-nms event-upcast# event-downcast# name-env))

(define (io->spin-io io-msgs io-assertions name-env event-upcast#)
  (spin-io io-msgs
           io-assertions))

;; [Setof τ] [Setof τ] NameEnvironment -> MessageTable
(define (make-message-table message-tys msg-event-tys name-env)
  (define msg-downcast# (make-event-downcast-map message-tys msg-event-tys))
  (define extra-downcasted-msgs (apply set-union (set) (hash-values msg-downcast#)))
  (define all-sent-msgs (set-union message-tys extra-downcasted-msgs))
  (define msg-upcast# (make-event-upcast-map all-sent-msgs msg-event-tys))
  (message-table msg-upcast# msg-downcast#))

;; [Setof X] [Pred X] -> (Values [Setof X] [Setof X])
;; like partition on lists but for sets
(define (set-partition p s)
  (for/fold ([yays (set)]
             [nays (set)])
            ([x (in-set s)])
    (if (p x)
        (values (set-add yays x) nays)
        (values yays (set-add nays x)))))

;; RoleGraph [Hashof StateName SName] EventMap EventMap NameEnvironment -> SpinProcess
(define (rg->spin rg state-renaming* event-upcast# event-downcast# name-env #:name [name (gensym 'proc)])
  (match-define (role-graph st0 states) rg)
  ;; need to make sure the empty state is distinct between each process
  (define state-renaming (hash-set state-renaming* (set) (gensym 'inert)))
  (define all-events (all-event-types (in-hash-values states)))
  (define states- (for/list ([st (in-hash-values states)])
                    (state->spin st states all-events event-upcast# name-env state-renaming)))
  (define state-names (list->set (map sstate-name states-)))
  (define st0- (hash-ref state-renaming st0))
  ;; ergh the invariant for when I tack on _assertions to a name is getting tricksy
  (define st0-asserts (state-assertions (hash-ref states st0)))
  (define st0-msg-interests (message-transitions (state-transitions (hash-ref states st0))))
  (define initial-asserts (transition-assertions (set) st0-asserts all-events event-upcast# name-env))
  (define initial-msg-interests (transition-msg-interests (set) st0-msg-interests event-upcast# name-env))
  (define init-acts (append initial-asserts initial-msg-interests))
  (define assignment (local-variables-for st0- all-events name-env))
  (define made-assertions (all-assertions (list rg)))
  (define downcastable-asserts (for*/set ([asrt (in-set made-assertions)]
                                          [subtypes (in-value (hash-ref event-downcast# asrt))]
                                          #:unless (set-empty? (set-remove subtypes asrt)))
                                 asrt))
  (define relevant-assertions (for/set ([evt (in-set all-events)]
                                         #:unless (Message? evt))
                                (hash-ref name-env evt)))
  (sproc name state-names st0- relevant-assertions init-acts downcastable-asserts (list->set states-)))

;; State [Sequenceof State] [Setof [U τ (Message τ)]] [Hashof τ [Setof τ]] NameEnvironment [Hashof StateName SName] -> SpinState
(define (state->spin st states all-events event-upcast# name-env state-env)
  (match-define (state name transitions assertions) st)
  (define name- (hash-ref state-env name))
  (define msg-txns (message-transitions transitions))
  (define branches (for*/list ([(D+ txns) (in-hash transitions)]
                               [txn (in-set txns)])
                     (match-define (transition effs dest) txn)
                     (match-define (state _ dest-txns dest-assertions) (hash-ref states dest))
                     (define dest- (hash-ref state-env dest))
                     (define dest-msg-txns (message-transitions dest-txns))
                     (branch-on D+ assertions msg-txns dest- dest-assertions dest-msg-txns effs all-events event-upcast# name-env)))
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
(define (assertions-update-var-name s)
  (string->symbol (format "~a_update" s)))

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
;; map each type to all of its assertion super types
(define (make-event-upcast-map assertion-tys event-tys)
  ;; TODO - potentially use non-empty intersection
  (for/hash ([a (in-set assertion-tys)])
    (values a
            (all-supertypes-of a event-tys))))

;; τ [Setof τ] -> [Setof τ]
(define (all-supertypes-of τ tys)
  (for/set ([ty (in-set tys)]
            #:when (<:? τ ty))
    ty))

;; [Setof τ] [Hashof τ [Setof τ]]
(define (super-type-closure asserts event-upcast#)
  (for*/set ([a (in-set asserts)]
             [supers (in-value (hash-ref event-upcast# a))]
             [τ (in-set (set-add supers a))])
    τ))

;; [Setof τ] [Setof τ] -> [Hashof τ [Setof τ]]
;; map each made {assertion,message} type the set of event types that are its
;; subtypes
(define (make-event-downcast-map assertion-tys event-tys)
  ;; TODO - potentially use non-empty intersection
  (for/hash ([a (in-set assertion-tys)])
    (define subs (all-subtypes-of a event-tys))
    (define a* (explode-unions a))
    (values a
            (if (<:? (U a*) (U (set->list subs)))
                subs
                (set-add subs a)))))

;; τ -> [Listof τ]
(define (explode-unions ty)
  (match ty
    [(U tys)
     (apply append (map explode-unions tys))]
    [(Struct nm tys)
     (define τ* (map explode-unions tys))
     ;; (Listof τ) -> (Listof (Listof τ))
     (for/list ([tys (in-list (apply cartesian-product τ*))])
       (Struct nm tys))]
    [(Observe ty)
     (for/list ([ty* (in-list (explode-unions ty))])
       (Observe ty*))]
    [_ (list ty)]))

;; τ [Setof τ] -> [Setof τ]
(define (all-subtypes-of τ tys)
  (for/set ([ty (in-set tys)]
            #:when (<:? ty τ))
    ty))

;; [Setof τ] NameEnvironment -> Assignment
(define (initial-assertion-vars-for assertion-tys name-env)
  (for*/hash ([τ (in-set assertion-tys)]
              [bucket? (in-list '(#t #f))])
    (define namer (if bucket? assertions-var-name assertions-update-var-name))
    (values (svar (namer (hash-ref name-env τ)) SInt)
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

;; [Sequenceof State] -> [Setof [U τ (Message τ)]]
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

;; (Setof τ) -> (Values (Setof τ) (Setof τ))
;; partition the Message and non-Message types in a set,
;; returning the body of message types
(define (partition-messages tys)
  (define-values (msgs non-msgs) (set-partition Message? tys))
  (define msg-bodies (list->set (set-map msgs Message-ty)))
  (values msg-bodies non-msgs))

;; SName [Setof [U τ D+] NameEnvironment -> Assignment
(define (local-variables-for st0 all-events name-env)
  (define assign
    (for/hash ([evt (in-set all-events)]
               #:unless (Message? evt))
      (values (svar (active-var-name (hash-ref name-env evt))
                    SBool)
              #f)))
  assign
  #;(hash-set assign CURRENT-STATE st0))

;; [Setof SName] -> Void
(define (update-knowledge relevant-assertions)
  (for ([assert-nm (in-set relevant-assertions)])
    (define know-nm (active-var-name assert-nm))
    (indent) (printf "~a = (~a -> ASSERTED(~a) : ~a);\n" know-nm know-nm assert-nm know-nm)))

;; D+ [Setof τ] [Setof τ] SName [Setof τ] [Setof τ] [Listof TransitionEffect] [Setof [U τ (Message τ)]] [Hashof τ [Setof τ]] NameEnvironment -> SBranch
(define (branch-on D+ curr-assertions curr-msg-txns dest dest-assertions dest-msg-txns effs all-events event-upcast# name-env)
  (define assertion-updates (transition-assertions curr-assertions dest-assertions all-events event-upcast# name-env))
  (define msg-interest-updates (transition-msg-interests curr-msg-txns dest-msg-txns event-upcast# name-env))
  (define effs- (rename-effects effs name-env))
  (define renamed-evt (rename-event D+ name-env))
  (sbranch renamed-evt dest (list* (transition-to dest)
                                   (incorporate renamed-evt)
                                   (append assertion-updates
                                           msg-interest-updates
                                           effs-))))

;; [Setof τ] [Setof τ] [Setof [U τ (Message τ)]] [Hashof τ [Setof τ]] NameEnvironment -> [Listof SAction]
(define (transition-assertions curr-assertions dest-assertions all-events event-upcast# name-env)
  (define new-assertions (set-subtract dest-assertions curr-assertions))
  (define retractions (set-subtract curr-assertions dest-assertions))
  (define (lookup ty) (hash-ref name-env ty))
  (define asserts (set-map new-assertions assert))
  (define retracts (set-map retractions retract))
  (define unlearns (for/list ([τ (in-set retractions)]
                              #:when (and (Observe? τ)
                                          (set-member? all-events (Observe-ty τ))))
                     (unlearn (lookup (Observe-ty τ)))))
  (append asserts retracts unlearns))

;; [Setof τ] [Setof τ] [Hashof τ [Setof τ]] NameEnvironment -> [Listof SAction]
(define (transition-msg-interests curr-msg-txns dest-msg-txns event-upcast# name-env)
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
       (send ty)]
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
    (define renaming (hash (set 'seller27) 'sel
                           (set 'seller27 'during-inner29) 'sel_dur))
    (define/timeout seller-spin (rg->spin seller-rg renaming event# name-env))
    (check-true (sproc? seller-spin))))

(define tab-level (make-parameter 0))

(define TAB-WIDTH 2)

(define (indent)
  (display (make-string (* TAB-WIDTH (tab-level)) #\space)))

(define-syntax-rule (with-indent bdy ...)
  (parameterize ([tab-level (add1 (tab-level))])
    bdy ...))

(define SPIN_ID_RX #rx"^[a-zA-Z][a-zA-Z0-9_]*$")
(define SPIN_ID_TRAILING_CHAR #rx"[a-zA-Z0-9_]+")

;; (U Symbol String) -> Bool
(define (spin-id? s)
  (when (symbol? s)
    (set! s (symbol->string s)))
  (regexp-match? SPIN_ID_RX s))

(module+ test
  (check-not-false (spin-id? "hub"))
  (check-false (spin-id? "hub-impl")))

(define SPIN-KEYWORDS
  '(active assert atomic bit bool break byte chan d_step D_proctype do
		else empty enabled fi full goto hidden if init int len mtype nempty
    never nfull od of pc_value printf priority proctype provided run
		short skip timeout typedef unless unsigned xr xs))

;; Symbol -> Bool
(define (keyword? s)
  (member s SPIN-KEYWORDS))

;; Symbol -> Symbol
(define (unkeyword s)
  (if (keyword? s)
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

;; [Listof RG] -> [Listof RoleGraph]
;; rename duplicate facet names such that each one is globally unique
(define (ensure-distinct-facet-names rgs)
  (for/fold ([seen-names (set)]
             [renamed-rgs '()]
             #:result (values (reverse renamed-rgs)
                              (make-state-rename renamed-rgs)))
            ([rg* (in-list rgs)])
    (define rg (RG->rg rg*))
    (define facet-names (role-graph-facet-names rg))
    (define subst (for/hash ([fn (in-set facet-names)]
                             #:when (set-member? seen-names fn))
                    (values fn (gensym fn))))
    (define renamed-rg (rg-subst-facet-names rg subst))
    (define with-nm (if (named? rg*)
                        (struct-copy named rg* [v renamed-rg])
                        renamed-rg))
    (values (set-union seen-names facet-names)
            (cons with-nm renamed-rgs))))

;; RG -> RoleGraph
(define (RG->rg rg*)
  (if (named? rg*) (named-v rg*) rg*))

;; RoleGraph -> [Setof FacetName]
;; all FacetNames in a role graph
(define (role-graph-facet-names rg)
  (for*/set ([state-nm (in-hash-keys (role-graph-states rg))]
             [facet-nm (in-set state-nm)])
    facet-nm))

;; RoleGraph [Hashof FacetName FacetName] -> RoleGraph
(define (rg-subst-facet-names rg subst)
  (define st0 (role-graph-st0 rg))
  (define state# (role-graph-states rg))
  (role-graph (set-subst st0 subst)
              (for/hash ([(state-nm state) (in-hash state#)])
                (values (set-subst state-nm subst)
                        (state-subst state subst)))))

;; State [Hashof FacetName FacetName] -> State
;; rename facet names mentioned in a State according to a substitution
(define (state-subst st subst)
  (define state-nm (state-name st))
  (define txn# (state-transitions st))
  (define assertions (state-assertions st))
  (state (set-subst state-nm subst)
         (for/hash ([(D txns) (in-hash txn#)])
           (values D
                   (for/set ([txn (in-set txns)])
                     (transition-subst txn subst))))
         assertions))

;; Transition [Hashof FacetName FacetName] -> State
;; rename facet names mentioned in a State according to a substitution
(define (transition-subst txn subst)
  (define dest (transition-dest txn))
  (struct-copy transition txn [dest (set-subst dest subst)]))

;; [Setof FacetName] [Hashof FacetName FacetName] -> State
;; rename facet names mentioned in a State according to a substitution
(define (set-subst st subst)
  (for/set ([fn (in-set st)])
    (hash-ref subst fn fn)))

(module+ test
  (test-case "distinct names sanity"
    (define rg (role-graph (set 'root)
                           (hash (set 'root) (state (set 'root)
                                                    (hash)
                                                    (set)))))
    (check-equal? (role-graph-facet-names rg) (set 'root))
    (check-equal? (set-subst (set 'root 'loot 'foot) (hash 'root 'toot))
                  (set 'toot 'loot 'foot))
    (check-equal? (state-subst (state (set 'root) (hash) (set)) (hash 'root 'toot))
                  (state (set 'toot) (hash) (set)))
    (check-equal? (rg-subst-facet-names rg (hash 'root 'toot))
                  (role-graph (set 'toot)
                              (hash (set 'toot) (state (set 'toot)
                                                       (hash)
                                                       (set)))))
    (define-values (rg+ _renaming) (ensure-distinct-facet-names (list rg)))
    (check-equal? rg+
                  (list rg)))

  (test-case "distinct names actually distinct"
    (define rg (role-graph (set 'root)
                           (hash (set 'root) (state (set 'root)
                                                    (hash)
                                                    (set)))))
    (define-values (rgs _renaming) (ensure-distinct-facet-names (list rg rg)))
    (define fns (apply set-union (map role-graph-facet-names rgs)))
    (check-equal? (set-count fns)
                  2)
    ))

;; [Listof RG] -> [Hashof StateName SName]
(define (make-state-rename rgs)
  (define state-names (for*/set ([rg* (in-list rgs)]
                                 [rg (in-value (RG->rg rg*))]
                                 [state (in-hash-keys (role-graph-states rg))])
                        state))
  (let loop ([prefix 3])
    (define renaming (for/hash ([nm (in-set state-names)])
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

(module+ test
  (test-case "make-state-rename sanity"
    (define rg (role-graph (set 'root)
                           (hash (set 'root) (state (set 'root)
                                                    (hash)
                                                    (set)))))
    (check-equal? (make-state-rename (list rg))
                  (hash (set 'root) 'roo)))

  (test-case "make-state-rename inert distinct"
    (define rg (role-graph (set)
                           (hash (set) (state (set) (hash) (set)))))
    (define renaming (make-state-rename (list rg rg)))
    (check-equal? (set-count (hash-values-set renaming))
                  2)))

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
    [(sprog assignment procs io spec msg-table assertion-tys event-upcast# event-downcast# name-env)
     (display SPIN-PRELUDE)
     (gen-assignment assignment)
     (newline)
     (gen-assign GLOBAL-CLOCK-VAR GLOBAL-CLOCK-INIT-VAL #:declare #t)
     (newline)
     (gen-downcast-mtype event-downcast# name-env)
     (newline)
     (for ([p procs])
       (gen-spin-proc p name-env event-upcast# event-downcast# msg-table)
       (newline))
     (gen-clock-ticker (map sproc-name procs) msg-table assertion-tys name-env)
     (newline)
     (gen-io io event-upcast# event-downcast# msg-table name-env)
     (newline)
     (gen-spec "spec" (lambda () (gen-ltl spec)))
     (newline)
     (gen-sanity-ltl assertion-tys)]))

(define DOWNCAST-NONE '__none__)

;; EventMap NameEnv -> Void
(define (gen-downcast-mtype event-downcast# name-env)
  (define ty-nms (for*/set ([(ty subs) (in-hash event-downcast#)]
                            #:unless (set-empty? (set-remove subs ty))
                            [sub (in-set subs)])
                   (hash-ref name-env sub)))
  (declare-mtype (set-add ty-nms DOWNCAST-NONE)))

;; SName (-> Void) -> Void
(define (gen-spin-active-proctype nm gen-bdy)
  (indent) (printf "active proctype ~a() {\n" nm)
  (with-indent (gen-bdy))
  (indent) (displayln "}"))

(define-syntax-parse-rule (with-spin-active-proctype nm bdy ...+)
  (gen-spin-active-proctype nm (lambda () bdy ...)))


;; (-> Void) -> Void
;; generate the prelude for spin atomic sequences, call `gen-bdy`,
;; then end the atomic block
(define (with-spin-atomic* gen-bdy)
  (indent) (displayln "atomic {")
  (with-indent (gen-bdy))
  (indent) (displayln "}"))

(define-syntax-parse-rule (with-spin-atomic bdy ...+)
  (with-spin-atomic* (lambda () bdy ...)))

;; (-> Void) -> Void
;; generate the prelude for spin dstep sequences, call `gen-bdy`,
;; then end the dstep block
(define (gen-spin-dstep gen-bdy)
  (indent) (printf "d_step { ~a\n" (format-as-comment DSTEP-EVENT))
  (with-indent (gen-bdy))
  (indent) (displayln "}"))

(define-syntax-parse-rule (with-spin-dstep bdy ...+)
  (gen-spin-dstep (lambda () bdy ...)))

;; (-> Void) -> Void
(define (gen-spin-if gen-bdy)
  (indent) (displayln "if")
  (with-indent (gen-bdy))
  (indent) (displayln "fi;"))

(define-syntax-parse-rule (with-spin-if bdy ...+)
  (gen-spin-if (lambda () bdy ...)))

;; (-> Void) -> Void
(define (gen-spin-do gen-bdy)
  (indent) (displayln "do")
  (with-indent (gen-bdy))
  (indent) (displayln "od;"))

(define-syntax-parse-rule (with-spin-do bdy ...+)
  (gen-spin-do (lambda () bdy ...)))

(define (gen-spin-branch pred gen-body [comment ""])
  (indent) (printf ":: ~a -> ~a\n" pred comment)
  (with-indent (gen-body)))

(define-syntax-parse-rule (with-spin-branch pred (~optional (~seq #:comment comment))
                            body ...+)
  (gen-spin-branch pred (lambda () body ...) (~? comment)))

(define (gen-spin-break)
  (indent) (displayln "break;"))

(define (gen-spin-skip)
  (indent) (displayln "skip;"))

;; SpinProcess NameEnvironment EventMap EventMap MessageTable -> Void
(define (gen-spin-proc proc name-env event-upcast# event-downcast# msg-table)
  (match-define (sproc name state-names st0 relevant-assertions init-actions down-asserts states) proc)
  (define my-clock (proc-clock-var name))
  (define locals (for/hash ([evt (in-set relevant-assertions)])
                   (values (svar (active-var-name evt)
                                 SBool)
                           #f)))
  (define down-assert-locals (for/hash ([ty (in-set down-asserts)])
                               (values (svar (assertion-downcast-local-nm ty name-env) mtype)
                                       DOWNCAST-NONE)))
  (indent) (declare-mtype state-names)
  (indent) (gen-assign my-clock GLOBAL-CLOCK-INIT-VAL #:declare #t)
  (indent) (printf "active proctype ~a() {\n" name)
  (with-indent
    (gen-assign CURRENT-STATE st0 #:declare #t)
    (gen-assignment locals)
    (gen-assignment down-assert-locals)
    (unless (empty? init-actions)
      (with-spin-atomic
        (for ([a init-actions])
          (gen-spin-form a name-env event-upcast# event-downcast# msg-table name))))
    (indent) (printf "~a: do\n" (format-end-label name))
    (with-indent
      (with-spin-branch "true"
        (indent) (printf "~a;\n" (clock-predicate my-clock))
        (with-spin-atomic
          (indent) (update-clock my-clock)
          (with-spin-do
            (for ([st states])
              (gen-spin-form st name-env event-upcast# event-downcast# msg-table name)))
          (update-knowledge relevant-assertions))))
    (indent) (displayln "od;"))
  (indent) (displayln "}"))

;; SpinThang NameEnvironment EventMap EventMap MessageTable SName -> Void
(define (gen-spin-form spin name-env event-upcast# event-downcast# msg-table proc-name)
  (match spin
    [(sstate name branches)
     (indent) (printf ":: ~a == ~a ->\n" (svar-name CURRENT-STATE) name)
     (cond
       [(empty? branches)
        ;; no transitions out of this state
        #;(gen-spin-skip) (gen-spin-break)
        ]
       [else
        (with-indent
          (with-spin-if
            (for ([branch branches])
              (gen-spin-form branch name-env event-upcast# event-downcast# msg-table proc-name))
            (gen-spin-branch "else" #;gen-spin-skip gen-spin-break)))])]
    [(sbranch event dest actions)
     (indent) (printf ":: ~a -> ~a\n" (predicate-for event proc-name) (embed-event-as-comment event name-env))
     (with-indent
       (with-indent
         (for ([act actions])
           (gen-spin-form act name-env event-upcast# event-downcast# msg-table proc-name))))]
    [(assert ty)
     (define subs (hash-ref event-downcast# ty))
     (define supers (hash-ref event-upcast# ty))
     (cond
       [(set-empty? (set-remove subs ty))
        (assert-all ty supers name-env)]
       [else
        (assert-one-of ty subs event-upcast# name-env)])]
    [(retract ty)
     (define subs (hash-ref event-downcast# ty))
     (define supers (hash-ref event-upcast# ty))
     (cond
       [(set-empty? (set-remove subs ty))
        (retract-all ty supers name-env)]
       [else
        (retract-one-of ty subs event-upcast# name-env)])]
    [(unlearn x)
     (indent) (printf "~a = false;\n" (active-var-name x))]
    [(send ty)
     (gen-spin-send! ty msg-table name-env)]
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

;; τ [Setof τ] NameEnv -> Void
;; assert ty and all of its super types
(define (assert-all ty supers name-env)
  (spin-assert! ty name-env)
  (for ([super (in-set (set-remove supers ty))])
    (spin-assert! super name-env)))

;; τ [Setof τ] EventMap NameEnv
;; assert one of ty's subtypes, and all of that subtypes supertypes.
;; Incorporates the choice of subtypes into a local variable whose name is based
;; on ty.
(define (assert-one-of ty subs event-upcast# name-env)
  (define local-nm (assertion-downcast-local-nm ty name-env))
  (with-spin-if
    (for ([sub (in-set subs)])
      (define supers (hash-ref event-upcast# sub))
      (with-spin-branch "true"
        (assert-all sub supers name-env)
        (indent) (printf "~a = ~a;\n" local-nm (hash-ref name-env sub))))))

;; τ NameEnv -> SName
(define (assertion-downcast-local-nm ty name-env)
  (format "~a_choice" (hash-ref name-env ty)))

(define (spin-assert! ty name-env)
  (define nm (hash-ref name-env ty))
  (indent) (printf "ASSERT(~a); ~a\n" nm (format-as-comment (assert ty))))

;; τ [Setof τ] NameEnv -> Void
;; retract ty and all of its super types
(define (retract-all ty supers name-env)
  (spin-retract! ty name-env)
  (for ([super (in-set (set-remove supers ty))])
    (spin-retract! super name-env)))

;; τ [Setof τ] EventMap NameEnv
;; retract the subtype of ty being asserted, based on the local variable for ty,
;; and all of that subtypes supertypes.
(define (retract-one-of ty subs event-upcast# name-env)
  (define local-nm (assertion-downcast-local-nm ty name-env))
  (with-spin-if
    (for ([sub (in-set subs)])
      (define sub-nm (hash-ref name-env sub))
      (define supers (hash-ref event-upcast# sub))
      (with-spin-branch (format "~a == ~a" local-nm sub-nm)
        (retract-all sub supers name-env)
        (indent) (printf "~a = ~a;\n" local-nm DOWNCAST-NONE)))))

(define (spin-retract! ty name-env)
  (define nm (hash-ref name-env ty))
  (indent) (printf "RETRACT(~a); ~a\n" nm (format-as-comment (retract ty))))

;; τ MessageTable NameEnv -> Void
(define (gen-spin-send! ty msg-table name-env)
  (define subtys (hash-ref (message-table-down# msg-table) ty))
  (cond
    [(set-empty? (set-remove subtys ty))
     (spin-send! ty name-env)]
    [else
     (send-one-of (set-remove subtys ty) name-env)]))

;; [Setof τ] NameEnvironment -> Void
(define (send-one-of tys name-env)
  (with-spin-if
    (for ([msg (in-set tys)])
      (with-spin-branch "true"
        (spin-send! msg name-env)))))

;; τ NameEnvironment -> Void
(define (spin-send! ty name-env)
  (define nm (hash-ref name-env ty))
  (indent) (printf "SEND(~a); ~a\n" nm (format-as-comment (send ty))))

;; [Listof SName] MessageTable [Setof SName] NameEnvironent -> Void
(define (gen-clock-ticker proc-names msg-table assertion-tys name-env)
  (define clock-names (for/list ([pn (in-list proc-names)])
                        (svar-name (proc-clock-var pn))))
  (indent) (displayln "active proctype __clock_ticker__() {")
  (with-indent
    (indent) (displayln "end_clock_ticker:")
    (with-spin-do
      (with-spin-branch (spin-&& (all-procs-ready-predicate clock-names)
                                 (any-activity-predicate assertion-tys msg-table name-env))
        (with-spin-dstep
          (indent) (update-clock GLOBAL-CLOCK-VAR (format-as-comment TURN-BEGIN-EVENT))
          (update-all-assertion-vars assertion-tys)
          (unless (message-table-empty? msg-table)
            (with-spin-do
              (gen-spin-branch "else" gen-spin-break)
              (for ([(sent-msg matching-evts) (in-hash (message-table-up# msg-table))])
                (gen-msg-dispatch sent-msg matching-evts proc-names name-env))))))))
  (indent) (displayln "}"))

;; MessageTable -> Bool
(define (message-table-empty? mt)
  (hash-empty? (message-table-up# mt)))

;; (Setof SName) -> Void
;; update the assertion bucket variables based on the update variables
(define (update-all-assertion-vars assertion-nms)
  (for ([assertion (in-set assertion-nms)])
    (define bucket (assertions-var-name assertion))
    (define update (assertions-update-var-name assertion))
    (indent) (printf "~a = ~a + ~a;\n" bucket bucket update)
    (indent) (printf "~a = 0;\n" update)))

;; τ (Setof τ) [Listof SName] NameEnv -> Void
(define (gen-msg-dispatch sent-msg matching-evts proc-names name-env)
  (define sent-nm (hash-ref name-env sent-msg))
  (define matching-nms (rename-all name-env matching-evts))
  (define mailbox-nm (messages-var-name sent-nm))
  (indent) (printf ":: ~a > 0 ->\n" mailbox-nm)
  (with-indent
    (indent) (printf "~a--;\n" mailbox-nm)
    (for ([proc (in-list proc-names)])
      (dispatch-to matching-nms proc))))

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

(define IO-PROC-NAME 'IO)
(define (gen-io io event-upcast# event-downcast# msg-table name-env)
  (match-define (spin-io msgs asserts) io)
  ;; in order for deadlock checking to work, it seems like there needs to be
  ;; some way for the clock to keep ticking
  (define down-asserts (for*/set ([asrt (in-set asserts)]
                                  [subtypes (in-value (hash-ref event-downcast# asrt))]
                                  #:when (> (set-count subtypes) 1))
                                 asrt))
  (define down-assert-locals (for/hash ([ty (in-set down-asserts)])
                               (values (svar (assertion-downcast-local-nm ty name-env) mtype)
                                       DOWNCAST-NONE)))
  (define assert-locals (for/hash ([asrt (in-set asserts)])
                          (values (svar (io-assert-var-name (hash-ref name-env asrt))
                                        SBool)
                                  #f)))
  (define fuel-nm 'fuel)
  (define fuel-nonzero #;"true" (format "~a > 0" fuel-nm))
  (with-spin-active-proctype (~a IO-PROC-NAME)
    (gen-assignment assert-locals)
    (gen-assignment down-assert-locals)
    (gen-assignment (hash (svar fuel-nm SInt) 100))
    (with-spin-do
      (with-spin-branch "timeout"
        (with-spin-atomic
          (with-spin-if
            (gen-spin-branch "else" gen-spin-skip)
            #;(with-spin-branch fuel-nonzero
              (printf "~a--\n" fuel-nm)))
          (with-spin-if
            (gen-spin-branch "else" gen-spin-skip)
            (for ([msg (in-set msgs)])
              (with-spin-branch fuel-nonzero
                (gen-spin-form (send msg) name-env event-upcast# event-downcast# msg-table IO-PROC-NAME)))
            (for ([asrt (in-set asserts)])
              (define local-nm (io-assert-var-name (hash-ref name-env asrt)))
              (with-spin-branch (spin-&& (format "!~a" local-nm) fuel-nonzero)
                (gen-spin-form (assert asrt) name-env event-upcast# event-downcast# msg-table IO-PROC-NAME)
                (indent) (printf "~a = !~a\n" local-nm local-nm))
              (with-spin-branch (spin-&& local-nm fuel-nonzero)
                (gen-spin-form (retract asrt) name-env event-upcast# event-downcast# msg-table IO-PROC-NAME)
                (indent) (printf "~a = !~a\n" local-nm local-nm)))
            #;(gen-spin-branch "true" gen-spin-break))
          ;; get the clock-ticker moving again
          (indent) (update-clock GLOBAL-CLOCK-VAR))))))

;; SName -> SName
(define (io-assert-var-name s)
  (string->symbol (format "asserting_~a" s)))

;; [Setof SName] -> Void
(define (declare-mtype state-names)
  (display "mtype = {")
  (display (string-join (set-map state-names symbol->string) ", "))
  (displayln "}"))

;; Assignment -> Void
(define (gen-assignment assign)
  (for ([(var val) (in-hash assign)])
    (gen-assign var val #:declare #t)))

;; SVar SValue [Bool] -> Void
(define (gen-assign var val #:declare [declare? #f])
  (indent) (printf "~a = ~a;\n"
                   (if declare? (var-decl var) (svar-name var))
                   (spin-val->string val)))

;; SVar -> String
(define (var-decl var)
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
    [(== SInt) "short"]
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
  (format-as-comment (tag ty)))

;; Any -> String
(define (format-as-comment v)
  (format "/*~a*/" v))

;; NameEnvironment SName -> τ
(define (reverse-lookup name-env sname)
  (for/first ([(k v) (in-hash name-env)]
              #:when (equal? v sname))
    k))

;; String -> String
;; format a suitable end label based on the process/state name
(define (format-end-label s)
  (format "end_~a" s))

;; SName -> SVar
;; SVar for a process's clock
(define (proc-clock-var proc-name)
  (svar (string->symbol (format "~a_clock" proc-name))
        SBool))

(define GLOBAL-CLOCK-VAR (svar 'GLOBAL_CLOCK SBool))
(define GLOBAL-CLOCK-INIT-VAL #t)

;; SVar -> String
(define (clock-predicate clock-var)
  (format "~a == ~a" (svar-name clock-var) (svar-name GLOBAL-CLOCK-VAR)))

;; SVar -> Void
(define (update-clock clock [comment ""])
  (printf "~a = !~a;~a\n" (svar-name clock) (svar-name clock) comment))

;; (Listof SName) -> String
(define (all-procs-ready-predicate clock-names)
  (define global-name (svar-name GLOBAL-CLOCK-VAR))
  (define preds (for/list ([cn (in-list clock-names)])
                  (format "(~a != ~a)" global-name cn)))
  (spin-&&* preds))

;; [Setof SName] MessageTable NameEnv -> String
(define (any-activity-predicate assertion-tys msg-table name-env)
  (define global-name (svar-name GLOBAL-CLOCK-VAR))
  (define assertion-nms (set-map assertion-tys assertions-update-var-name))
  (define msg-nms (set-map (rename-all name-env (list->set (hash-keys (message-table-up# msg-table))))
                           messages-var-name ))
  (spin-||* (append assertion-nms msg-nms)))

(define (spin-&& a b)
  (format "(~a && ~a)" a b))

(define (spin-&&* ps)
  (fold-bin-op spin-&& ps "true"))

(define (spin-|| a b)
  (format "(~a || ~a)" a b))

(define (spin-||* ps)
  (fold-bin-op spin-|| ps "false"))

(define (fold-bin-op op args base)
  (let loop ([args args])
    (match args
      ['()
       base]
      [(list x)
       x]
      [(list x y)
       (op x y)]
      [(cons fst rst)
       (op fst (loop rst))])))

(module+ test
  (check-equal? (spin-||* '(1 2 3))
                "(1 || (2 || 3))"))

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
    [(release p q)
     (gen-ltl-bin-op "V" p q)]
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

;; [Setof SName] -> Void
;; SPIN sometimes errors (seemingly in the front end) if this is "too big." What
;; constitutes too big seems to change. At first setting the limit to 33 worked,
;; but then I lowered it again, so IDK. It gives an error message like:
;;   tl_spin: expected ')', saw 'predicate'
(define (gen-sanity-ltl assertion-tys)
  (gen-spec "sanity"
            (lambda ()
              (indent) (displayln "[](")
              (with-indent
                (for ([assertion-ty (in-set assertion-tys)]
                      [i (in-range 14)])
                  (indent) (printf "~a >= 0 &&\n" (assertions-var-name assertion-ty)))
                (indent) (displayln "true"))
              (indent) (displayln ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking Spin

(define-runtime-path RUN-SPIN.EXE "run-spin.sh")
(define-runtime-path REPLAY-TRAIL.EXE "replay-trail.sh")

;; [LTL τ] [Listof (U (Named Role) Role)] τ -> (U #t String SpinTrace)
;; returns #true if verification succeeds
;; returns a string error message if compilation fails
;; returns a SpinTrace of a counterexample if verification fails
(define (compile+verify spec roles io)
  (let/ec stop
    (define role-graphs
      (for/list ([nr? (in-list roles)])
        (define-values (nm? r)
          (match nr?
            [(named nm v)
             (cond
               [(keyword? nm)
                (stop (format "Unable to use SPIN keyword as name: ~a\n" nm))]
               [(spin-id? nm)
                (values nm v)]
               [else
                (define transformed (rkt-nm->spin-id nm))
                (unless (spin-id? transformed)
                  (stop (format "Unable to use SPIN keyword as name: ~a\n" nm)))
                (values transformed v)])]
            [_ (values #f nr?)]))
        (define ans (compile/internal-events (compile r)))
        (when (detected-cycle? ans)
          (printf "detected cycle!\n")
          (describe-detected-cycle ans)
          (stop "compilation failed"))
        (if nm?
            (named nm? ans)
            ans)))
    (define io* (ty->set io))
    (run-spin (program->spin role-graphs spec io*))))

(define (ty->set ty)
  (match ty
    [(U tys)
     (list->set tys)]
    [_
     (set ty)]))

;; Symbol -> Symbol
;; try to convert a racket name to a suitable SPIN identifier by replace illegal
;; characters with '_'
(define (rkt-nm->spin-id nm)
  (define chars
    (for/list ([ch (in-string (symbol->string nm))])
      (cond
        [(char-alphabetic? ch)
         ch]
        [(char-numeric? ch)
         ch]
        [else
         #\_])))
  (string->symbol (list->string chars)))

(module+ test
  (check-equal? (rkt-nm->spin-id 'basic)
                'basic)
  (check-equal? (rkt-nm->spin-id 'ba1337sic)
                'ba1337sic)
  (check-equal? (rkt-nm->spin-id 'two-parts)
                'two_parts)
  (check-equal? (rkt-nm->spin-id 'two-parts/ext)
                'two_parts_ext)
  (check-equal? (rkt-nm->spin-id 'list*)
                'list_))

;; SpinThang String -> (U #t String SpinTrace)
(define (run-spin spin [spec-name "spec"])
  (define tmp (make-temporary-file "typed-syndicate-spin~a.pml"))
  (gen-spin/to-file spin tmp)
  (copy-file tmp (build-path (current-directory) "model.pml") #t)
  (define-values (script-completed? script-output script-err)
    (run-script RUN-SPIN.EXE (list tmp spec-name)))
  (define trail-file (format "~a.trail" (path->string tmp)))
  (define trail-exists? (file-exists? trail-file))
  (define maybe-trace #f)
  (define maybe-err #f)
  (cond
    [(not script-completed?)
     (set! maybe-err
           (with-output-to-string
             (lambda ()
               (displayln "Error running SPIN; Output:")
               (display script-err)
               (display script-output))))]
    [trail-exists?
     #;(displayln "Detected Trail File!")
     (copy-file tmp (build-path (current-directory) "model.pml") #t)
     (copy-file trail-file (build-path (current-directory) "model.pml.trail") #t)
     (set! maybe-trace (analyze-spin-trail tmp))
     (delete-file trail-file)])
  (delete-file tmp)
  (or maybe-err maybe-trace #t))

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
(define TRAIL-LINE-RX #px"(?m:^\\s*<<<<<START OF CYCLE>>>>>|^\\s*\\d+:\\s*proc\\s*(\\d+)\\s*\\(([^:]*):\\d*\\) \\S+\\.pml:(\\d+))")

;; Path -> SpinTrace
;; assume the trail file exists in the same directory as the spin (model) file
(define (analyze-spin-trail spin-file)
  (define-values (_ out __) (run-script REPLAY-TRAIL.EXE (list spin-file)))
  #;(pretty-display out)
  (define trace (spin-trace->syndicate-trace out spin-file))
  trace
  #;(print-trace trace)
  #;(log-trace-msd trace))

;; String Path -> SpinTrace
(define (spin-trace->syndicate-trace spin-out spin-file)
  (define pid/line-trace (regexp-match* TRAIL-LINE-RX spin-out #:match-select cdr))
  (define model-lines (file->vector spin-file))
  (interpret-spin-trace pid/line-trace model-lines))

;; String (Listof String) -> (Values Bool String String)
(define (run-script cmd args)
  (match-define (list stdo stdin pid stderr ctrl)
    (apply process* cmd args))
  (define script-output (port->string stdo))
  (define script-err (port->string stderr))
  (define script-completed? (equal? (ctrl 'status) 'done-ok))
  (close-output-port stdin)
  (values script-completed? script-output script-err))

;; a SpinTrace is a (Listof TraceStep)
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
;;   - 'dstep
;;   - 'turn-begin

;; the first statement in a d_step sequence (possibly atomic too) has the line
;; number of the d_step block itself in the trace
(define DSTEP-EVENT 'dstep)
(define TURN-BEGIN-EVENT 'turn-begin)

;; (Listof (List String String)) (Vectorof String) -> SpinTrace
(define (interpret-spin-trace pid/line-trace model-lines)
  (define maybe-steps
    (for/list ([item (in-list pid/line-trace)])
      (match item
        ['(#f #f #f)
         START-OF-CYCLE]
        [(list pid-str proc-name-str line-no-str)
         (define line-no (string->number line-no-str))
         (extract-trace-step pid-str proc-name-str line-no model-lines)])))
  (filter values maybe-steps))

;; String String Nat (Vectorof String) -> (Maybe TraceEvent)
(define (extract-trace-step pid-str proc-name-str line-no model-lines)
  (define line (vector-ref model-lines (sub1 line-no)))
  (define evt (extract-comment-value line))
  (cond
    [(equal? evt DSTEP-EVENT)
     (extract-trace-step pid-str proc-name-str (add1 line-no) model-lines)]
    [evt
     (trace-step proc-name-str #;(string->number pid-str) evt)]
    [else
     #f]))

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
         [(== TURN-BEGIN-EVENT)
          (printf "A new turn begins\n")]
         [(== DSTEP-EVENT)
          (printf "\nERROR ERROR!! SAW DSTEP EVENT!! ERROR ERROR\n\n")]
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
           [(== DSTEP-EVENT)
            (printf "\nERROR ERROR!! SAW DSTEP EVENT!! ERROR ERROR\n\n")]
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
            (values pid next-point next-patch messages)]
           [(or (== TURN-BEGIN-EVENT)
                (== DSTEP-EVENT))
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
