#lang racket

(require "proto.rkt")

(module+ test
  (require rackunit)
  (require "test-utils.rkt"))

;; a SpinProgram is a
;;   (sprog [Assignment [Listof SpinProcess]])
(struct sprog [assignment procs] #:transparent)


;; a SpinProcess is a
;;   (sproc SName Assignment [Setof SpinState])
(struct sproc [name init states] #:transparent)

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

;; [Sequenceof RoleGraph] -> SpinProgram
(define (compile-program rgs)
  (define globals (initial-assertion-vars-for rgs))
  (define procs (for/list ([rg rgs]) (compile-spin rg)))
  (sprog globals procs))

;; RoleGraph -> SpinProcess
(define (compile-spin rg #:name [name (gensym 'proc)])
  (match-define (role-graph st0 states) rg)
  (define all-events (all-event-types (in-hash-values states)))
  (define states- (for/list ([st (in-hash-values states)])
                    (compile-state st states)))
  (define assignment (local-variables-for st0 all-events))
  (sproc name assignment (list->set states-)))

;; [Sequenceof RoleGraph] -> Assignment
(define (initial-assertion-vars-for rgs)
  (define potential-assertions (all-assertions rgs))
  (for/hash ([τ (in-set potential-assertions)])
    (values (svar τ SInt)
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

;; StateName [Setof τ] -> Assignment
(define (local-variables-for st0 all-events)
  (define assign
    (for/hash ([evt (in-set all-events)])
      (values (svar evt SBool)
              #f)))
  (hash-set assign CURRENT-STATE st0))

;; State -> SpinState
(define (compile-state st states)
  (match-define (state name transitions assertions) st)
  (define branches (for*/list ([(D+ txns) (in-hash transitions)]
                               [txn (in-set txns)])
                     (match-define (transition effs dest) txn)
                     (match-define (state _ _ dest-assertions) (hash-ref states dest))
                     (branch-on D+ assertions dest dest-assertions effs)))
  (sstate name branches))

;; D+ [Setof τ] SName [Setof τ] [Listof TransitionEffect] -> SBranch
(define (branch-on D+ curr-assertions dest dest-assertions effs)
  (define new-assertions (set-subtract dest-assertions curr-assertions))
  (define retractions (set-subtract curr-assertions dest-assertions))
  (define asserts (set-map new-assertions assert))
  (define retracts (set-map retractions retract))
  (unless (andmap send? effs)
    (raise-argument-error 'branch-on "all external effects" effs))
  (sbranch D+ dest (list* (transition-to dest)
                          (incorporate D+)
                          (append asserts retracts effs))))

(module+ test
  (test-case
      "sanity: compile book seller type"
    (define/timeout seller-rg (compile seller-actual))
    (define/timeout seller-spin (compile-spin seller-rg))
    (check-true (sproc? seller-spin))))

(define tab-level (make-parameter 0))

(define TAB-WIDTH 2)

(define (indent)
  (display (make-string (* TAB-WIDTH (tab-level)) #\space)))

(define-syntax-rule (with-indent bdy ...)
  (parameterize ([tab-level (add1 (tab-level))])
    bdy ...))

;; SpinThang -> Void
(define (gen-spin spin)
  (match spin
    [(sprog assignment procs)
     #f]
    [(sproc name init states)
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
     (indent) (displayln "}")
     #f]
    [(sstate name branches)
     (indent) (printf ":: ~a == ~a ->\n" (svar-name CURRENT-STATE) name)
     (with-indent
       (indent) (displayln "if")
       (with-indent
         (for ([branch branches])
           (gen-spin branch)))
       (indent) (displayln "fi;"))]
    [(sbranch event dest actions)
     (indent) (printf "~a ->\n" (predicate-for event))
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
    [(Asserted ty)
     (define assertion-var (assertion-var-name event))
     (define active-var (active-var-name event))
     (format "ASSERTED(~a) && !~a" assertion-var active-var)]
    [(Retracted ty)
     (define assertion-var (assertion-var-name event))
     (define active-var (active-var-name event))
     (format "RETRACTED(~a) && ~a" assertion-var active-var)]
    [(Message ty)
     (raise-argument-error 'predicate-for "message sending not supported yet" event)]))

;; D+ -> Void
(define (update-for event)
  (match event
    [(Asserted ty)
     (define active-var (active-var-name event))
     (printf "~a = ~a;\n" active-var (spin-val->string #t))]
    [(Retracted ty)
     (define active-var (active-var-name event))
     (printf "~a = ~a;\n" active-var (spin-val->string #f))]
    [(Message ty)
     (raise-argument-error 'predicate-for "message sending not supported yet" event)]))

;; D+ -> String
(define (assertion-var-name event)
  ;; TODO
  (~a event))

(define (active-var-name event)
  ;; TODO
  (~a event))
