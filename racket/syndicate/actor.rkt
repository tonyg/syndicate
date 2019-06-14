#lang racket/base

(provide spawn
         spawn*
         dataspace

         react
         react/suspend
         until
         forever

         current-facet-id
         field
         field/c
         assert
         stop-facet
         stop-current-facet
         stop-when
         stop-when-true
         on-start
         on-stop
         on-event
         on-event*
         on
         during
         during/spawn
         begin/dataflow
         define/dataflow

         asserted
         retracted
         rising-edge
         (rename-out [core:message message])
         know
         forget
         realize

         let-event

         query-value
         query-set
         query-hash
         query-hash-set
         query-count
         query-value*
         query-set*
         query-hash*
         query-hash-set*
         query-count*
         define/query-value
         define/query-set
         define/query-hash
         define/query-hash-set
         define/query-count
         immediate-query

         send!
         assert!
         retract!
         patch!
         perform-actions!
         flush!
         quit-dataspace!
         realize!

         syndicate-effects-available?

         ? ;; from pattern.rkt

         ;;

         current-action-transformer
         schedule-action!
         schedule-actions!
         actor-action
         (for-syntax (rename-out [name actor-name]))

         pretty-print-actor-state
         )

(module reader syntax/module-reader
  syndicate/actor-lang)

(require racket/set)
(require racket/match)
(require racket/contract)
(require (only-in racket/list flatten))

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax syntax/srcloc))
(require "syntax-classes.rkt")

(require (prefix-in core: "core.rkt"))
(require (prefix-in core: "dataspace.rkt"))
(require "mux.rkt")
(require "patch.rkt")
(require "trie.rkt")
(require "pattern.rkt")
(require "dataflow.rkt")
(require "store.rkt")
(require "support/hash.rkt")
(require "pretty.rkt")
(require "functional-queue.rkt")
(require "protocol/instance.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions and Structures

;; A FieldTable is a WEAK hash table: (FieldDescriptor |-> Any)

;; (field-descriptor Symbol UniqueNatural)
(struct field-descriptor (name id)
  #:methods gen:custom-write
  [(define (write-proc d p mode)
     (fprintf p "#<field-descriptor:~a>" (field-descriptor-name d)))])

;; (field-handle FieldDescriptor)
(struct field-handle (desc)
  #:methods gen:custom-write
  [(define (write-proc h p mode)
     (fprintf p "#<field-handle:~a>" (field-descriptor-name (field-handle-desc h))))]
  #:property prop:procedure
  (case-lambda
    [(handle)
     (define desc (field-handle-desc handle))
     (dataflow-record-observation! (actor-state-field-dataflow (current-actor-state)) desc)
     (field-ref desc)]
    [(handle v)
     (define desc (field-handle-desc handle))
     (dataflow-record-damage! (actor-state-field-dataflow (current-actor-state)) desc)
     (field-set! desc v)]))

(define (make-field-proxy field guard wrap)
  (case-lambda
    [() (wrap (field))]
    [(x) (field (guard x))]))

(define/subexpression-pos-prop field/c
  (case-lambda
    [(ctc)
     (let ([ctc (coerce-contract 'field/c ctc)])
       (make-field/c ctc ctc #f))]
    [(in out)
     (make-field/c (coerce-contract 'field/c in)
                   (coerce-contract 'field/c out)
                   #t)]))

(define-struct field/c (in out both-supplied?)
  #:property prop:custom-write custom-write-property-proc
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:name
   (lambda (ctc)
     (apply build-compound-type-name
            `(field/c ,(field/c-in ctc)
                          ,@(if (field/c-both-supplied? ctc)
                                (list (field/c-out ctc))
                                (list)))))
   #:first-order
   (lambda (ctc)
     (let ([ctc (field/c-in ctc)])
       (lambda (f) (and (field-handle? f)
                        (ctc (f))))))
   #:late-neg-projection
   (lambda (ctc)
     (define in-proc (get/build-late-neg-projection (field/c-in ctc)))
     (define out-proc (get/build-late-neg-projection (field/c-out ctc)))
     (λ (blame)
       (define blame/c (blame-add-context blame "the field of"))
       (define in-proj (in-proc (blame-swap blame/c)))
       (define out-proj (out-proc blame/c))
       (define proj-pos (lambda (x) (out-proj x (blame-positive blame))))
       (lambda (f neg-party)
         (define proj-neg (lambda (x) (in-proj x neg-party)))
         (cond
           [(field-handle? f)
            (make-field-proxy f proj-neg proj-pos)]
           [else (raise-blame-error blame/c
                                    #:missing-party neg-party
                                    f
                                    '(expected: "a field"))]))))))

;; A FID is a (Listof UniqueNatural).
;;
;; The (unique) ID of the specific facet is the car; the parent's
;; unique ID is the cadr; and so on.

(struct actor-state (mux ;; Mux
                     facets ;; (Hash FID Facet)
                     previous-knowledge ;; AssertionSet
                     knowledge ;; AssertionSet
                     field-table ;; FieldTable
                     field-dataflow ;; DataflowGraph
                     )
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print a [p (current-output-port)])
     (pretty-print-actor-state a p))])

(struct facet (id ;; FID; this includes parent IDs etc
               endpoints ;; (Hash EID Endpoint)
               stop-scripts ;; (Listof Script) -- IN REVERSE ORDER
               children ;; (Setof FID)
               previous-knowledge ;; AssertionSet of internal knowledge
               knowledge ;; AssertionSet of internal knowledge
               ) #:prefab)

(struct endpoint (id patch-fn handler-fn) #:prefab)

(struct internal-knowledge (v) #:prefab)
(define internal-knowledge-parenthesis (open-parenthesis 1 struct:internal-knowledge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Script priorities. These are used to ensure that the results of
;; some *side effects* are visible to certain pieces of code.

(module priorities racket/base
  (require (for-syntax racket/base))

  (define-syntax (define-priority-levels stx)
    (let loop ((counter 0) (stx (syntax-case stx ()
                                  [(_ level ...) #'(level ...)])))
      (syntax-case stx ()
        [()
         #'(void)]
        [(#:count c)
         #`(begin (define c #,counter)
                  (provide c))]
        [(this-level more ...)
         #`(begin (define this-level #,counter)
                  (provide this-level)
                  #,(loop (+ counter 1) #'(more ...)))])))

  (define-priority-levels ;; highest-priority to lowest-priority
    *query-priority-high*
    *query-priority*
    *query-handler-priority*
    *normal-priority*
    *gc-priority*
    *idle-priority*
    #:count priority-count))

(require (submod "." priorities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters and Stores. Many of these are *updated* during facet execution!

;; Storeof ActorState
(define current-actor-state (make-store))

;; Parameterof FID
(define current-facet-id (make-parameter '()))

;; Storeof Patch
(define current-pending-patch (make-store))

;; Storeof (Constreeof Action)
(define current-pending-actions (make-store))

;; Storeof Patch
(define current-pending-internal-patch (make-store))

;; Storeof (Constreeof Action)
(define current-pending-internal-events (make-store))

;; Storeof (Vector (Queue Script) ...)
;; Mutates the vector!
(define current-pending-scripts (make-store))

;; Parameterof Boolean
(define in-script? (make-parameter #f))

;; Parameterof (Action -> Action)
(define current-action-transformer (make-store))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax; main entry points

(begin-for-syntax
  (define-splicing-syntax-class actor-wrapper
    (pattern (~seq #:spawn wrapper))
    (pattern (~seq) #:attr wrapper #'spawn))

  (define-splicing-syntax-class on-crash-option
    (pattern (~seq #:on-crash expr))
    (pattern (~seq) #:attr expr #f))

  (define-splicing-syntax-class let-option
    (pattern (~seq #:let clauses))
    (pattern (~seq) #:attr clauses #'()))

  (define-splicing-syntax-class when-pred
    (pattern (~seq #:when Pred))
    (pattern (~seq) #:attr Pred #'#t))

  (define-splicing-syntax-class priority
    (pattern (~seq #:priority level))
    (pattern (~seq) #:attr level #'*normal-priority*))

  (define-splicing-syntax-class field-contract
    (pattern (~seq #:contract in (~optional out #:defaults ([out #f]))))))

(define-syntax (actor-action stx)
  (syntax-parse stx
    [(_ name:name assertions:assertions script ...)
     (quasisyntax/loc stx
       (core:make-actor
        (lambda ()
          (list actor-behavior
                (boot-actor (lambda () (begin/void-default script ...)))
                name.N))
        assertions.P))]))

(define-syntax (spawn stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:name name-expr) #:defaults ([name-expr #'#f])
                        #:name "#:name")
             (~optional (~seq #:assertions assertions-expr)
                        #:name "#:assertions")
             (~optional (~seq #:assertions* assertions*-expr)
                        #:name "#:assertions*")
             (~optional (~seq #:linkage [linkage-expr ...]) #:defaults ([(linkage-expr 1) '()])
                        #:name "#:linkage"))
        ...
        O ...)
     (quasisyntax/loc stx
       (let ((spawn-action (actor-action
                            #:name name-expr
                            #:assertions*
                            #,(cond
                                [(attribute assertions-expr)
                                 (when (attribute assertions*-expr)
                                   (raise-syntax-error
                                    'spawn
                                    "Both #:assertions and #:assertions* supplied"
                                    stx))
                                 #'(pattern->trie '<initial-spawn-assertions> assertions-expr)]
                                [(attribute assertions*-expr)
                                 #'assertions*-expr]
                                [else
                                 #'trie-empty])
                            (react linkage-expr ... O ...))))
         (if (syndicate-effects-available?)
             (schedule-action! spawn-action)
             spawn-action)))]))

(define-syntax (spawn* stx)
  (syntax-parse stx
    [(_ name:name assertions:assertions script ...)
     (quasisyntax/loc stx
       (let ((spawn-action (actor-action #:name name.N #:assertions* assertions.P script ...)))
         (if (syndicate-effects-available?)
             (schedule-action! spawn-action)
             spawn-action)))]))

(define-syntax (dataspace stx)
  (syntax-parse stx
    [(_ name:name script ...)
     (quasisyntax/loc stx
       (let ((spawn-action (core:dataspace-actor #:name name.N (actor-action script ...))))
         (if (syndicate-effects-available?)
             (schedule-action! spawn-action)
             spawn-action)))]))

(define-syntax (react stx)
  (syntax-parse stx
    [(_ O ...)
     (quasisyntax/loc stx
       (add-facet! #,(source-location->string stx)
                   (lambda () (begin/void-default O ...))))]))

(define-syntax (react/suspend stx)
  (syntax-parse stx
    [(_ (resume-parent) O ...)
     (quasisyntax/loc stx
       (suspend-script* #,(source-location->string stx)
                        (lambda (resume-parent)
                          (add-facet! #,(source-location->string stx)
                                      (lambda () (begin/void-default O ...))))))]))

(define-syntax (until stx)
  (syntax-parse stx
    [(_ E O ...)
     (syntax/loc stx
       (react/suspend (continue)
                      (stop-when E (continue (void)))
                      O ...))]))

(define-syntax (forever stx)
  (syntax-parse stx
    [(_ O ...)
     (syntax/loc stx
       (react/suspend (continue) O ...))]))

(define-syntax (define-field stx)
  (syntax-parse stx
    [(_ id init)
     #'(define id (make-field 'id init))]
    [(_ id init contract:field-contract)
     (with-syntax ([ctc (if (attribute contract.out)
                            #'(field/c contract.in contract.out)
                            #'(field/c contract.in))])
       #'(define/contract id ctc (make-field 'id init)))]))

(define-syntax (field stx)
  (syntax-parse stx
    [(_ [id:id init maybe-contract ...] ...)
     (quasisyntax/loc stx
       (begin
         (when (and (in-script?) (pair? (current-facet-id)))
           (error 'field
                  "~a: Cannot declare fields in a script; are you missing a (react ...)?"
                  #,(source-location->string stx)))
         (define-field id init maybe-contract ...)
         ...))]))

(define-syntax (assert stx)
  (syntax-parse stx
    [(_ w:when-pred P)
     (define-values (proj pat bindings _instantiated)
       (analyze-pattern stx #'P))
     (quasisyntax/loc stx
       (add-endpoint! #,(source-location->string stx)
                      #f
                      (lambda ()
                        #,(let ((patch-stx #`(core:assert #,pat)))
                            (if #'w.Pred
                                #`(if w.Pred #,patch-stx patch-empty)
                                patch-stx)))
                      void))]))

(define-syntax (know stx)
  (syntax-parse stx
    [(_ w:when-pred P)
     (define-values (proj pat bindings _instantiated)
       (analyze-pattern stx #'P))
     (quasisyntax/loc stx
       (add-endpoint!
        #,(source-location->string stx)
        #t
        (lambda ()
          #,(let ((patch-stx #`(core:assert (internal-knowledge #,pat))))
              (if #'w.Pred
                  #`(if w.Pred #,patch-stx patch-empty)
                  patch-stx)))
        void))]))

(define (fid-ancestor? fid maybe-ancestor)
  (and (pair? fid) ;; empty fid lists obviously no ancestors at all!
       (or (equal? fid maybe-ancestor)
           (fid-ancestor? (cdr fid) maybe-ancestor))))

(define-syntax (stop-facet stx)
  (syntax-parse stx
    [(_ fid-expr script ...)
     (quasisyntax/loc stx
       (let ((fid fid-expr))
         (when (not (fid-ancestor? (current-facet-id) fid))
           (error 'stop-facet "Attempt to stop non-ancestor facet ~a" fid))
         (parameterize ((current-facet-id (cdr fid))) ;; run in parent context wrt terminating facet
           (schedule-script! (lambda ()
                               (terminate-facet! fid)
                               (schedule-script!
                                (lambda ()
                                  (begin/void-default script ...))))))))]))

(define-syntax-rule (stop-current-facet script ...)
  (stop-facet (current-facet-id) script ...))

(define-syntax (stop-when stx)
  (syntax-parse stx
    [(_ w:when-pred E prio:priority script ...)
     (analyze-event stx
                    #'w.Pred
                    #'E
                    (syntax/loc stx (stop-facet (current-facet-id) script ...))
                    #'prio.level)]))

(define-syntax-rule (stop-when-true condition script ...)
  (begin/dataflow
    (when condition
      (stop-facet (current-facet-id) script ...))))

(define-syntax (on-start stx)
  (syntax-parse stx
    [(_ script ...)
     (quasisyntax/loc stx
       (schedule-script! (lambda () (begin/void-default script ...))))]))

(define-syntax (on-stop stx)
  (syntax-parse stx
    [(_ script ...)
     (quasisyntax/loc stx
       (add-stop-script! (lambda () (begin/void-default script ...))))]))

(define-syntax (on-event stx)
  (syntax-parse stx
    [(_ prio:priority clause ...)
     (quasisyntax/loc stx
       (on-event* #,(source-location->string stx)
                  (lambda (e)
                    (core:match-event e
                      clause ...))
                  #:priority prio.level))]))

(define (on-event* where proc #:priority [priority *normal-priority*])
  (add-endpoint! where
                 #f
                 (lambda () patch-empty)
                 (lambda (e _current-interests _synthetic?)
                   (schedule-script! #:priority priority (lambda () (proc e))))))

(define-syntax (on stx)
  (syntax-parse stx
    [(_ w:when-pred E prio:priority script ...)
     (analyze-event stx
                    #'w.Pred
                    #'E
                    (syntax/loc stx (begin/void-default script ...))
                    #'prio.level)]))

(define-syntax (during stx)
  (syntax-parse stx
    #:literals (know)
    [(_ (~or (~and K (know P)) P) O ...)
     (define E-stx  (quasisyntax/loc #'P #,(if (attribute K)
                                               #'K
                                               #'(asserted P))))
     (define R-stx (if (attribute K) #'forget #'retracted))
     (define-values (_proj _pat _bindings instantiated)
       (analyze-pattern E-stx #'P))
     (quasisyntax/loc stx
       (on #,E-stx
           (let ((p #,instantiated))
             (react (stop-when (#,R-stx p))
                    O ...))))]))

(define-syntax (during/spawn stx)
  (syntax-parse stx
    [(_ P w:actor-wrapper name:name assertions:assertions parent-let:let-option
        oncrash:on-crash-option
        O ...)
     (define E-stx (syntax/loc #'P (asserted P)))
     (define-values (_proj _pat _bindings instantiated)
       (analyze-pattern E-stx #'P))
     (quasisyntax/loc stx
       (on #,E-stx
           (let* ((id (gensym 'during/spawn))
                  (p #,instantiated) ;; this is the concrete assertion corresponding to demand
                  (inst (instance id p))) ;; this is the assertion representing supply
             (react (stop-when (asserted inst)
                               ;; Supply (inst) appeared before demand (p) retracted.
                               ;; Transition to a state where we monitor demand, but also
                               ;; express interest in supply: this latter acts as a signal
                               ;; to the supply that it should stick around. We react to
                               ;; retraction of supply before retraction of demand by
                               ;; invoking the on-crash expression, if supplied. Once
                               ;; demand is retracted, this facet terminates, retracting
                               ;; its interest in supply, thereby signalling to the supply
                               ;; that it is no longer wanted.
                               (react (stop-when (retracted inst) ;; NOT OPTIONAL
                                                 #,@(if (attribute oncrash.expr)
                                                        #'(oncrash.expr)
                                                        #'()))
                                      (stop-when (retracted p))))
                    (stop-when (retracted p)
                               ;; Demand (p) retracted before supply (inst) appeared. We
                               ;; MUST wait for the supply to fully appear so that we can
                               ;; reliably tell it to shut down. We must maintain interest
                               ;; in supply until we see supply, and then terminate, thus
                               ;; signalling to supply that it is no longer wanted.
                               (react (stop-when (asserted inst)))))
             (let parent-let.clauses
               (w.wrapper #:linkage [(assert inst)
                                     (stop-when (retracted (observe inst)))]
                          #:name name.N
                          #:assertions* assertions.P
                          O ...)))))]))

(define-syntax (begin/dataflow stx)
  (syntax-parse stx
    [(_ prio:priority expr ...)
     (quasisyntax/loc stx
       (let ()
         (add-endpoint! #,(source-location->string stx)
                        #f
                        (lambda ()
                          (define subject-id (current-dataflow-subject-id))
                          (schedule-script!
                           #:priority prio.level
                           (lambda ()
                             (parameterize ((current-dataflow-subject-id subject-id))
                               expr ...)))
                          patch-empty)
                        void)))]))

(define-syntax (define/dataflow stx)
  (syntax-parse stx
    [(_ fieldname expr)
     (quasisyntax/loc stx (define/dataflow fieldname expr #:default #f))]
    [(_ fieldname expr #:default default-expr)
     (quasisyntax/loc stx
       (begin
         (field [fieldname default-expr])
         (begin/dataflow (fieldname expr))))]))

(define-syntax (asserted stx) (raise-syntax-error #f "asserted: Used outside event spec" stx))
(define-syntax (retracted stx) (raise-syntax-error #f "retracted: Used outside event spec" stx))
(define-syntax (rising-edge stx) (raise-syntax-error #f "rising-edge: Used outside event spec" stx))
(define-syntax (forget stx) (raise-syntax-error #f "forget: Used outside event spec" stx))
(define-syntax (realize stx) (raise-syntax-error #f "realize: Used outside event spec" stx))

(define-syntax (suspend-script stx)
  (syntax-parse stx
    [(_ proc)
     (quasisyntax/loc stx
       (suspend-script* #,(source-location->string stx) proc))]))

(define-syntax (let-event stx)
  (syntax-parse stx
    [(_ [e ...] body ...)
     (syntax/loc stx
       ((react/suspend (k)
          (on-start (-let-event [e ...] (k (lambda () body ...)))))))]))

(define-syntax (-let-event stx)
  (syntax-parse stx
    [(_ [] expr) #'expr]
    [(_ [e es ...] expr) (quasisyntax/loc #'e (react (stop-when e (-let-event [es ...] expr))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queries

(begin-for-syntax
  (define-splicing-syntax-class on-add
    (pattern (~optional (~seq #:on-add expr) #:defaults ([expr #f]))))
  (define-splicing-syntax-class on-remove
    (pattern (~optional (~seq #:on-remove expr) #:defaults ([expr #f]))))

  (define (schedule-query-handler-stxs maybe-expr-stx)
    (if maybe-expr-stx
        (quasisyntax/loc maybe-expr-stx
          ((schedule-script! #:priority *query-handler-priority*
                             (lambda () #,maybe-expr-stx))))
        #'())))

(define-syntax (query-value stx)
  (syntax-parse stx
    [(_ field-name absent-expr args ...)
     (quasisyntax/loc stx
       (let ()
         (field [field-name absent-expr])
         (query-value* field-name absent-expr args ...)))]))

(define-syntax (query-value* stx)
  (syntax-parse stx
    [(_ field-name absent-expr P expr on-add:on-add on-remove:on-remove)
     (quasisyntax/loc stx
       (let ((F field-name))
         (on (asserted P) #:priority *query-priority*
             #,@(schedule-query-handler-stxs (attribute on-add.expr))
             (F expr))
         (on (retracted P) #:priority *query-priority-high*
             #,@(schedule-query-handler-stxs (attribute on-remove.expr))
             (F absent-expr))
         F))]))

(define-syntax (query-set stx)
  (syntax-parse stx
    [(_ field-name args ...)
     (quasisyntax/loc stx
       (let ()
         (field [field-name (set)])
         (query-set* field-name args ...)))]))

(define-syntax (query-set* stx)
  (syntax-parse stx
    [(_ field-name P expr on-add:on-add on-remove:on-remove)
     (quasisyntax/loc stx
       (let ((F field-name))
         (on (asserted P) #:priority *query-priority*
             (let ((V expr))
               (when (not (set-member? (F) V))
                 #,@(schedule-query-handler-stxs (attribute on-add.expr))
                 (F (set-add (F) V)))))
         (on (retracted P) #:priority *query-priority-high*
             (let ((V expr))
               (when (set-member? (F) V)
                 #,@(schedule-query-handler-stxs (attribute on-remove.expr))
                 (F (set-remove (F) V)))))
         F))]))

(define-syntax (query-hash stx)
  (syntax-parse stx
    [(_ field-name args ...)
     (quasisyntax/loc stx
       (let ()
         (field [field-name (hash)])
         (query-hash* field-name args ...)))]))

(define-syntax (query-hash* stx)
  (syntax-parse stx
    [(_ field-name P key-expr value-expr on-add:on-add on-remove:on-remove)
     (quasisyntax/loc stx
       (let ((F field-name))
         (on (asserted P) #:priority *query-priority*
             (let ((key key-expr))
               (when (hash-has-key? (F) key)
                 (log-warning "query-hash: field ~v with pattern ~v: overwriting existing entry ~v"
                              'field-name
                              'P
                              key))
               #,@(schedule-query-handler-stxs (attribute on-add.expr))
               (F (hash-set (F) key value-expr))))
         (on (retracted P) #:priority *query-priority-high*
             #,@(schedule-query-handler-stxs (attribute on-remove.expr))
             (F (hash-remove (F) key-expr)))
         F))]))

(define-syntax (query-hash-set stx)
  (syntax-parse stx
    [(_ field-name args ...)
     (quasisyntax/loc stx
       (let ()
         (field [field-name (hash)])
         (query-hash-set* field-name args ...)))]))

(define-syntax (query-hash-set* stx)
  (syntax-parse stx
    [(_ field-name P key-expr value-expr on-add:on-add on-remove:on-remove)
     (quasisyntax/loc stx
       (let ((F field-name))
         (on (asserted P) #:priority *query-priority*
             (let ((K key-expr) (V value-expr))
               (when (not (hashset-member? (F) K V))
                 #,@(schedule-query-handler-stxs (attribute on-add.expr))
                 (F (hashset-add (F) K V)))))
         (on (retracted P) #:priority *query-priority-high*
             (let ((K key-expr) (V value-expr))
               (when (hashset-member? (F) K V)
                 #,@(schedule-query-handler-stxs (attribute on-remove.expr))
                 (F (hashset-remove (F) K V)))))
         F))]))

(define-syntax (query-count stx)
  (syntax-parse stx
    [(_ field-name args ...)
     (quasisyntax/loc stx
       (let ()
         (field [field-name (hash)])
         (query-count* field-name args ...)))]))

(define-syntax (query-count* stx)
  (syntax-parse stx
    [(_ field-name P expr on-add:on-add on-remove:on-remove)
     (quasisyntax/loc stx
       (let ((F field-name))
         (on (asserted P) #:priority *query-priority*
             (let ((E expr))
               #,@(schedule-query-handler-stxs (attribute on-add.expr))
               (F (hash-set (F) E (+ 1 (hash-ref (F) E 0))))))
         (on (retracted P) #:priority *query-priority-high*
             (let ((E expr))
               #,@(schedule-query-handler-stxs (attribute on-remove.expr))
               (let ((F0 (F)))
                 (F (match (hash-ref F0 E 0)
                      [0 F0] ;; huh
                      [1 (hash-remove F0 E)]
                      [n (hash-set F0 E (- n 1))])))))
         F))]))

(define-syntax-rule (define/query-value id ae P x ...) (define id (query-value id ae P x ...)))
(define-syntax-rule (define/query-set id P x ...) (define id (query-set id P x ...)))
(define-syntax-rule (define/query-hash id P x ...) (define id (query-hash id P x ...)))
(define-syntax-rule (define/query-hash-set id P x ...) (define id (query-hash-set id P x ...)))
(define-syntax-rule (define/query-count id P x ...) (define id (query-count id P x ...)))

(define-syntax (immediate-query stx)
  (syntax-case stx ()
    [(_ [op args ...] ...)
     (with-syntax [((query-result ...) (generate-temporaries #'(op ...)))]
       (syntax/loc stx
         (react/suspend (k)
                        (define query-result (op query-result args ...)) ...
                        (on-start (flush!) (k (query-result) ...)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require auxiliary-macro-context)

(define-auxiliary-macro-context
  #:context-name event-expander
  #:prop-name prop:event-expander
  #:prop-predicate-name event-expander?
  #:prop-accessor-name event-expander-proc
  #:macro-definer-name define-event-expander
  #:introducer-parameter-name current-event-expander-introducer
  #:local-introduce-name syntax-local-event-expander-introduce
  #:expander-id-predicate-name event-expander-id?
  #:expander-transform-name event-expander-transform)

(provide (for-syntax
          prop:event-expander
          event-expander?
          event-expander-proc
          syntax-local-event-expander-introduce
          event-expander-id?
          event-expander-transform)
         define-event-expander)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax-time support

(define (interests-pre-and-post-patch pat synthetic? retrieve-knowledge)
  (define (or* x y) (or x y))
  (define-values (prev current) (retrieve-knowledge synthetic?))
  (define old (trie-lookup prev pat #f #:wildcard-union or*))
  (define new (trie-lookup current pat #f #:wildcard-union or*))
  (values old new))

(define (interest-just-appeared-matching? pat synthetic? retrieve-knowledge)
  (define-values (old new) (interests-pre-and-post-patch pat synthetic? retrieve-knowledge))
  (and (not old) new))

(define (interest-just-disappeared-matching? pat synthetic? retrieve-knowledge)
  (define-values (old new) (interests-pre-and-post-patch pat synthetic? retrieve-knowledge))
  (and old (not new)))

;; Bool -> (Values AssertionSet AssertionSet)
;; retrieve the previous and current knowledge fields from the current actor state
(define (current-actor-state-knowledges synthetic?)
  (define a (current-actor-state))
  (define previous-knowledge (if synthetic? trie-empty (actor-state-previous-knowledge a)))
  (define current-knowledge (actor-state-knowledge a))
  (values previous-knowledge current-knowledge))

;; Bool -> (Values AssertionSet AssertionSet)
;; retrieve the previous and current knowledge fields from the current facet
(define (current-facet-knowledges synthetic?)
  (define f (lookup-facet (current-facet-id)))
  (define previous-knowledge (if synthetic? trie-empty (facet-previous-knowledge f)))
  (define current-knowledge (facet-knowledge f))
  (values previous-knowledge current-knowledge))

(define-for-syntax (analyze-appear/disappear outer-expr-stx
                                             when-pred-stx
                                             event-stx
                                             script-stx
                                             asserted?
                                             P-stx
                                             priority-stx
                                             internal?)
  (define P+
    (if internal? #`(internal-knowledge #,P-stx) P-stx))
  (define-values (proj-stx pat bindings _instantiated)
    (analyze-pattern event-stx P+))
  (define event-predicate-stx (if asserted? #'patch/added? #'patch/removed?))
  (define patch-accessor-stx (if asserted? #'patch-added #'patch-removed))
  (define change-detector-stx
    (if asserted? #'interest-just-appeared-matching? #'interest-just-disappeared-matching?))
  (define knowledge-retriever
    (if internal? #'current-facet-knowledges #'current-actor-state-knowledges))
  (quasisyntax/loc outer-expr-stx
    (add-endpoint!
     #,(source-location->string outer-expr-stx)
     #,internal?
     (lambda () (if #,when-pred-stx
                    (core:sub #,pat)
                    patch-empty))
     (lambda (e current-interests synthetic?)
       (when (not (trie-empty? current-interests))
         (core:match-event e
           [(? #,event-predicate-stx p)
            (define proj #,proj-stx)
            (define proj-arity (projection-arity proj))
            (define entry-set (trie-project/set #:take proj-arity
                                                (#,patch-accessor-stx p)
                                                proj))
            (when (not entry-set)
              (error 'asserted
                     "Wildcard interest discovered while projecting by ~v at ~a"
                     proj
                     #,(source-location->string P-stx)))
            (for [(entry (in-set entry-set))]
              (let ((instantiated (instantiate-projection proj entry)))
                (and (#,change-detector-stx instantiated synthetic? #,knowledge-retriever)
                     (schedule-script!
                      #:priority #,priority-stx
                      (lambda ()
                        (match-define (list #,@bindings) entry)
                        #,script-stx)))))]))))))

(define-for-syntax (analyze-message outer-expr-stx
                                    when-pred-stx
                                    event-stx
                                    script-stx
                                    P-stx
                                    priority-stx
                                    internal?)
  (define-values (proj pat bindings _instantiated)
       (analyze-pattern event-stx P-stx))
  (define sub
    (if internal? #`(internal-knowledge #,pat) pat))
  (define matchp
    (if internal? #'(internal-knowledge body) #'body))
  (quasisyntax/loc outer-expr-stx
    (add-endpoint!
     #,(source-location->string outer-expr-stx)
     #,internal?
     (lambda () (if #,when-pred-stx
                    (core:sub #,sub)
                    patch-empty))
     (lambda (e current-interests _synthetic?)
       (when (not (trie-empty? current-interests))
         (core:match-event e
           [(core:message #,matchp)
            (define capture-vals
              (match-value/captures
               body
               #,proj))
            (and capture-vals
                 (schedule-script!
                  #:priority #,priority-stx
                  (lambda ()
                    (apply (lambda #,bindings #,script-stx)
                           capture-vals))))]))))))

(define-for-syntax orig-insp
  (variable-reference->module-declaration-inspector (#%variable-reference)))

(define-for-syntax (analyze-event outer-expr-stx
                                  when-pred-stx
                                  armed-event-stx
                                  script-stx
                                  priority-stx)
  (define event-stx (syntax-disarm armed-event-stx orig-insp))
  (syntax-parse event-stx
    #:literals [core:message asserted retracted rising-edge know forget realize]
    [(expander args ...)
     #:when (event-expander-id? #'expander)
     (event-expander-transform
      event-stx
      (lambda (result)
        (analyze-event outer-expr-stx
                       when-pred-stx
                       (syntax-rearm result event-stx)
                       script-stx
                       priority-stx)))]
    [(core:message P)
     (analyze-message outer-expr-stx when-pred-stx event-stx script-stx
                      #'P priority-stx #f)]
    [(asserted P)
     (analyze-appear/disappear outer-expr-stx when-pred-stx event-stx script-stx
                               #t #'P priority-stx #f)]
    [(retracted P)
     (analyze-appear/disappear outer-expr-stx when-pred-stx event-stx script-stx
                               #f #'P priority-stx #f)]
    [(realize P)
     (analyze-message outer-expr-stx when-pred-stx event-stx script-stx
                      #'P priority-stx #t)]
    [(know P)
     (analyze-appear/disappear outer-expr-stx when-pred-stx event-stx script-stx
                               #t #'P priority-stx #t)]
    [(forget P)
     (analyze-appear/disappear outer-expr-stx when-pred-stx event-stx script-stx
                               #f #'P priority-stx #t)]
    [(rising-edge Pred)
     (define field-name
       (datum->syntax event-stx
                      (string->symbol
                       (format "~a:rising-edge" (source-location->string event-stx)))))
     (quasisyntax/loc outer-expr-stx
       (let ()
         (field [#,field-name #f])
         (add-endpoint! #,(source-location->string outer-expr-stx)
                        #f
                        (lambda ()
                          (when #,when-pred-stx
                            (define old-val (#,field-name))
                            (define new-val Pred)
                            (when (not (eq? old-val new-val))
                              (#,field-name new-val)
                              (when new-val
                                (schedule-script! #:priority #,priority-stx
                                                  (lambda () #,script-stx)))))
                          patch-empty)
                        void)))]))

(define-syntax (begin/void-default stx)
  (syntax-parse stx
    [(_)
     (syntax/loc stx (void))]
    [(_ expr0 expr ...)
     (syntax/loc stx (begin expr0 expr ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field Construction and Access

(define field-counter 0)
(define (make-field name initial-value)
  (define desc (field-descriptor name field-counter))
  (set! field-counter (+ field-counter 1))
  (hash-set! (actor-state-field-table (current-actor-state))
             desc
             (make-ephemeron desc initial-value))
  (field-handle desc))

(define (field-scope-error who desc)
  (error who "Field ~a used out-of-scope" (field-descriptor-name desc)))

(define (field-ref desc)
  (ephemeron-value
   (hash-ref (actor-state-field-table (current-actor-state))
             desc
             (lambda () (field-scope-error 'field-ref desc)))))

(define (field-set! desc v)
  (define a (current-actor-state))
  (define ft (actor-state-field-table a))
  (unless (hash-has-key? ft desc)
    (field-scope-error 'field-set! desc))
  (hash-set! ft desc (make-ephemeron desc v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facet Storage in an Actor

(define (facet-live? fid)
  (hash-has-key? (actor-state-facets (current-actor-state)) fid))

(define (lookup-facet fid)
  (hash-ref (actor-state-facets (current-actor-state)) fid #f))

(define (facet-live-but-inert? fid)
  (define f (lookup-facet fid))
  (and f
       (hash-empty? (facet-endpoints f))
       (set-empty? (facet-children f))))

(define (update-facet! fid proc)
  (define old-facet (lookup-facet fid))
  (define new-facet (proc old-facet))
  (store-facet! fid new-facet))

(define (store-facet! fid new-facet)
  (define a (current-actor-state))
  (current-actor-state
   (struct-copy actor-state a
                [facets (hash-set/remove (actor-state-facets a) fid new-facet)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entering and Leaving Facet Context; Queueing of Work Items

(define-syntax-rule (with-current-facet fid in? body ...)
  (parameterize ((current-facet-id fid)
                 (in-script? in?))
    body ...))

(define (capture-facet-context proc)
  (let ((fid (current-facet-id)))
    (lambda args
      (with-current-facet fid #t
        (call-with-syndicate-effects
         (lambda () (apply proc args)))))))

(define (schedule-script! #:priority [priority *normal-priority*] thunk)
  (push-script! priority (capture-facet-context thunk)))

(define (push-script! priority thunk-with-context)
  (define v (current-pending-scripts))
  (vector-set! v priority (enqueue (vector-ref v priority) thunk-with-context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Action Queue Management

(define (schedule-action! ac)
  (if (patch? ac)
      (when (patch-non-empty? ac)
        (current-pending-patch (compose-patch ac (current-pending-patch))))
      (begin (flush-pending-patch!)
             (current-pending-actions (list (current-pending-actions)
                                            ((current-action-transformer) ac))))))

(define (schedule-actions! . acs)
  (for [(ac (core:clean-actions acs))] (schedule-action! ac)))

(define (flush-pending-patch!)
  (define p (current-pending-patch))
  (when (patch-non-empty? p)
    (current-pending-patch patch-empty)
    (current-pending-actions (list (current-pending-actions)
                                   ((current-action-transformer) p)))))

(define (schedule-internal-event! ac)
  (if (patch? ac)
      (when (patch-non-empty? ac)
        (current-pending-internal-patch (compose-patch ac (current-pending-internal-patch))))
      (begin (flush-pending-internal-patch!)
             (current-pending-internal-events (list (current-pending-internal-events)
                                                     ((current-action-transformer) ac))))))

(define (flush-pending-internal-patch!)
  (define p (current-pending-internal-patch))
  (when (patch-non-empty? p)
    (current-pending-internal-patch patch-empty)
    (current-pending-internal-events (list (current-pending-internal-events)
                                            ((current-action-transformer) p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Endpoint Creation

(define (add-endpoint! where internal? patch-fn handler-fn)
  (when (in-script?)
    (error 'add-endpoint!
           "~a: Cannot add endpoint in script; are you missing a (react ...)?"
           where))
  (define-values (new-eid delta-aggregate)
    (let ()
      (define a (current-actor-state))
      (define new-eid (mux-next-pid (actor-state-mux a)))
      (define-values (new-mux _new-eid _delta delta-aggregate)
        (mux-add-stream (actor-state-mux a)
                        (parameterize ((current-dataflow-subject-id
                                        (list (current-facet-id) new-eid)))
                          (patch-fn))))
      (current-actor-state (struct-copy actor-state (current-actor-state) [mux new-mux]))
      (values new-eid delta-aggregate)))
  (update-facet! (current-facet-id)
                 (lambda (f)
                   (and f
                        (struct-copy facet f
                                     [endpoints
                                      (hash-set (facet-endpoints f)
                                                new-eid
                                                (endpoint new-eid patch-fn handler-fn))]))))
  (if internal?
      (schedule-internal-event! delta-aggregate)
      (schedule-action! delta-aggregate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facet Lifecycle

(define next-fid-uid 0)
(define (add-facet! where setup-proc)
  (when (not (in-script?))
    (error 'add-facet!
           "~a: Cannot add facet outside script; are you missing an (on ...)?"
           where))
  (define parent-fid (current-facet-id))
  (define fid-uid next-fid-uid)
  (define fid (cons fid-uid parent-fid))
  (set! next-fid-uid (+ next-fid-uid 1))
  (update-facet! fid (lambda (_f) (facet fid (hasheqv) '() (set) trie-empty trie-empty)))
  (update-facet! parent-fid
                 (lambda (pf)
                   (and pf (struct-copy facet pf
                                        [children (set-add (facet-children pf) fid)]))))
  (with-current-facet fid #f
    (setup-proc)
    (schedule-script!
     (lambda ()
       (when (and (facet-live? fid)
                  (or (and (pair? parent-fid) (not (facet-live? parent-fid)))
                      (facet-live-but-inert? fid)))
         (terminate-facet! fid)))))
  (facet-handle-event! fid
                       (lookup-facet fid)
                       (patch (actor-state-knowledge (current-actor-state)) trie-empty)
                       #t))

;; If the named facet is live, terminate it.
(define (terminate-facet! fid)
  (define f (lookup-facet fid))
  (when f
    (define parent-fid (cdr fid))

    (when (pair? parent-fid)
      (update-facet! parent-fid
                     (lambda (f)
                       (and f
                            (struct-copy facet f
                              [children (set-remove (facet-children f)
                                                    fid)])))))

    (store-facet! fid #f)

    (for [(child-fid (in-set (facet-children f)))]
      (terminate-facet! child-fid))

    ;; Run stop-scripts after terminating children. This means that
    ;; children's stop-scripts run before ours.
    (with-current-facet fid #t
      (map schedule-script! (reverse (facet-stop-scripts f))))

    (schedule-script!
     (lambda ()
       (for [((eid ep) (in-hash (facet-endpoints f)))]
         (define a (current-actor-state))
         (dataflow-forget-subject! (actor-state-field-dataflow a) (list fid eid))
         (define-values (new-mux _eid _delta delta-aggregate)
           (mux-remove-stream (actor-state-mux a) eid))
         (define internal (patch-step delta-aggregate internal-knowledge-parenthesis))
         (define external (patch (trie-subtract (patch-added delta-aggregate) (patch-added internal))
                                 (trie-subtract (patch-removed delta-aggregate) (patch-removed internal))))
         (current-actor-state (struct-copy actor-state a
                                           [mux new-mux]))
         (define internal-aggregate (patch-prepend internal-knowledge-parenthesis internal))
         (schedule-script!
          #:priority *gc-priority*
          ;; need to do this later for the forget change detector
          (lambda ()
            (define a (current-actor-state))
            (define new-knowledge
              (apply-patch (actor-state-knowledge a) internal))
            (current-actor-state (struct-copy actor-state a
                                              [knowledge new-knowledge]))))

         (schedule-internal-event! internal-aggregate)
         (schedule-action! external))))

    (schedule-script!
     #:priority *gc-priority*
     (lambda ()
       (when (facet-live-but-inert? parent-fid)
         (log-info "terminating ~v because it's dead and child ~v terminated" parent-fid fid)
         (terminate-facet! parent-fid))))))

(define (add-stop-script! script-proc)
  (update-facet! (current-facet-id)
                 (lambda (f)
                   (and f
                        (struct-copy facet f
                                     [stop-scripts (cons script-proc (facet-stop-scripts f))])))))

(define (make-empty-pending-scripts)
  (make-vector priority-count (make-queue)))

(define (boot-actor script-proc)
  (with-store [(current-actor-state
                (actor-state (mux)
                             (hash)
                             trie-empty
                             trie-empty
                             (make-weak-hasheq)
                             (make-dataflow-graph)))
               (current-pending-patch patch-empty)
               (current-pending-actions '())
               (current-pending-internal-patch patch-empty)
               (current-pending-internal-events '())
               (current-pending-scripts (make-empty-pending-scripts))
               (current-action-transformer values)]
    (with-current-facet '() #f
      (schedule-action! (core:retract ?))
      ;; Retract any initial-assertions we might have been given. We
      ;; must ensure that we explicitly maintain them: retracting them
      ;; here prevents us from accidentally relying on their
      ;; persistence from our creation.
      (schedule-script! script-proc)
      (run-scripts!))))

(define (pop-next-script!)
  (define priority-levels (current-pending-scripts))
  (let loop ((level 0))
    (and (< level (vector-length priority-levels))
         (let ((q (vector-ref priority-levels level)))
           (if (queue-empty? q)
               (loop (+ level 1))
               (let-values (((script q) (dequeue q)))
                 (vector-set! priority-levels level q)
                 script))))))

(define (run-all-pending-scripts!)
  (define script (pop-next-script!))
  (when script
    (script)
    (refresh-facet-assertions!)
    (dispatch-internal-events!)
    (run-all-pending-scripts!)))

(define (run-scripts!)
  (run-all-pending-scripts!)
  (flush-pending-patch!)
  (define pending-actions (current-pending-actions))
  (current-pending-actions '())
  (if (hash-empty? (actor-state-facets (current-actor-state)))
      (core:quit pending-actions)
      (core:transition (current-actor-state) pending-actions)))

;; dispatch the internal events that have accumulated during script execution
(define (dispatch-internal-events!)
  (flush-pending-internal-patch!)
  (define pending (flatten (current-pending-internal-events)))
  (current-pending-internal-events '())
  (define a (current-actor-state))
  (for* ([e (in-list pending)]
         [(fid f) (in-hash (actor-state-facets a))])
    (when (patch? e)
      (define a (current-actor-state))
      (current-actor-state (struct-copy actor-state a
                                        [knowledge (apply-patch (actor-state-knowledge a) e)])))
    (facet-handle-event! fid f e #f)))

(define (refresh-facet-assertions!)
  (dataflow-repair-damage! (actor-state-field-dataflow (current-actor-state))
                           (lambda (subject-id)
                             (match-define (list fid eid) subject-id)
                             (define f (lookup-facet fid))
                             (when f
                               (with-current-facet fid #f
                                 (define ep (hash-ref (facet-endpoints f) eid))
                                 (define new-patch ((endpoint-patch-fn ep)))
                                 (define a (current-actor-state))
                                 (define new-interests
                                   (trie-subtract (patch-added new-patch)
                                                  (mux-interests-of (actor-state-mux a) eid)
                                                  #:combiner (lambda (v1 v2) trie-empty)))
                                 (define newly-relevant-knowledge
                                   (biased-intersection (actor-state-knowledge a) new-interests))
                                 (update-stream! eid (compose-patch new-patch (core:retract ?)))
                                 (facet-handle-event! fid
                                                      (lookup-facet fid)
                                                      (patch newly-relevant-knowledge trie-empty)
                                                      #t))))))

(define (update-stream! eid patch)
  (define a (current-actor-state))
  (define-values (new-mux _eid _delta delta-aggregate)
    (mux-update-stream (actor-state-mux a) eid patch))
  (current-actor-state (struct-copy actor-state a [mux new-mux]))
  (schedule-action! delta-aggregate))

(define (actor-behavior e a)
  (and e
       (with-store [(current-actor-state
                     (if (patch? e)
                         (struct-copy actor-state a
                                      [previous-knowledge (actor-state-knowledge a)]
                                      [knowledge (apply-patch (actor-state-knowledge a) e)])
                         a))
                    (current-pending-patch patch-empty)
                    (current-pending-actions '())
                    (current-pending-internal-patch patch-empty)
                    (current-pending-internal-events '())
                    (current-pending-scripts (make-empty-pending-scripts))
                    (current-action-transformer values)]
         (for [((fid f) (in-hash (actor-state-facets a)))]
           (facet-handle-event! fid f e #f))
         (run-scripts!))))

(define (facet-handle-event! fid f e synthetic?)
  (define mux (actor-state-mux (current-actor-state)))
  (with-current-facet fid #f
    (when (patch? e)
      ;; quick-and-dirty intersection with (internal-knowledge ?)
      (define internal (patch-prepend internal-knowledge-parenthesis
                                      (patch-step e internal-knowledge-parenthesis)))
      (update-facet! fid
                     (lambda (f)
                       (and f
                            (struct-copy facet f
                                         [previous-knowledge (facet-knowledge f)]
                                         [knowledge (apply-patch (facet-knowledge f) internal)])))))
    (for [(ep (in-hash-values (facet-endpoints f)))]
      ((endpoint-handler-fn ep) e (mux-interests-of mux (endpoint-id ep)) synthetic?))))

(module+ implementation-details
  (provide actor-behavior
           boot-actor
           make-field
           (struct-out field-descriptor)
           (struct-out field-handle)
           (struct-out actor-state)
           (struct-out facet)
           (struct-out endpoint)

           field-ref
           field-set!

           suspend-script
           suspend-script*

           capture-actor-actions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Script suspend-and-resume.

(define prompt-tag (make-continuation-prompt-tag 'syndicate))

(define (syndicate-effects-available?)
  (continuation-prompt-available? prompt-tag))

(define (call-with-syndicate-effects thunk)
  (call-with-continuation-prompt thunk prompt-tag))

(define (capture-actor-actions thunk)
  (call-with-syndicate-effects
   (lambda ()
     (with-store [(current-pending-actions '())
                  (current-pending-patch patch-empty)
                  (current-action-transformer values)]
       (call-with-values thunk
                         (lambda results
                           (flush-pending-patch!)
                           (when (> (length results) 1)
                             (error 'capture-actor-actions
                                    "~a values supplied in top-level Syndicate action; more than one is unacceptable"
                                    (length results)))
                           (cons results (current-pending-actions))))))))

(module+ for-module-begin
  (provide capture-actor-actions))

(define (suspend-script* where proc)
  (when (not (in-script?))
    (error 'suspend-script
           "~a: Cannot suspend script outside script; are you missing an (on ...)?"
           where))
  (call-with-composable-continuation
   (lambda (k)
     (abort-current-continuation
      prompt-tag
      (lambda ()
        (define suspended-fid (current-facet-id))
        (define in? (in-script?))
        (define stale? #f)
        (define raw-resume-parent
          (capture-facet-context
           (lambda results
             (parameterize ((in-script? in?))
               (apply k results)))))
        (define resume-parent
          (lambda results
            (when stale? (error 'suspend-script
                                "Attempt to resume suspension (suspended at ~a) more than once"
                                where))
            (set! stale? #t)
            (abort-current-continuation
             prompt-tag
             (lambda ()
               (let ((invoking-fid (current-facet-id)))
                 (when (not (equal? invoking-fid suspended-fid))
                   (terminate-facet! invoking-fid)))
               (push-script! *normal-priority*
                             (lambda () (apply raw-resume-parent results)))))))
        (proc resume-parent))))
   prompt-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immediate actions

(define (ensure-in-script! who)
  (when (not (in-script?))
    (error who "Attempt to perform action outside script; are you missing an (on ...)?")))

(define (send! M)
  (ensure-in-script! 'send!)
  (schedule-action! (core:message M)))

(define (realize! M)
  (ensure-in-script! 'realize!)
  (schedule-internal-event! (core:message (internal-knowledge M))))

(define *adhoc-label* -1)

(define (assert! P)
  (ensure-in-script! 'assert!)
  (update-stream! *adhoc-label* (core:assert P)))

(define (retract! P)
  (ensure-in-script! 'retract!)
  (update-stream! *adhoc-label* (core:retract P)))

(define (patch! p)
  (ensure-in-script! 'patch!)
  (update-stream! *adhoc-label* p))

(define (perform-actions! acs)
  (ensure-in-script! 'perform-actions!)
  (for [(ac (core:clean-actions acs))]
    (match ac
      [(? patch? p) (update-stream! *adhoc-label* p)]
      [_ (schedule-action! ac)])))

(define (flush!)
  (ensure-in-script! 'flush!)
  (define ack (gensym 'flush!))
  (until (core:message ack)
         (on-start (send! ack))))

(define (quit-dataspace!)
  (ensure-in-script! 'quit-dataspace!)
  (schedule-action! (core:quit-dataspace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-field-descriptor d)
  (match-define (field-descriptor name id) d)
  (format "~a/~a" name id))

(define (pretty-print-actor-state a p)
  (match-define (actor-state mux facets _ knowledge field-table dfg) a)
  (fprintf p "ACTOR:\n")
  (fprintf p " - ")
  (display (indented-port-output 3 (lambda (p) (syndicate-pretty-print mux p)) #:first-line? #f) p)
  (newline p)
  (fprintf p " - Knowledge:\n   ~a" (trie->pretty-string knowledge #:indent 3))
  (fprintf p " - Facets:\n")
  (for ([(fid f) (in-hash facets)])
    (match-define (facet _fid endpoints _ children _ _) f)
    (fprintf p "    ---- facet ~a, children=~a" fid (set->list children))
    (when (not (hash-empty? endpoints))
      (fprintf p ", endpoints=~a" (hash-keys endpoints)))
    (newline p))
  (when (not (hash-empty? field-table))
    (fprintf p " - Fields:\n")
    (for ([(d ve) (in-hash field-table)])
      (define subject-ids (hash-ref (dataflow-graph-edges-forward dfg) d set))
      (define v (ephemeron-value ve))
      (define v*
        (indented-port-output 6 (lambda (p) (syndicate-pretty-print v p)) #:first-line? #f))
      (if (set-empty? subject-ids)
          (fprintf p "    - ~a: ~a\n" (format-field-descriptor d) v*)
          (fprintf p "    - ~a: ~a ~a\n"
                   (format-field-descriptor d)
                   (for/list [(subject-id subject-ids)]
                     (match-define (list fid eid) subject-id)
                     (format "~a:~a" fid eid))
                   v*)))))
