#lang racket/base

(provide actor
         actor*
         dataspace

         react
         react/suspend
         until
         forever

         field
         field/c
         assert
         stop-when
         on-start
         on-stop
         on-event
         on-event*
         on
         during
         during/actor
         begin/dataflow
         define/dataflow

         asserted
         retracted
         rising-edge
         (rename-out [core:message message])

         suspend-script
         let-event

         query-value
         query-set
         query-hash
         query-hash-set
         query-value*
         query-set*
         query-hash*
         query-hash-set*
         define/query-value
         define/query-set
         define/query-hash
         define/query-hash-set

         send!
         assert!
         retract!
         patch!
         flush!

         syndicate-effects-available?
         suspend-script*

         ? ;; from pattern.rkt

         ;;

         schedule-action!

         (struct-out field-descriptor)
         (struct-out field-handle)
         (struct-out actor-state)
         (struct-out facet)
         (struct-out endpoint)

         pretty-print-actor-state
         )

(module reader syntax/module-reader
  syndicate/actor-lang)

(require racket/set)
(require racket/match)
(require racket/contract)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax syntax/srcloc))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions and Structures

;; A FieldTable is a (FieldDescriptor |-> Any)

;; (field-descriptor Symbol UniqueNatural (Option FID))
(struct field-descriptor (name id defining-fid) #:prefab)

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

(struct facet (field-descriptors ;; (Setof FieldDescriptor)
               endpoints ;; (Hash EID Endpoint)
               stop-scripts ;; (Listof Script) -- IN REVERSE ORDER
               children ;; (Setof FID)
               parent ;; (Option FID)
               ) #:prefab)

(struct endpoint (id patch-fn handler-fn) #:prefab)

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
    *idle-priority*
    *gc-priority*
    #:count priority-count))

(require (submod "." priorities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters and Stores. Many of these are *updated* during facet execution!

;; Parameterof (Setof FieldDescriptor)
(define current-field-descriptors (make-parameter 'unset:current-field-descriptors))

;; Storeof ActorState
(define current-actor-state (make-store))

;; Parameterof FID
(define current-facet-id (make-parameter #f))

;; Storeof Patch
(define current-pending-patch (make-store))

;; Storeof (Constreeof Action)
(define current-pending-actions (make-store))

;; Storeof (Vector (Queue Script) ...)
;; Mutates the vector!
(define current-pending-scripts (make-store))

;; Parameterof Boolean
(define in-script? (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax; main entry points

(begin-for-syntax
  (define-splicing-syntax-class actor-wrapper
    (pattern (~seq #:actor wrapper))
    (pattern (~seq) #:attr wrapper #'actor))

  (define-splicing-syntax-class name
    (pattern (~seq #:name N))
    (pattern (~seq) #:attr N #'#f))

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
    [(_ name:name script ...)
     (quasisyntax/loc stx
       (core:make-spawn
        (lambda ()
          (list actor-behavior
                (boot-actor (lambda () (begin/void-default script ...)))
                name.N))))]))

(define-syntax (actor stx)
  (syntax-parse stx
    [(_ name:name O ...)
     (quasisyntax/loc stx
       (let ((spawn-action (actor-action #:name name.N (react O ...))))
         (if (syndicate-effects-available?)
             (schedule-action! spawn-action)
             spawn-action)))]))

(define-syntax (actor* stx)
  (syntax-parse stx
    [(_ name:name script ...)
     (quasisyntax/loc stx
       (let ((spawn-action (actor-action #:name name.N script ...)))
         (if (syndicate-effects-available?)
             (schedule-action! spawn-action)
             spawn-action)))]))

(define-syntax (dataspace stx)
  (syntax-parse stx
    [(_ name:name script ...)
     (quasisyntax/loc stx
       (let ((spawn-action (core:spawn-dataspace
                            #:name name.N
                            (actor-action script ...
                                          (schedule-action! (core:quit-dataspace))))))
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
         (when (and (in-script?) (current-facet-id))
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
                      (lambda ()
                        #,(let ((patch-stx #`(core:assert #,pat)))
                            (if #'w.Pred
                                #`(if w.Pred #,patch-stx patch-empty)
                                patch-stx)))
                      void))]))

(define-syntax (stop-when stx)
  (syntax-parse stx
    [(_ w:when-pred E prio:priority script ...)
     (analyze-event stx
                    #'w.Pred
                    #'E
                    #t
                    (syntax/loc stx (begin/void-default script ...))
                    #'prio.level)]))

(define-syntax (on-start stx)
  (syntax-parse stx
    [(_ script ...)
     (quasisyntax/loc stx
       (schedule-script! #f (lambda () (begin/void-default script ...))))]))

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
                 (lambda () patch-empty)
                 (lambda (e _synthetic?)
                   (schedule-script! #:priority priority #f (lambda () (proc e))))))

(define-syntax (on stx)
  (syntax-parse stx
    [(_ w:when-pred E prio:priority script ...)
     (analyze-event stx
                    #'w.Pred
                    #'E
                    #f
                    (syntax/loc stx (begin/void-default script ...))
                    #'prio.level)]))

(define-syntax (during stx)
  (syntax-parse stx
    [(_ P O ...)
     (define E-stx (syntax/loc #'P (asserted P)))
     (define-values (_proj _pat _bindings instantiated)
       (analyze-pattern E-stx #'P))
     (quasisyntax/loc stx
       (on #,E-stx
           (let ((p #,instantiated))
             (react (stop-when (retracted p))
                    O ...))))]))

(define-syntax (during/actor stx)
  (syntax-parse stx
    [(_ P w:actor-wrapper name:name O ...)
     (define E-stx (syntax/loc #'P (asserted P)))
     (define-values (_proj _pat _bindings instantiated)
       (analyze-pattern E-stx #'P))
     (quasisyntax/loc stx
       (on #,E-stx
           (let ((p #,instantiated))
             (w.wrapper #:name name.N
              (stop-when (retracted p))
              O ...))))]))

(define-syntax (begin/dataflow stx)
  (syntax-parse stx
    [(_ prio:priority expr ...)
     (quasisyntax/loc stx
       (let ()
         (add-endpoint! #,(source-location->string stx)
                        (lambda ()
                          (define subject-id (current-dataflow-subject-id))
                          (schedule-script!
                           #:priority prio.level
                           #f
                           (lambda ()
                             (parameterize ((current-dataflow-subject-id subject-id))
                               expr ...)))
                          patch-empty)
                        void)))]))

(define-syntax (define/dataflow stx)
  (syntax-parse stx
    [(_ fieldname expr)
     (quasisyntax/loc stx
       (begin
         (field [fieldname #f])
         (begin/dataflow (fieldname expr))))]))

(define-syntax (asserted stx) (raise-syntax-error #f "asserted: Used outside event spec" stx))
(define-syntax (retracted stx) (raise-syntax-error #f "retracted: Used outside event spec" stx))
(define-syntax (rising-edge stx) (raise-syntax-error #f "rising-edge: Used outside event spec" stx))

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
                             #f
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
       (let ()
         (on (asserted P) #:priority *query-priority*
             #,@(schedule-query-handler-stxs (attribute on-add.expr))
             (field-name expr))
         (on (retracted P) #:priority *query-priority-high*
             #,@(schedule-query-handler-stxs (attribute on-remove.expr))
             (field-name absent-expr))
         field-name))]))

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
       (let ()
         (on (asserted P) #:priority *query-priority*
             #,@(schedule-query-handler-stxs (attribute on-add.expr))
             (field-name (set-add (field-name) expr)))
         (on (retracted P) #:priority *query-priority-high*
             #,@(schedule-query-handler-stxs (attribute on-remove.expr))
             (field-name (set-remove (field-name) expr)))
         field-name))]))

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
       (let ()
         (on (asserted P) #:priority *query-priority*
             (let ((key key-expr))
               (when (hash-has-key? (field-name) key)
                 (log-warning "query-hash: field ~v with pattern ~v: overwriting existing entry ~v"
                              'field-name
                              'P
                              key))
               #,@(schedule-query-handler-stxs (attribute on-add.expr))
               (field-name (hash-set (field-name) key value-expr))))
         (on (retracted P) #:priority *query-priority-high*
             #,@(schedule-query-handler-stxs (attribute on-remove.expr))
             (field-name (hash-remove (field-name) key-expr)))
         field-name))]))

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
       (let ()
         (on (asserted P) #:priority *query-priority*
             #,@(schedule-query-handler-stxs (attribute on-add.expr))
             (field-name (hashset-add (field-name) key-expr value-expr)))
         (on (retracted P) #:priority *query-priority-high*
             #,@(schedule-query-handler-stxs (attribute on-remove.expr))
             (field-name (hashset-remove (field-name) key-expr value-expr)))
         field-name))]))

(define-syntax-rule (define/query-value id ae P x ...) (define id (query-value id ae P x ...)))
(define-syntax-rule (define/query-set id P x ...) (define id (query-set id P x ...)))
(define-syntax-rule (define/query-hash id P x ...) (define id (query-hash id P x ...)))
(define-syntax-rule (define/query-hash-set id P x ...) (define id (query-hash-set id P x ...)))

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

(define (interests-pre-and-post-patch pat synthetic?)
  (define (or* x y) (or x y))
  (define a (current-actor-state))
  (define previous-knowledge (if synthetic? trie-empty (actor-state-previous-knowledge a)))
  (define old (trie-lookup previous-knowledge pat #f #:wildcard-union or*))
  (define new (trie-lookup (actor-state-knowledge a) pat #f #:wildcard-union or*))
  (values old new))

(define (interest-just-appeared-matching? pat synthetic?)
  (define-values (old new) (interests-pre-and-post-patch pat synthetic?))
  (and (not old) new))

(define (interest-just-disappeared-matching? pat synthetic?)
  (define-values (old new) (interests-pre-and-post-patch pat synthetic?))
  (and old (not new)))

(define-for-syntax (analyze-asserted/retracted outer-expr-stx
                                               when-pred-stx
                                               event-stx
                                               terminal?
                                               script-stx
                                               asserted?
                                               P-stx
                                               priority-stx)
  (define-values (proj-stx pat bindings _instantiated)
    (analyze-pattern event-stx P-stx))
  (define event-predicate-stx (if asserted? #'patch/added? #'patch/removed?))
  (define patch-accessor-stx (if asserted? #'patch-added #'patch-removed))
  (define change-detector-stx
    (if asserted? #'interest-just-appeared-matching? #'interest-just-disappeared-matching?))
  (quasisyntax/loc outer-expr-stx
    (add-endpoint! #,(source-location->string outer-expr-stx)
                   (lambda () (if #,when-pred-stx
                                  (core:sub #,pat)
                                  patch-empty))
                   (lambda (e synthetic?)
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
                        #,(let ((entry-handler-stx
                                 (quasisyntax/loc script-stx
                                   (let ((instantiated (instantiate-projection proj entry)))
                                     (and (#,change-detector-stx instantiated synthetic?)
                                          (schedule-script!
                                           #:priority #,priority-stx
                                           #,(if terminal? #'#t #'#f)
                                           (lambda ()
                                             (match-define (list #,@bindings) entry)
                                             #,script-stx)))))))
                            (if terminal?
                                #`(let ((entry-count (set-count entry-set)))
                                    (cond
                                      [(zero? entry-count)]
                                      [(= entry-count 1)
                                       (let ((entry (set-first entry-set)))
                                         #,entry-handler-stx)]
                                      [else
                                       (error 'asserted
                                              "Multiple assertions triggered stop-when at ~a"
                                              #,(source-location->string P-stx))]))
                                #`(for [(entry (in-set entry-set))]
                                    #,entry-handler-stx)))])))))

(define-for-syntax orig-insp
  (variable-reference->module-declaration-inspector (#%variable-reference)))

(define-for-syntax (analyze-event outer-expr-stx
                                  when-pred-stx
                                  armed-event-stx
                                  terminal?
                                  script-stx
                                  priority-stx)
  (define event-stx (syntax-disarm armed-event-stx orig-insp))
  (syntax-parse event-stx
    #:literals [core:message asserted retracted rising-edge]
    [(expander args ...)
     #:when (event-expander-id? #'expander)
     (event-expander-transform
      event-stx
      (lambda (result)
        (analyze-event outer-expr-stx
                       when-pred-stx
                       (syntax-rearm result event-stx)
                       terminal?
                       script-stx
                       priority-stx)))]
    [(core:message P)
     (define-values (proj pat bindings _instantiated)
       (analyze-pattern event-stx #'P))
     (quasisyntax/loc outer-expr-stx
       (add-endpoint! #,(source-location->string outer-expr-stx)
                      (lambda () (if #,when-pred-stx
                                     (core:sub #,pat)
                                     patch-empty))
                      (lambda (e _synthetic?)
                        (core:match-event e
                          [(core:message body)
                           (define capture-vals
                             (match-value/captures
                              body
                              #,proj))
                           (and capture-vals
                                (schedule-script!
                                 #:priority #,priority-stx
                                 #,(if terminal? #'#t #'#f)
                                 (lambda ()
                                   (apply (lambda #,bindings #,script-stx)
                                          capture-vals))))]))))]
    [(asserted P)
     (analyze-asserted/retracted outer-expr-stx when-pred-stx event-stx terminal? script-stx
                                 #t #'P priority-stx)]
    [(retracted P)
     (analyze-asserted/retracted outer-expr-stx when-pred-stx event-stx terminal? script-stx
                                 #f #'P priority-stx)]
    [(rising-edge Pred)
     (define field-name
       (datum->syntax event-stx
                      (string->symbol
                       (format "~a:rising-edge" (source-location->string event-stx)))))
     (quasisyntax/loc outer-expr-stx
       (let ()
         (field [#,field-name #f])
         (add-endpoint! #,(source-location->string outer-expr-stx)
                        (lambda ()
                          (when #,when-pred-stx
                            (define old-val (#,field-name))
                            (define new-val Pred)
                            (when (not (eq? old-val new-val))
                              (#,field-name new-val)
                              (when new-val
                                (schedule-script! #:priority #,priority-stx
                                                  #,(if terminal? #'#t #'#f)
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
  (define fid (current-facet-id))
  (define desc (field-descriptor name field-counter fid))
  (set! field-counter (+ field-counter 1))
  (let ((a (current-actor-state)))
    (current-actor-state
     (struct-copy actor-state a
                  [field-table (hash-set (actor-state-field-table a) desc initial-value)])))
  (when fid (current-field-descriptors (set-add (current-field-descriptors) desc)))
  (field-handle desc))

(define (field-accessible? desc)
  (and (or (not (field-descriptor-defining-fid desc))
           (set-member? (current-field-descriptors) desc))
       (hash-has-key? (actor-state-field-table (current-actor-state)) desc)))

(define (ensure-field-accessible! who desc)
  (when (not (field-accessible? desc))
    (error who "Field ~a used out-of-scope" (field-descriptor-name desc))))

(define (field-ref desc)
  (ensure-field-accessible! 'field-ref desc)
  (hash-ref (actor-state-field-table (current-actor-state)) desc))

(define (field-set! desc v)
  (ensure-field-accessible! 'field-set! desc)
  (define a (current-actor-state))
  (define ft (actor-state-field-table a))
  (current-actor-state
   (struct-copy actor-state a [field-table (hash-set ft desc v)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facet Storage in an Actor

(define (facet-live? fid)
  (hash-has-key? (actor-state-facets (current-actor-state)) fid))

(define (lookup-facet fid)
  (hash-ref (actor-state-facets (current-actor-state)) fid #f))

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

(define-syntax-rule (with-current-facet fid cfds in? body ...)
  (parameterize ((current-facet-id fid)
                 (current-field-descriptors cfds)
                 (in-script? in?))
    body ...))

(define (capture-facet-context proc)
  (let ((fid (current-facet-id))
        (cfds (current-field-descriptors)))
    (lambda args
      (with-current-facet fid cfds #t
        (call-with-syndicate-effects
         (lambda () (apply proc args)))))))

(define (schedule-script! #:priority [priority *normal-priority*] terminal? thunk)
  (if terminal?
      (let ((f (terminate-facet! (current-facet-id))))
        (when f ;; only want to run a terminal script if we genuinely terminated
          (push-script! priority
                        (parameterize ((current-facet-id (facet-parent f)))
                          (capture-facet-context thunk)))))
      (push-script! priority (capture-facet-context thunk))))

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
             (current-pending-actions (list (current-pending-actions) ac)))))

(define (flush-pending-patch!)
  (define p (current-pending-patch))
  (when (patch-non-empty? p)
    (current-pending-patch patch-empty)
    (current-pending-actions (list (current-pending-actions) p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Endpoint Creation

(define (add-endpoint! where patch-fn handler-fn)
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
                        (parameterize ((current-dataflow-subject-id (list (current-facet-id) new-eid)))
                          (patch-fn))))
      (current-actor-state (struct-copy actor-state a [mux new-mux]))
      (values new-eid delta-aggregate)))
  (update-facet! (current-facet-id)
                 (lambda (f)
                   (and f
                        (struct-copy facet f
                                     [endpoints
                                      (hash-set (facet-endpoints f)
                                                new-eid
                                                (endpoint new-eid patch-fn handler-fn))]))))
  (schedule-action! delta-aggregate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facet Lifecycle

(define next-fid 0)
(define (add-facet! where setup-proc)
  (when (not (in-script?))
    (error 'add-facet!
           "~a: Cannot add facet outside script; are you missing an (on ...)?"
           where))
  (define parent-fid (current-facet-id))
  (define fid next-fid)
  (set! next-fid (+ next-fid 1))
  (define starting-field-descriptors
    (if parent-fid
        (cond [(lookup-facet parent-fid) => facet-field-descriptors]
              [else (current-field-descriptors)]) ;; TODO: Is this correct???
        (set)))
  (update-facet! fid (lambda (_f) (facet 'not-yet-ready
                                         (hasheqv)
                                         '()
                                         (seteqv)
                                         parent-fid)))
  (update-facet! parent-fid
                 (lambda (pf)
                   (and pf (struct-copy facet pf
                                        [children (set-add (facet-children pf) fid)]))))
  (with-current-facet fid starting-field-descriptors #f
    (setup-proc)
    (update-facet! fid (lambda (f)
                         (and f (struct-copy facet f
                                             [field-descriptors (current-field-descriptors)])))))
  (facet-handle-event! fid
                       (lookup-facet fid)
                       (patch (actor-state-knowledge (current-actor-state)) trie-empty)
                       #t)
  (when (and (facet-live? fid) parent-fid (not (facet-live? parent-fid)))
    (terminate-facet! fid)))

;; If the named facet is live, terminate it and return its facet
;; record; otherwise, return #f.
(define (terminate-facet! fid)
  (define f (lookup-facet fid))
  (and f
       (begin
         (for [((eid ep) (in-hash (facet-endpoints f)))]
           (define a (current-actor-state))
           (dataflow-forget-subject! (actor-state-field-dataflow a) (list fid eid))
           (define-values (new-mux _eid _delta delta-aggregate)
             (mux-remove-stream (actor-state-mux a) eid))
           (current-actor-state (struct-copy actor-state a [mux new-mux]))
           (schedule-action! delta-aggregate))

         (let ((parent-fid (facet-parent f)))
           (when parent-fid
             (update-facet! parent-fid
                            (lambda (f)
                              (and f
                                   (struct-copy facet f
                                                [children (set-remove (facet-children f)
                                                                      fid)]))))))
         (store-facet! fid #f)

         (for [(child-fid (in-set (facet-children f)))]
           (terminate-facet! child-fid))

         ;; Run stop-scripts after terminating children. This means that
         ;; children's stop-scripts run before ours.
         (with-current-facet fid (facet-field-descriptors f) #t
           (for [(script (in-list (reverse (facet-stop-scripts f))))]
             (call-with-syndicate-effects script)))

         (push-script! *gc-priority*
                       (lambda ()
                         (let* ((a (current-actor-state))
                                (new-table
                                 (for/fold [(t (actor-state-field-table a))]
                                           [(desc (in-set (facet-field-descriptors f)))
                                            #:when (equal? (field-descriptor-defining-fid desc)
                                                           fid)]
                                   (hash-remove t desc))))
                           (current-actor-state
                            (struct-copy actor-state a [field-table new-table])))))

         f)))

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
                             (hasheqv)
                             trie-empty
                             trie-empty
                             (hash)
                             (make-dataflow-graph)))
               (current-pending-patch patch-empty)
               (current-pending-actions '())
               (current-pending-scripts (make-empty-pending-scripts))]
    (with-current-facet #f (set) #f
      (schedule-script! #f script-proc)
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
    (run-all-pending-scripts!)))

(define (run-scripts!)
  (run-all-pending-scripts!)
  (flush-pending-patch!)
  (define pending-actions (current-pending-actions))
  (current-pending-actions '())
  (if (hash-empty? (actor-state-facets (current-actor-state)))
      (core:quit pending-actions)
      (core:transition (current-actor-state) pending-actions)))

(define (refresh-facet-assertions!)
  (dataflow-repair-damage! (actor-state-field-dataflow (current-actor-state))
                           (lambda (subject-id)
                             (match-define (list fid eid) subject-id)
                             (define f (lookup-facet fid))
                             (when f
                               (with-current-facet fid (facet-field-descriptors f) #f
                                 (define ep (hash-ref (facet-endpoints f) eid))
                                 (define new-patch ((endpoint-patch-fn ep)))
                                 (update-stream! eid (compose-patch new-patch
                                                                    (core:retract ?))))))))

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
                                      [knowledge (update-interests (actor-state-knowledge a) e)])
                         a))
                    (current-pending-patch patch-empty)
                    (current-pending-actions '())
                    (current-pending-scripts (make-empty-pending-scripts))]
         (for [((fid f) (in-hash (actor-state-facets a)))]
           (facet-handle-event! fid f e #f))
         (run-scripts!))))

(define (facet-handle-event! fid f e synthetic?)
  (with-current-facet fid (facet-field-descriptors f) #f
    (for [(ep (in-hash-values (facet-endpoints f)))]
      ((endpoint-handler-fn ep) e synthetic?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Script suspend-and-resume.

(define prompt-tag (make-continuation-prompt-tag 'syndicate))

(define (syndicate-effects-available?)
  (continuation-prompt-available? prompt-tag))

(define (call-with-syndicate-effects thunk)
  (call-with-continuation-prompt thunk prompt-tag))

(module+ for-module-begin
  (provide call-with-syndicate-effects
           flush-pending-patch!
           current-pending-actions
           current-pending-patch))

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

(define (flush!)
  (ensure-in-script! 'flush!)
  (define ack (gensym 'flush!))
  (until (core:message ack)
         (on-start (send! ack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-field-descriptor d)
  (match-define (field-descriptor name id defining-fid) d)
  (if defining-fid
      (format "~a/~a(~a)" name id defining-fid)
      (format "~a/~a" name id)))

(define (pretty-print-actor-state a p)
  (match-define (actor-state mux facets _ knowledge field-table dfg) a)
  (fprintf p "ACTOR:\n")
  (fprintf p " - ")
  (display (indented-port-output 3 (lambda (p) (syndicate-pretty-print mux p)) #:first-line? #f) p)
  (newline p)
  (fprintf p " - Knowledge:\n   ~a" (trie->pretty-string knowledge #:indent 3))
  (fprintf p " - Facets:\n")
  (for ([(fid f) (in-hash facets)])
    (match-define (facet descs endpoints _ children parent) f)
    (fprintf p "    ---- facet ~a, parent=~a, children=~a" fid parent (set->list children))
    (when (not (hash-empty? endpoints))
      (fprintf p ", endpoints=~a" (hash-keys endpoints)))
    (newline p)
    (when (not (set-empty? descs))
      (fprintf p "         field descriptors: ~a\n"
               (for/list [(d descs)] (format-field-descriptor d)))))
  (when (not (hash-empty? field-table))
    (fprintf p " - Fields:\n")
    (for ([(d v) (in-hash field-table)])
      (define subject-ids (hash-ref (dataflow-graph-edges-forward dfg) d set))
      (if (set-empty? subject-ids)
          (fprintf p "    - ~a: ~v\n" (format-field-descriptor d) v)
          (fprintf p "    - ~a: ~v ~a\n"
                   (format-field-descriptor d)
                   v
                   (for/list [(subject-id subject-ids)]
                     (match-define (list fid eid) subject-id)
                     (format "~a:~a" fid eid)))))))
