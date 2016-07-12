#lang racket/base

(provide actor
         dataspace

         react
         react/independent
         react/suspend
         until
         forever

         field
         assert
         stop-when
         on-start
         on-stop
         on-event
         on-event*
         on
         during
         during/actor

         asserted
         retracted
         rising-edge
         (rename-out [core:message message])

         suspend-script

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
         )

(module reader syntax/module-reader
  syndicate/actor-lang)

(require racket/set)
(require racket/match)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax syntax/srcloc))

(require (prefix-in core: "core.rkt"))
(require "mux.rkt")
(require "patch.rkt")
(require "trie.rkt")
(require "pattern.rkt")
(require "dataflow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions and Structures

;; A FieldTable is a (FieldDescriptor |-> Boxof Any)

;; (field-descriptor Symbol UniqueNatural)
(struct field-descriptor (name id) #:prefab)

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
     (unbox (get-field-box desc))]
    [(handle v)
     (define desc (field-handle-desc handle))
     (dataflow-record-damage! (actor-state-field-dataflow (current-actor-state)) desc)
     (set-box! (get-field-box desc) v)]))

(struct actor-state (mux ;; Mux
                     facets ;; (Hash FID Facet)
                     previous-knowledge ;; AssertionSet
                     knowledge ;; AssertionSet
                     field-table ;; FieldTable
                     field-dataflow ;; DataflowGraph
                     ) #:prefab)

(struct facet (field-table ;; FieldTable
               endpoints ;; (Hash EID Endpoint)
               stop-scripts ;; (Listof Script) -- IN REVERSE ORDER
               children ;; (Setof FID)
               parent ;; (Option FID)
               ) #:prefab)

(struct endpoint (id patch-fn handler-fn) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters. Many of these are *updated* during facet execution!

;; Parameterof FieldTable
(define current-field-table (make-parameter 'unset:current-field-table))

;; Parameterof ActorState
(define current-actor-state (make-parameter #f))

;; Parameterof FID
(define current-facet-id (make-parameter #f))

;; Parameterof Patch
(define current-pending-patch (make-parameter patch-empty))

;; Parameterof (Constreeof Action)
(define current-pending-actions (make-parameter '()))

(define (make-empty-pending-scripts)
  (vector '() '()))

;; Parameterof (Vector (List Script) (List Script))
(define current-pending-scripts (make-parameter (make-empty-pending-scripts)))

;; Parameterof Boolean
(define in-script? (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax; main entry points

(begin-for-syntax
  (define-splicing-syntax-class name
    (pattern (~seq #:name N))
    (pattern (~seq) #:attr N #'#f))

  (define-splicing-syntax-class when-pred
    (pattern (~seq #:when Pred))
    (pattern (~seq) #:attr Pred #'#t))

  (define-splicing-syntax-class meta-level
    (pattern (~seq #:meta-level level:integer))
    (pattern (~seq) #:attr level #'0)))

(define-syntax (actor stx)
  (syntax-parse stx
    [(_ name:name script ...)
     (quasisyntax/loc stx
       (let* ((spawn-action (core:<spawn>
                             (lambda ()
                               (list actor-behavior
                                     (boot-actor (lambda () (begin/void-default script ...)))
                                     name.N)))))
         (if (syndicate-effects-available?)
             (schedule-action! spawn-action)
             spawn-action)))]))

(define-syntax (dataspace stx)
  (syntax-parse stx
    [(_ name:name script ...)
     (quasisyntax/loc stx
       (core:spawn-dataspace #:name name.N
                             (actor script ...
                                    (schedule-action! (core:quit-dataspace)))))]))

(define-syntax (react stx)
  (syntax-parse stx
    [(_ O ...)
     (quasisyntax/loc stx
       (add-facet! #:substate #t
                   #,(source-location->string stx)
                   (lambda () (begin/void-default O ...))))]))

(define-syntax (react/independent stx)
  (syntax-parse stx
    [(_ O ...)
     (quasisyntax/loc stx
       (add-facet! #:substate #f
                   #,(source-location->string stx)
                   (lambda () (begin/void-default O ...))))]))

(define-syntax (react/suspend stx)
  (syntax-parse stx
    [(_ (resume-parent) O ...)
     (quasisyntax/loc stx
       (suspend-script* #,(source-location->string stx)
                        (lambda (resume-parent)
                          (add-facet! #:substate #t
                                      #,(source-location->string stx)
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

(define-syntax (field stx)
  (syntax-parse stx
    [(_ [id:id init] ...)
     (quasisyntax/loc stx
       (begin
         (when (and (in-script?) (current-facet-id))
           (error 'field
                  "~a: Cannot declare fields in a script; are you missing a (react ...)?"
                  #,(source-location->string stx)))
         (define id (make-field 'id init))
         ...))]))

(define-syntax (assert stx)
  (syntax-parse stx
    [(_ w:when-pred P L:meta-level)
     (define-values (proj pat bindings _instantiated)
       (analyze-pattern stx #'P))
     (quasisyntax/loc stx
       (add-endpoint! #,(source-location->string stx)
                      (lambda ()
                        #,(let ((patch-stx #`(core:assert #,pat #:meta-level L.level)))
                            (if #'w.Pred
                                #`(if w.Pred #,patch-stx patch-empty)
                                patch-stx)))
                      void))]))

(define-syntax (stop-when stx)
  (syntax-parse stx
    [(_ E script ...)
     (analyze-event stx #'E #t (syntax/loc stx (begin/void-default script ...)))]))

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
    [(_ clause ...)
     (quasisyntax/loc stx
       (on-event* #,(source-location->string stx)
                  (lambda (e)
                    (core:match-event e
                      clause ...))))]))

(define (on-event* where proc #:priority [priority 0])
  (add-endpoint! where
                 (lambda () patch-empty)
                 (lambda (e)
                   (schedule-script! #:priority priority #f (lambda () (proc e))))))

(define-syntax (on stx)
  (syntax-parse stx
    [(_ E script ...)
     (analyze-event stx #'E #f (syntax/loc stx (begin/void-default script ...)))]))

(define-syntax (during stx)
  (syntax-parse stx
    [(_ P L:meta-level O ...)
     (define E-stx (syntax/loc #'P (asserted P #:meta-level L.level)))
     (define-values (_proj _pat _bindings instantiated)
       (analyze-pattern E-stx #'P))
     (quasisyntax/loc stx
       (on #,E-stx
           (let ((p #,instantiated))
             (react (stop-when (retracted p #:meta-level L.level))
                    O ...))))]))

(define-syntax (during/actor stx)
  (syntax-parse stx
    [(_ P L:meta-level O ...)
     (define E-stx (syntax/loc #'P (asserted P #:meta-level L.level)))
     (define-values (_proj _pat _bindings instantiated)
       (analyze-pattern E-stx #'P))
     (quasisyntax/loc stx
       (on #,E-stx
           (let ((p #,instantiated))
             (actor
              (react (stop-when (retracted p #:meta-level L.level))
                     O ...)))))]))

(define-syntax (asserted stx) (raise-syntax-error #f "asserted: Used outside event spec" stx))
(define-syntax (retracted stx) (raise-syntax-error #f "retracted: Used outside event spec" stx))
(define-syntax (rising-edge stx) (raise-syntax-error #f "rising-edge: Used outside event spec" stx))

(define-syntax (suspend-script stx)
  (syntax-parse stx
    [(_ proc)
     (quasisyntax/loc stx
       (suspend-script* #,(source-location->string stx) proc))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax-time support

(define (interests-pre-and-post-patch pat)
  (define (or* x y) (or x y))
  (define a (current-actor-state))
  (define old (trie-lookup (actor-state-previous-knowledge a) pat #f #:wildcard-union or*))
  (define new (trie-lookup (actor-state-knowledge a) pat #f #:wildcard-union or*))
  (values old new))

(define (interest-just-appeared-matching? pat)
  (define-values (old new) (interests-pre-and-post-patch pat))
  (and (not old) new))

(define (interest-just-disappeared-matching? pat)
  (define-values (old new) (interests-pre-and-post-patch pat))
  (and old (not new)))

(define-for-syntax (analyze-asserted/retracted outer-expr-stx
                                               event-stx
                                               terminal?
                                               script-stx
                                               asserted?
                                               P-stx
                                               meta-level)
  (define-values (proj-stx pat bindings _instantiated)
    (analyze-pattern event-stx P-stx))
  (define event-predicate-stx (if asserted? #'patch/added? #'patch/removed?))
  (define patch-accessor-stx (if asserted? #'patch-added #'patch-removed))
  (define change-detector-stx
    (if asserted? #'interest-just-appeared-matching? #'interest-just-disappeared-matching?))
  (quasisyntax/loc outer-expr-stx
    (add-endpoint! #,(source-location->string outer-expr-stx)
                   (lambda () (core:sub #,pat #:meta-level #,meta-level))
                   (lambda (e)
                     (core:match-event e
                       [(? #,event-predicate-stx p)
                        (define proj (core:prepend-at-meta #,proj-stx #,meta-level))
                        (define proj-arity (projection-arity proj))
                        (define entry-set (trie-project/set #:take proj-arity
                                                            (#,patch-accessor-stx p)
                                                            proj))
                        (when (not entry-set)
                          (error 'asserted
                                 "Wildcard interest discovered while projecting by ~v at ~a"
                                 proj
                                 #,(source-location->string event-stx)))
                        #,(let ((entry-handler-stx
                                 (quasisyntax/loc script-stx
                                   (let ((instantiated (instantiate-projection proj entry)))
                                     (and (#,change-detector-stx instantiated)
                                          (schedule-script!
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
                                              #,(source-location->string event-stx))]))
                                #`(for [(entry (in-set entry-set))]
                                    #,entry-handler-stx)))])))))

(define-for-syntax (prepend-at-meta-stx stx level)
  (if (zero? level)
      stx
      #`(at-meta #,(prepend-at-meta-stx stx (- level 1)))))

(define-for-syntax (analyze-event outer-expr-stx event-stx terminal? script-stx)
  (syntax-parse event-stx
    #:literals [core:message asserted retracted rising-edge]
    [(core:message P L:meta-level)
     (define-values (proj pat bindings _instantiated)
       (analyze-pattern event-stx #'P))
     (quasisyntax/loc outer-expr-stx
       (add-endpoint! #,(source-location->string outer-expr-stx)
                      (lambda () (core:sub #,pat #:meta-level L.level))
                      (lambda (e)
                        (core:match-event e
                          [(core:message body)
                           (define capture-vals
                             (match-value/captures
                              body
                              #,(prepend-at-meta-stx proj (syntax-e #'L.level))))
                           (and capture-vals
                                (schedule-script!
                                 #,(if terminal? #'#t #'#f)
                                 (lambda ()
                                   (apply (lambda #,bindings #,script-stx)
                                          capture-vals))))]))))]
    [(asserted P L:meta-level)
     (analyze-asserted/retracted outer-expr-stx event-stx terminal? script-stx #t #'P #'L.level)]
    [(retracted P L:meta-level)
     (analyze-asserted/retracted outer-expr-stx event-stx terminal? script-stx #f #'P #'L.level)]
    [(rising-edge Pred)
     (define field-name (format "~a:rising-edge" (source-location->string event-stx)))
     (quasisyntax/loc outer-expr-stx
       (let ()
         (field [edge-state #f])
         (on-event* #,(source-location->string outer-expr-stx)
                    (lambda (e)
                      (define old-val (edge-state))
                      (define new-val Pred)
                      (when (not (eq? old-val new-val))
                        (edge-state new-val)
                        (when new-val
                          (schedule-script! #,(if terminal? #'#t #'#f)
                                            (lambda () #,script-stx)))))
                    #:priority 1)))]))

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
  (define b (box initial-value))
  (if (current-facet-id)
      (current-field-table (hash-set (current-field-table) desc b))
      (let ((a (current-actor-state)))
        (current-actor-state
         (struct-copy actor-state a
                      [field-table (hash-set (actor-state-field-table a) desc b)]))))
  (field-handle desc))

(define (get-field-box desc)
  (hash-ref (current-field-table)
            desc
            (lambda ()
              (error 'get-field-box
                     "Field ~a used out-of-scope"
                     (field-descriptor-name desc)))))

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
                [facets
                 (if new-facet
                     (hash-set (actor-state-facets a) fid new-facet)
                     (hash-remove (actor-state-facets a) fid))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entering and Leaving Facet Context; Queueing of Work Items

(define-syntax-rule (with-current-facet fid field-table in? body ...)
  (parameterize ((current-field-table field-table)
                 (current-facet-id fid)
                 (in-script? in?))
    body ...))

(define (capture-facet-context proc)
  (let ((field-table (current-field-table))
        (fid (current-facet-id)))
    (lambda args
      (with-current-facet fid field-table #t
        (call-with-continuation-prompt
         (lambda () (apply proc args))
         prompt-tag)))))

(define (schedule-script! #:priority [priority 0] terminal? thunk)
  (if terminal?
      (let ((f (terminate-facet! (current-facet-id))))
        (when f ;; only want to run a terminal script if we genuinely terminated
          (push-script! priority
                        (parameterize ((current-facet-id (facet-parent f)))
                          (capture-facet-context thunk)))))
      (push-script! priority (capture-facet-context thunk))))

(define (push-script! priority thunk-with-context)
  (define v (current-pending-scripts))
  (vector-set! v priority (cons thunk-with-context (vector-ref v priority))))

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
(define (add-facet! #:substate substate? where setup-proc)
  (when (not (in-script?))
    (error 'add-facet!
           "~a: Cannot add facet outside script; are you missing an (on ...)?"
           where))
  (define parent-fid (and substate? (current-facet-id)))
  (define fid next-fid)
  (set! next-fid (+ next-fid 1))
  (update-facet! fid (lambda (_f) (facet 'not-yet-ready
                                         (hasheqv)
                                         '()
                                         (seteqv)
                                         parent-fid)))
  (define starting-field-table
    (if parent-fid
        (match (lookup-facet parent-fid)
          [#f (current-field-table)] ;; TODO: Is this correct???
          [f
           (store-facet! parent-fid (struct-copy facet f
                                                 [children (set-add (facet-children f) fid)]))
           (facet-field-table f)])
        (actor-state-field-table (current-actor-state))))
  (with-current-facet fid starting-field-table #f
    (setup-proc)
    (update-facet! fid (lambda (f) (and f
                                        (struct-copy facet f
                                                     [field-table (current-field-table)])))))
  (facet-handle-event! fid
                       (lookup-facet fid)
                       (patch (actor-state-knowledge (current-actor-state)) trie-empty))
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
         (with-current-facet fid (facet-field-table f) #t
           (for [(script (in-list (reverse (facet-stop-scripts f))))]
             (call-with-continuation-prompt script prompt-tag)))

         f)))

(define (add-stop-script! script-proc)
  (update-facet! (current-facet-id)
                 (lambda (f)
                   (and f
                        (struct-copy facet f
                                     [stop-scripts (cons script-proc (facet-stop-scripts f))])))))

(define (boot-actor script-proc)
  (parameterize ((current-actor-state
                  (actor-state (mux)
                               (hasheqv)
                               trie-empty
                               trie-empty
                               (hash)
                               (make-dataflow-graph)))
                 (current-pending-patch patch-empty)
                 (current-pending-actions '())
                 (current-pending-scripts (make-empty-pending-scripts)))
    (with-current-facet #f (hasheq) #f
      (schedule-script! #f script-proc)
      (run-scripts!))))

(define (run-scripts!)
  (let loop ()
    (define pending-scripts (current-pending-scripts))
    (current-pending-scripts (make-empty-pending-scripts))
    (when (for*/fold [(did-something? #f)]
                     [(scripts (in-vector pending-scripts))
                      (script (in-list (reverse scripts)))]
            (script)
            #t)
      (loop)))
  (refresh-facet-assertions!)
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
                               (with-current-facet fid (facet-field-table f) #f
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
       (parameterize ((current-actor-state
                       (if (patch? e)
                           (struct-copy actor-state a
                                        [previous-knowledge (actor-state-knowledge a)]
                                        [knowledge (update-interests (actor-state-knowledge a) e)])
                           a))
                      (current-pending-patch patch-empty)
                      (current-pending-actions '())
                      (current-pending-scripts (make-empty-pending-scripts)))
         (for [((fid f) (in-hash (actor-state-facets a)))]
           (facet-handle-event! fid f e))
         (run-scripts!))))

(define (facet-handle-event! fid f e)
  (with-current-facet fid (facet-field-table f) #f
    (for [(ep (in-hash-values (facet-endpoints f)))]
      ((endpoint-handler-fn ep) e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Script suspend-and-resume.

(define prompt-tag (make-continuation-prompt-tag 'syndicate))

(define (syndicate-effects-available?)
  (continuation-prompt-available? prompt-tag))

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
        (define raw-resume-parent
          (capture-facet-context
           (lambda results
             (parameterize ((in-script? in?))
               (apply k results)))))
        (define resume-parent
          (lambda results
            (let ((invoking-fid (current-facet-id)))
              (when (not (equal? invoking-fid suspended-fid))
                (terminate-facet! invoking-fid)))
            (push-script! 0 (lambda () (apply raw-resume-parent results)))))
        (proc resume-parent))))
   prompt-tag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immediate actions

(define (ensure-in-script! who)
  (when (not (in-script?))
    (error who "Attempt to perform action outside script; are you missing an (on ...)?")))

(define (send! M #:meta-level [meta-level 0])
  (ensure-in-script! 'send!)
  (schedule-action! (core:message (core:prepend-at-meta M meta-level))))

(define *adhoc-label* -1)

(define (assert! P #:meta-level [meta-level 0])
  (ensure-in-script! 'assert!)
  (update-stream! *adhoc-label* (core:assert P #:meta-level meta-level)))

(define (retract! P #:meta-level [meta-level 0])
  (ensure-in-script! 'retract!)
  (update-stream! *adhoc-label* (core:retract P #:meta-level meta-level)))

(define (patch! p)
  (ensure-in-script! 'patch!)
  (update-stream! *adhoc-label* p))

(define (flush!)
  (ensure-in-script! 'flush!)
  (define ack (gensym 'flush!))
  (until (core:message ack)
         (on-start (send! ack))))
