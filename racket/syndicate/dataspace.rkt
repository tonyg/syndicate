#lang racket/base
;; Dataspaces without configured relaying.

(provide (struct-out dataspace)
         make-dataspace
         dataspace-actor
         make-dataspace-actor
         pretty-print-dataspace)

(require racket/set)
(require racket/match)
(require "functional-queue.rkt")
(require "trie.rkt")
(require "patch.rkt")
(require "hierarchy.rkt")
(require "trace.rkt")
(require "mux.rkt")
(require "pretty.rkt")
(require "core.rkt")
(require "protocol/standard-relay.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax syntax/srcloc))
(require "syntax-classes.rkt")

;; Sentinel
(define missing-process (process #f #f #f))

;; VM private states
(struct dataspace (mux ;; Multiplexer
                   pending-action-queue ;; (Queueof (Vector Label (U Action 'quit) SpaceTime))
                   runnable-pids ;; (Setof PID)
                   process-table ;; (HashTable PID Process)
                   )
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print w [p (current-output-port)])
     (pretty-print-dataspace w p))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (send-event interpreted-point produced-point e pid w)
  (match-define (and the-process (process process-name behavior old-state))
    (hash-ref (dataspace-process-table w) pid missing-process))
  (if (not behavior)
      w
      (let ((turn-begin-point (trace-turn-begin (trace-event-consumed interpreted-point
                                                                      produced-point
                                                                      pid
                                                                      e)
                                                pid
                                                the-process)))
        (invoke-process pid
                        (lambda () (clean-transition (ensure-transition (behavior e old-state))))
                        (match-lambda
                          [#f
                           (trace-turn-end turn-begin-point pid the-process)
                           w]
                          [(and q (<quit> exn final-actions))
                           (define turn-end-point (trace-turn-end turn-begin-point pid the-process))
                           (trace-actor-exit turn-end-point pid exn)
                           (enqueue-actions turn-end-point
                                            (disable-process pid exn w)
                                            pid
                                            (append final-actions (list 'quit)))]
                          [(and t (transition new-state new-actions))
                           (enqueue-actions (trace-turn-end turn-begin-point
                                                            pid
                                                            (process process-name
                                                                     behavior
                                                                     new-state))
                                            (mark-pid-runnable (update-state w pid new-state) pid)
                                            pid
                                            new-actions)])
                        (lambda (exn)
                          (define turn-end-point (trace-turn-end turn-begin-point pid the-process))
                          (trace-actor-exit turn-end-point pid exn)
                          (enqueue-actions turn-end-point
                                           (disable-process pid exn w)
                                           pid
                                           (list 'quit)))))))

(define (update-process-entry w pid f)
  (define old-pt (dataspace-process-table w))
  (match (hash-ref old-pt pid #f)
    [#f w]
    [old-p (struct-copy dataspace w [process-table (hash-set old-pt pid (f old-p))])]))

(define (update-state w pid s)
  (update-process-entry w pid (lambda (p) (update-process-state p s))))

(define (send-event/guard interpreted-point produced-point e pid w)
  (if (patch-empty? e)
      w
      (send-event interpreted-point produced-point e pid w)))

(define (disable-process pid exn w)
  (when exn
    (log-error "Process ~v ~a died with exception:\n~a"
               (process-name (hash-ref (dataspace-process-table w) pid missing-process))
               (append (current-actor-path) (list pid))
               (exn->string exn)))
  ;; We leave a "tombstone", just the process name, until the 'quit pseudoaction takes effect.
  (update-process-entry w pid (lambda (p) (process (process-name p) #f #f))))

(define (invoke-process pid thunk k-ok k-exn)
  (define-values (ok? result)
    (call/extended-actor-path
     pid
     (lambda ()
       (with-handlers ([(lambda (exn) #t) (lambda (exn) (values #f exn))])
         (values #t (with-continuation-mark 'minimart-process pid (thunk)))))))
  (if ok?
      (k-ok result)
      (k-exn result)))

(define (mark-pid-runnable w pid)
  (struct-copy dataspace w [runnable-pids (set-add (dataspace-runnable-pids w) pid)]))

(define (enqueue-actions turn-end-point w label actions)
  (define produced-point (trace-actions-produced turn-end-point label actions))
  (struct-copy dataspace w
    [pending-action-queue
     (queue-append-list (dataspace-pending-action-queue w)
                        (for/list [(a actions)] (vector label a produced-point)))]))

(define-syntax (dataspace-actor stx)
  (syntax-parse stx
    [(dataspace-actor name:name boot-action ...)
     #'(spawn-standard-relay
        (make-dataspace-actor #:name name.N (lambda () (list boot-action ...))))]))

(define (make-dataspace boot-actions)
  (dataspace (mux)
             (list->queue (for/list ((a (in-list (clean-actions boot-actions))))
                            (vector 'meta a #f)))
             (set)
             (hash)))

(define (make-dataspace-actor #:name [name #f] boot-actions-thunk)
  (<actor> (lambda ()
             (list dataspace-handle-event
                   (transition (make-dataspace (boot-actions-thunk)) '())
                   name))
           trie-empty))

(define (inert? w)
  (and (queue-empty? (dataspace-pending-action-queue w))
       (set-empty? (dataspace-runnable-pids w))))

(define (dataspace-handle-event e w)
  (if (or e (not (inert? w)))
      (sequence-transitions (transition w '())
                            (inject-event e)
                            perform-actions
                            (lambda (w) (or (step-children w) (transition w '()))))
      (step-children w)))

(define ((inject-event e) w)
  ;; TODO: What is the best way of getting something sensible to
  ;; supply to `enqueue-actions` as `turn-end-point`? Similar applies
  ;; to use of #f in `make-dataspace` for the boot actions and to
  ;; relaying of `targeted-event`s.
  (transition (if (not e) w (enqueue-actions #f w 'meta (list e))) '()))

(define (perform-actions w)
  (for/fold ([wt (transition (struct-copy dataspace w [pending-action-queue (make-queue)]) '())])
      ((entry (in-list (queue->list (dataspace-pending-action-queue w)))))
    #:break (quit? wt) ;; TODO: should a quit action be delayed until the end of the turn?
    (match-define (vector label a produced-point) entry)
    (define wt1 (transition-bind (perform-action produced-point label a) wt))
    wt1))

(define ((perform-action produced-point label a) w)
  (match a
    [(<actor> boot initial-assertions)
     (invoke-process (mux-next-pid (dataspace-mux w)) ;; anticipate pid allocation
                     (lambda ()
                       (match (boot)
                         [(and results (list (? procedure?) (? general-transition?) _))
                          results]
                         [other
                          (error 'actor
                                 "actor boot procedure must yield boot spec; received ~v"
                                 other)]))
                     (lambda (results)
                       (match-define (list behavior initial-transition name) results)
                       (create-process produced-point
                                       w
                                       behavior
                                       initial-transition
                                       initial-assertions
                                       name))
                     (lambda (exn)
                       (log-error "Spawned process in dataspace ~a died with exception:\n~a"
                                  (current-actor-path)
                                  (exn->string exn))
                       (transition w '())))]
    ['quit
     (define-values (new-mux _label delta delta-aggregate)
       (mux-remove-stream (dataspace-mux w) label))
     (define interpreted-point (trace-action-interpreted produced-point label delta))
     ;; Clean up the "tombstone" left for us by disable-process
     (let ((w (struct-copy dataspace w
                           [process-table (hash-remove (dataspace-process-table w) label)])))
       (begin0 (deliver-patches interpreted-point produced-point w new-mux label delta delta-aggregate)
         (trace-action-interpreted produced-point label a)))]
    [(quit-dataspace)
     (quit)]
    [(? patch? delta-orig)
     (define-values (new-mux _label delta delta-aggregate)
       (mux-update-stream (dataspace-mux w) label delta-orig))
     (define interpreted-point (trace-action-interpreted produced-point label delta))
     (deliver-patches interpreted-point produced-point w new-mux label delta delta-aggregate)]
    [(and m (message body))
     (define interpreted-point (trace-action-interpreted produced-point label a))
     (when (observe? body)
       (log-warning "Stream ~a sent message containing query ~v"
                    (append (current-actor-path) (list label))
                    body))
     (define-values (affected-pids meta-affected?) (mux-route-message (dataspace-mux w) body))
     (transition (for/fold [(w w)] [(pid (in-list affected-pids))]
                   (send-event interpreted-point produced-point m pid w))
                 (and meta-affected? m))]
    [(targeted-event (cons pid remaining-path) e)
     (transition (send-event/guard #f #f (target-event remaining-path e) pid w) '())]))

(define (create-process produced-point w behavior initial-transition initial-assertions name)
  (define initial-assertions? (not (trie-empty? initial-assertions)))
  (define initial-patch (patch initial-assertions trie-empty))
  (define (trace-spawn/initial-patch pid state0)
    (define spawn-point (trace-actor-spawn produced-point pid (process name behavior state0)))
    (cons spawn-point
          (and initial-assertions? (trace-actions-produced spawn-point pid (list initial-patch)))))
  (define-values (postprocess initial-state initial-actions)
    (match (clean-transition initial-transition)
      [#f
       (values (lambda (w pid)
                 (values (trace-spawn/initial-patch pid (void))
                         w))
               #f
               '())]
      [(and q (<quit> exn initial-actions0))
       (values (lambda (w pid)
                 (define points (trace-spawn/initial-patch pid (void)))
                 (match-define (cons spawn-point _) points)
                 (trace-actor-exit spawn-point pid exn)
                 (values points (disable-process pid exn w)))
               #f
               (append initial-actions0 (list 'quit)))]
      [(and t (transition initial-state initial-actions0))
       (values (lambda (w pid)
                 (values (trace-spawn/initial-patch pid initial-state)
                         (mark-pid-runnable w pid)))
               initial-state
               initial-actions0)]))
  (define-values (new-mux new-pid delta delta-aggregate)
    (mux-add-stream (dataspace-mux w) initial-patch))
  (let ((w (struct-copy dataspace w
             [process-table (hash-set (dataspace-process-table w)
                                      new-pid
                                      (process name
                                               behavior
                                               initial-state))])))
    (let-values (((points w) (postprocess w new-pid)))
      (match-define (cons spawn-point initial-patch-produced-point) points)
      (let ((w (enqueue-actions spawn-point w new-pid initial-actions)))
        (deliver-patches (and initial-assertions?
                              (trace-action-interpreted initial-patch-produced-point
                                                        new-pid
                                                        delta))
                         initial-patch-produced-point
                         w
                         new-mux
                         new-pid
                         delta
                         delta-aggregate)))))

(define (deliver-patches interpreted-point
                         produced-point
                         w
                         new-mux
                         acting-label
                         delta
                         delta-aggregate)
  (define-values (patches meta-action)
    (compute-patches (dataspace-mux w) new-mux acting-label delta delta-aggregate))
  (transition (for/fold [(w (struct-copy dataspace w [mux new-mux]))]
                        [(entry (in-list patches))]
                (match-define (cons label event) entry)
                (send-event/guard interpreted-point produced-point event label w))
              (and (patch-non-empty? meta-action) meta-action)))

(define (step-children w)
  (define runnable-pids (dataspace-runnable-pids w))
  (if (set-empty? runnable-pids)
      #f ;; dataspace is inert.
      (transition (for/fold [(w (struct-copy dataspace w [runnable-pids (set)]))]
                            [(pid (in-set runnable-pids))]
                    (send-event #f #f #f pid w))
		  '())))

(define (pretty-print-dataspace w [p (current-output-port)])
  (match-define (dataspace mux qs runnable process-table) w)
  (fprintf p "DATASPACE:\n")
  (fprintf p " - ~a queued actions\n" (queue-length qs))
  (fprintf p " - ~a runnable pids ~a\n" (set-count runnable) (set->list runnable))
  (fprintf p " - ~a live processes\n" (hash-count process-table))
  (fprintf p " - ")
  (display (indented-port-output 3 (lambda (p) (syndicate-pretty-print mux p)) #:first-line? #f) p)
  (newline p)
  (for ([pid (set-union (hash-keys (mux-interest-table mux)) (hash-keys process-table))])
    (define i (hash-ref process-table pid missing-process))
    (fprintf p " ---- process ~a, name ~v, behavior ~v, STATE:\n"
             pid
             (process-name i)
             (process-behavior i))
    (display (indented-port-output 6 (lambda (p) (syndicate-pretty-print (process-state i) p))) p)
    (newline p)
    (fprintf p "      process ~a, name ~v, behavior ~v, CLAIMS:\n"
             pid
             (process-name i)
             (process-behavior i))
    (display (indented-port-output 6 (lambda (p)
                                       (pretty-print-trie (mux-interests-of mux pid) p)))
             p)
    (newline p)))
