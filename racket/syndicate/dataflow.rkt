#lang racket/base
;; Simple lazy dataflow.

(provide dataflow-graph?
         make-dataflow-graph
         dataflow-graph-edges-forward

         current-dataflow-subject-id

         dataflow-record-observation
         dataflow-record-damage
         dataflow-forget-subject
         dataflow-repair-damage)

(require racket/match)
(require racket/set)
(require "store.rkt")
(require "support/hash.rkt")

(struct dataflow-graph (edges-forward ;; object-id -> (Setof subject-id)
                        edges-reverse ;; subject-id -> (Setof object-id)
                        damaged-nodes ;; Setof object-id
                        ) #:transparent) ;; TODO <---- REMOVE
;; TODO ^^^

(define current-dataflow-subject-id (make-parameter #f))

(define (make-dataflow-graph)
  (dataflow-graph (hash)
                  (hash)
                  (set)))

(define (dataflow-record-observation g object-id)
  (match (current-dataflow-subject-id)
    [#f g]
    [subject-id
     (match g
       [(dataflow-graph fwd rev damaged)
        (dataflow-graph (hashset-add fwd object-id subject-id)
                        (hashset-add rev subject-id object-id)
                        damaged)])]))

(define (dataflow-record-damage g object-id)
  (struct-copy dataflow-graph g
    [damaged-nodes (set-add (dataflow-graph-damaged-nodes g) object-id)]))

(define (dataflow-forget-subject g subject-id)
  (define rev (dataflow-graph-edges-reverse g))
  (define subject-objects (hash-ref rev subject-id set))
  (let ((g (struct-copy dataflow-graph g [edges-reverse (hash-remove rev subject-id)])))
    (for/fold [(g g)] [(object-id (in-set subject-objects))]
      (define fwd (dataflow-graph-edges-forward g))
      (struct-copy dataflow-graph g
        [edges-forward (hashset-remove fwd object-id subject-id)]))))

(define (dataflow-repair-damage g repair-node!)
  (define repaired-this-round (set))
  (let loop ((g g))
    (define workset (dataflow-graph-damaged-nodes g))
    (let ((already-damaged (set-intersect workset repaired-this-round)))
      (when (not (set-empty? already-damaged))
        (log-warning "Cyclic dependencies involving ids ~v\n" already-damaged)))

    (let ((g (struct-copy dataflow-graph g [damaged-nodes (set)])))
      (set! workset (set-subtract workset repaired-this-round))
      (set! repaired-this-round (set-union repaired-this-round workset))

      (if (set-empty? workset)
          g
          (loop (for/fold [(g g)] [(object-id (in-set workset))]
                  (define subjects (hash-ref (dataflow-graph-edges-forward g) object-id set))

                  (for/fold [(g g)] [(subject-id (in-set subjects))]
                    (let ((g (dataflow-forget-subject g subject-id)))
                      (parameterize ((current-dataflow-subject-id subject-id))
                        (repair-node! g subject-id))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (struct cell (value) #:mutable
    #:methods gen:custom-write
    [(define (write-proc c port mode)
       (fprintf port "#<cell:~v>" (cell-value c)))])

  (define g #f)

  (define (R b)
    (set! g (dataflow-record-observation g b))
    (cell-value b))

  (define (W b v)
    (when (not (equal? (cell-value b) v))
      (set! g (dataflow-record-damage g b))
      (set-cell-value! b v)))

  (define (repair! g* b)
    (set! g g*)
    (b)
    g)

  (define-syntax-rule (V expr)
    (letrec ((node (cell #f))
             (handler (lambda ()
                        ;; (printf "Evaluating ~a\n" 'expr)
                        (W node expr))))
      (parameterize ((current-dataflow-subject-id handler))
        (handler)
        node)))

  (set! g (make-dataflow-graph))
  (let* ((c (V 123))
         (d (V (* (R c) 2))))
    (check-equal? (list (R c) (R d)) (list 123 246))
    (set! g (dataflow-repair-damage g repair!))
    (check-equal? (list (R c) (R d)) (list 123 246))
    (set! g (dataflow-repair-damage g repair!))
    (check-equal? (list (R c) (R d)) (list 123 246))
    (W c 124)
    (check-equal? (list (R c) (R d)) (list 124 246))
    (set! g (dataflow-repair-damage g repair!))
    (check-equal? (list (R c) (R d)) (list 124 248)))

  (set! g (make-dataflow-graph))

  (define xs (V (list 1 2 3 4)))
  (define sum (V (foldr + 0 (R xs))))
  (define len (V (length (R xs))))
  (define avg (V (if (zero? (R len))
                     (void)
                     (/ (R sum) (R len)))))
  (define scale (V 1))
  (define ans (V (if (zero? (R scale))
                     (void)
                     (and (number? (R avg))
                          (/ (R avg) (R scale))))))

  (define (fix! stage)
    ;; (printf "\n----- Stage: ~a\n" stage)
    (set! g (dataflow-repair-damage g repair!))
    ;; (write `((xs ,(R xs))
    ;;          (sum ,(R sum))
    ;;          (len ,(R len))
    ;;          (avg ,(R avg))
    ;;          (scale ,(R scale))
    ;;          (ans ,(R ans))))
    ;; (newline)
    )

  (define-syntax-rule (check-results vs ...)
    (check-equal? (map R (list xs sum len avg scale ans)) (list vs ...)))

  (fix! 'initial)
  (check-results '(1 2 3 4) 10 4 10/4 1 10/4)

  (W scale 0)
  (fix! 'scale-zero)
  (check-results '(1 2 3 4) 10 4 10/4 0 (void))

  (W xs (list* 9 0 (R xs)))
  (fix! 'with-nine-and-zero)
  (check-results '(9 0 1 2 3 4) 19 6 19/6 0 (void))

  (W xs (list* 5 4 (cddr (R xs))))
  (fix! 'with-five-and-four)
  (check-results '(5 4 1 2 3 4) 19 6 19/6 0 (void))

  (W scale 1)
  (fix! 'scale-one)
  (check-results '(5 4 1 2 3 4) 19 6 19/6 1 19/6)

  (W xs '())
  (fix! 'empty)
  (check-results '() 0 0 (void) 1 #f)

  (W xs (list 4 5 6))
  (fix! 'four-five-six)
  (check-results '(4 5 6) 15 3 15/3 1 15/3))
