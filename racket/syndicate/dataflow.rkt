#lang racket/base
;; Simple lazy dataflow.

(provide dataflow-graph?
         make-dataflow-graph

         current-dataflow-subject-id

         dataflow-record-observation!
         dataflow-record-damage!
         dataflow-forget-subject!
         dataflow-repair-damage!)

(require racket/set)

(struct dataflow-graph (edges-forward ;; object-id -> (Setof subject-id)
                        edges-reverse ;; subject-id -> (Setof object-id)
                        damaged-nodes ;; Setof object-id
                        )
  #:mutable)

(define current-dataflow-subject-id (make-parameter #f))

(define (make-dataflow-graph)
  (dataflow-graph (hash)
                  (hash)
                  (set)))

(define (hash-set-add ht k v [set-ctor set])
  (hash-set ht k (set-add (hash-ref ht k set-ctor) v)))

(define (hash-set-remove ht k v)
  (define old (hash-ref ht k #f))
  (if old
      (let ((new (set-remove old v)))
        (if (set-empty? new)
            (hash-remove ht k)
            (hash-set ht k new)))
      ht))

(define (dataflow-record-observation! g object-id)
  (define subject-id (current-dataflow-subject-id))
  (when subject-id
    (define fwd (dataflow-graph-edges-forward g))
    (set-dataflow-graph-edges-forward! g (hash-set-add fwd object-id subject-id))
    (define rev (dataflow-graph-edges-reverse g))
    (set-dataflow-graph-edges-reverse! g (hash-set-add rev subject-id object-id))))

(define (dataflow-record-damage! g object-id)
  (set-dataflow-graph-damaged-nodes! g (set-add (dataflow-graph-damaged-nodes g) object-id)))

(define (dataflow-forget-subject! g subject-id)
  (define rev (dataflow-graph-edges-reverse g))
  (define subject-objects (hash-ref rev subject-id set))
  (set-dataflow-graph-edges-reverse! g (hash-remove rev subject-id))
  (for [(object-id (in-set subject-objects))]
    (define fwd (dataflow-graph-edges-forward g))
    (set-dataflow-graph-edges-forward! g (hash-set-remove fwd object-id subject-id))))

(define (dataflow-repair-damage! g repair-node!)
  (define repaired-this-round (set))
  (let loop ()
    (define workset (dataflow-graph-damaged-nodes g))
    (set-dataflow-graph-damaged-nodes! g (set))

    (let ((already-damaged (set-intersect workset repaired-this-round)))
      (when (not (set-empty? already-damaged))
        (log-warning "Cyclic dependencies involving ids ~v\n" already-damaged)))

    (set! workset (set-subtract workset repaired-this-round))
    (set! repaired-this-round (set-union repaired-this-round workset))

    (when (not (set-empty? workset))
      (for [(object-id (in-set workset))]
        (define subjects (hash-ref (dataflow-graph-edges-forward g) object-id set))
        (for [(subject-id (in-set subjects))]
          (dataflow-forget-subject! g subject-id)
          (parameterize ((current-dataflow-subject-id subject-id))
            (repair-node! subject-id))))
      (loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (struct cell (value) #:mutable
    #:methods gen:custom-write
    [(define (write-proc c port mode)
       (fprintf port "#<cell:~v>" (cell-value c)))])

  (define g (make-dataflow-graph))

  (define (R b)
    (dataflow-record-observation! g b)
    (cell-value b))

  (define (W b v)
    (when (not (equal? (cell-value b) v))
      (dataflow-record-damage! g b)
      (set-cell-value! b v)))

  (define (repair! b)
    (b))

  (define-syntax-rule (V expr)
    (letrec ((node (cell #f))
             (handler (lambda ()
                        (printf "Evaluating ~a\n" 'expr)
                        (W node expr))))
      (parameterize ((current-dataflow-subject-id handler))
        (handler)
        node)))

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
    (printf "\n----- Stage: ~a\n" stage)
    (dataflow-repair-damage! g repair!)
    (write `((xs ,(R xs))
             (sum ,(R sum))
             (len ,(R len))
             (avg ,(R avg))
             (scale ,(R scale))
             (ans ,(R ans))))
    (newline))

  (fix! 'initial)
  (W scale 0)
  (fix! 'scale-zero)
  (W xs (list* 9 0 (R xs)))
  (fix! 'with-nine-and-zero)
  (W xs (list* 5 4 (cddr (R xs))))
  (fix! 'with-five-and-four)
  (W scale 1)
  (fix! 'scale-one)
  (W xs '())
  (fix! 'empty)
  (W xs (list 4 5 6))
  (fix! 'four-five-six))
