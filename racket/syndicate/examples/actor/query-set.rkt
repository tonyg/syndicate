#lang syndicate/actor

(require racket/set)

(require (submod syndicate/actor priorities))

(define-syntax-rule (query-set field-name P expr)
  (let ()
    (field [field-name (set)])
    (on (asserted P) #:priority *query-priority* (field-name (set-add (field-name) expr)))
    (on (retracted P) #:priority *query-priority* (field-name (set-remove (field-name) expr)))
    field-name))

(define-syntax-rule (query-hash field-name P key-expr value-expr)
  (let ()
    (field [field-name (hash)])
    (on (asserted P)
        (let ((key key-expr))
          (when (hash-has-key? (field-name) key)
            (log-warning "query-hash: ~a: overwriting existing entry ~v"
                         'field-name
                         key))
          (field-name (hash-set (field-name) key value-expr))))
    (on (retracted P) (field-name (hash-remove (field-name) key-expr)))
    field-name))

(define-syntax-rule (query-hash-set field-name P key-expr value-expr)
  (let ()
    (field [field-name (hash)])
    (on (asserted P)
        (let ((key key-expr))
          (field-name (hash-set (field-name)
                                key
                                (set-add (hash-ref (field-name) key set)
                                         value-expr)))))
    (on (retracted P)
        (let ((key key-expr))
          (let ((new-entries (set-remove (hash-ref (field-name) key set)
                                         value-expr)))
            (field-name (if (set-empty? new-entries)
                            (hash-remove (field-name) key)
                            (hash-set (field-name) key new-entries))))))
    field-name))

(actor #:name 'queryer
       (forever
        (define as-set (query-set as-set `(item ,$a ,$b) (list a b)))
        (define as-hash (query-hash as-hash `(item ,$a ,$b) a b))
        (define as-hash-set (query-hash-set as-hash-set `(item ,$a ,$b) a b))

        (on (message 'dump)
            (printf "----------------------------------------\n")
            (printf "Queried as-set:\n")
            (for [(item (as-set))]
              (match-define (list a b) item)
              (printf "  ~v -> ~v\n" a b))
            (newline)
            (printf "Queried as-hash:\n")
            (for [((k v) (in-hash (as-hash)))]
              (printf "  ~v -> ~v\n" k v))
            (newline)
            (printf "Queried as-hash-set:\n")
            (for [((k vs) (in-hash (as-hash-set)))]
              (printf "  ~v -> ~v\n" k vs))
            (printf "----------------------------------------\n")
            (flush-output))))

(actor #:name 'mutator
       (assert! `(item a 1))
       (assert! `(item b 2))
       (assert! `(item b 3))
       (send! 'dump)
       (retract! `(item b ,?))
       (send! 'dump)
       (assert! `(item c 1))
       (assert! `(item c 4))
       (send! 'dump)
       (forever))
