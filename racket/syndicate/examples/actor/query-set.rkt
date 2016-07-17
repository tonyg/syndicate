#lang syndicate/actor

(require racket/set)

(actor #:name 'queryer
       (forever
        (define/query-value as-value 'absent `(item ,$a ,$b) (list a b))
        (define/query-set as-set `(item ,$a ,$b) (list a b)
          #:on-add (log-info "as-set adding ~v/~v" a b)
          #:on-remove (log-info "as-set removing ~v/~v" a b))
        (define/query-hash as-hash `(item ,$a ,$b) a b)
        (define/query-hash-set as-hash-set `(item ,$a ,$b) a b)

        (on (message 'dump)
            (printf "----------------------------------------\n")
            (printf "Queried as-value: ~v\n" (as-value))
            (newline)
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
