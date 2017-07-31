#lang racket/base
;; Implements a trie-like structure mapping structured, compound keys to values.
;; Used to provide the dataspace part of the Syndicate implementation.

;; TODO: examples showing the idea.

(require racket/contract)
(provide combiner/c trie-combiner/c)

(provide (contract-out (rename success trie-success (-> (not/c trie?) trie?)))
         ;; (rename-out [success trie-success])
         (rename-out [success? trie-success?]
                     [success-value trie-success-value])

         (rename-out [open-parenthesis <open-parenthesis>]
                     [canonical-open-parenthesis open-parenthesis])
         (except-out (struct-out open-parenthesis) open-parenthesis)
         struct-type->parenthesis

         ?
         wildcard?
         (struct-out embedded-trie)
         (struct-out capture)
         ?!

         trie-empty
         trie?
         trie
         trie-empty?
         trie-non-empty?
         trie-prepend

         pattern->trie*
         pattern->trie

         trie-combine

         empty-tset-guard
         tset-union-combiner
         tset-subtract-combiner

         (contract-out [trie-union trie-combiner/c])
         (contract-out [trie-intersect trie-combiner/c])
         (contract-out [trie-subtract trie-combiner/c])
         trie-union-all

         trie-lookup
         trie-match-trie

         trie-append
         trie-relabel

         ;; trie-prune-branch
         trie-step
         trie-step*
         trie-step-wild

         projection->pattern
         instantiate-projection
         projection-arity
         trie-project
         trie-key-set
         trie-key-set/single
         trie-project/set
         trie-project/set/single
         project-assertions

         trie-value-fold

         pretty-print-trie
         trie->pretty-string
         trie->abstract-graph
         abstract-graph->dot
         trie->dot

         trie->jsexpr
         jsexpr->trie)

(require racket/set)
(require racket/match)
(require (only-in racket/list append-map make-list))
(require (only-in racket/port call-with-output-string with-output-to-string))
(require "canonicalize.rkt")
(require "treap.rkt")
(require "tset.rkt")
(require "hash-order.rkt")
(require "support/struct.rkt")

(module+ test
  (require rackunit)
  (require racket/pretty)
  (define-logger trie-test))

;; Constructs a structure type and a singleton instance of it.
(define-syntax-rule (define-singleton-struct singleton-name struct-name print-representation)
  (begin
    (struct struct-name ()
	    #:transparent
            #:methods gen:custom-write
            [(define (write-proc v port mode) (display print-representation port))])
    (define singleton-name (struct-name))))

;;---------------------------------------------------------------------------

;; A Trie is one of
;;
;; - #f, indicating no further matches possible,
;; - (success Any), representing a successful match (if the end of
;;   the input has been reached), or
;; - (branch (Treap OpenParenthesis Trie) Trie (Treap Sigma Trie)),
;;   representing a node in the trie from which open-parenthesis
;;   edges, a wildcard edge, or sigma-labelled edges may be taken.
;;
;; Wildcard edges are to be used ONLY when the sought key is not
;; otherwise present: either no such open-parenthesis is listed, or no
;; such sigma is listed.
;;
;; INVARIANT: if a sigma is present in a treap, then the
;;            corresponding value MUST NOT be equal to the wildcard
;;            continuation.
;;
;; INVARIANT: if an open-parenthesis appears in a treap, then the
;;            corresponding value, after removing the same number of
;;            prefixed wildcard edges as the arity of the parenthesis,
;;            MUST NOT be equal to the wildcard continuation.
;;
;; TODO: Document invariants re canonicalization.
;;
(struct success (value) #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode) (pretty-print-trie v port #:with-parens #t))])
(struct branch (opens wild sigmas) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b =?)
     (match-define (branch os1 w1 h1) a)
     (match-define (branch os2 w2 h2) b)
     (and (eq? os1 os2)
          (eq? w1 w2)
          (eq? h1 h2)))
   (define (hash-proc a h)
     (match-define (branch os w h) a)
     (+ (eq-hash-code os)
        (eq-hash-code w)
        (eq-hash-code h)))
   (define (hash2-proc a h)
     (match-define (branch os w h) a)
     (bitwise-xor (eq-hash-code os)
                  (eq-hash-code w)
                  (eq-hash-code h)))]
  #:methods gen:custom-write
  [(define (write-proc v port mode) (pretty-print-trie v port #:with-parens #t))])

;; An OpenParenthesis, (open-parenthesis Natural ParenType), describes
;; the size and type of a compound data structure such as a list,
;; vector, or transparent struct.
(struct open-parenthesis (arity type) #:transparent)

;; A ParenType is one of
;; - 'list, signifying that the containing OpenParenthesis is for a list
;; - 'vector, likewise for a vector
;; - a StructType, likewise for a particular kind of struct.

;; A Pattern is an atom, the special wildcard value (?), an
;; (embedded-trie Trie), or a Racket compound (struct, pair, or
;; vector) containing Patterns.
(define-singleton-struct ? wildcard "★") ;; alternative printing: ¿
(struct embedded-trie (trie) #:transparent)

;; A Projection is an atom, the special wildcard value (?), a (capture
;; Pattern), or a Racket compound (struct, pair, or vector) containing
;; Projections. A Projection is much like a Pattern, but may include
;; captures, and may not include embedded tries.
;;
;; When projecting a trie, the capturing wildcard can be used.
(struct capture (pattern) #:transparent)

;; [Pattern] -> Projection
;; Construct a capture with default pattern of wildcard.
(define (?! [pattern ?]) (capture pattern))

;; Trie
(define trie-empty (canonicalize #f))

;; Any -> Boolean
;; Predicate recognising Tries.
(define (trie? x)
  (or (eq? x trie-empty)
      (success? x)
      (branch? x)))

(define combiner/c (-> any/c any/c trie?))
(define trie-combiner/c (->* (trie? trie?) (#:combiner combiner/c) trie?))

;; Pattern Any {Pattern Any ...} -> Trie
;; Constructs a trie as the union of the given pattern/value pairings.
;; (trie) is the empty trie.
(define (trie . args)
  (let loop ((args args))
    (match args
      ['() trie-empty]
      [(list* pat val rest) (trie-union (loop rest) (pattern->trie val pat))]
      [_ (error 'trie "Uneven argument list: expects equal numbers of patterns and values")])))

;; Trie -> Boolean
;; True iff the argument is the empty trie
(define (trie-empty? t) (not t))

;; Trie -> Boolean
;; True iff the argument is NOT the empty trie
(define (trie-non-empty? t) (not (trie-empty? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart constructors & accessors
;;
;; Maintain this INVARIANT: A Trie is non-empty iff it contains
;; some keys that map to some Values. Essentially, don't bother
;; prepending tokens to a Trie unless there's some possibility it
;; can map to one or more Values.
;;
;; TODO: Document canonicalization invariants.

;; Trie Trie -> Boolean
;; Exploits canonicalization to replace an expensive equal? check with eq?.
(define (requal? a b)
  (eq? a b))

;; (Option Value) -> Trie
;; Return a canonicalized success Trie.
(define (rsuccess v)
  (canonicalize (success v)))

;; Order for open-parentheses
(define (open-parenthesis-order a b)
  (match-define (open-parenthesis a-arity a-type) a)
  (match-define (open-parenthesis b-arity b-type) b)
  (define arity-difference (- a-arity b-arity))
  (cond
    [(< a-arity b-arity) '<]
    [(> a-arity b-arity) '>]
    [(eq? a-type b-type) '=]
    [else (match* (a-type b-type)
            [('list _) '<]
            [('vector 'list) '>]
            [('vector _) '<]
            [(_ 'list) '>]
            [(_ 'vector) '>]
            [(_ _) (hash-order (struct-type-name a-type) (struct-type-name b-type))])]))

;; (Treap OpenParenthesis Trie)
(define empty-omap (treap-empty open-parenthesis-order))

;; Order for sigmas
(define sigma-order hash-order)

;; (Treap Sigma Trie)
(define empty-smap (treap-empty sigma-order))

;; Trie -> Trie
;; If the argument is an empty branch, returns the canonical empty trie;
;; otherwise, returns the argument.
(define (collapse r)
  (match r
    [(branch (== empty-omap eq?) (== trie-empty eq?) (== empty-smap eq?)) trie-empty]
    [_ r]))

;; Trie -> Trie
;; Given a non-empty trie, returns it; otherwise, returns a branch
;; that is equivalent to the empty trie. Inverse of `collapse`.
(define expand
  (let ((canonical-expanded-empty (canonicalize (branch empty-omap trie-empty empty-smap))))
    (lambda (r)
      (if (trie-empty? r)
          canonical-expanded-empty
          r))))

;; Sigma Trie -> Trie
;; Prepends e to r, if r is non-empty.
(define (rsigma e r)
  (if (trie-empty? r)
      r
      (canonicalize (branch empty-omap trie-empty (treap-insert empty-smap e r)))))

;; [ Sigma Trie ] ... -> Trie
(define (rsigma-multi . ers)
  (canonicalize (branch empty-omap
                        trie-empty
                        (let walk ((ers ers))
                          (match ers
                            [(list* e r rest) (treap-insert (walk rest) e r)]
                            [(list) empty-smap])))))

;; Trie -> Trie
;; Prepends a wildcard edge to r, if r is non-empty.
(define (rwild r)
  (if (trie-empty? r)
      r
      (canonicalize (branch empty-omap r empty-smap))))

;; Trie Trie -> Trie
;; Base must be empty or a branch.
;; Returns a trie equivalent to base, but with an added wildcard edge pointing to r.
(define (rwild* base r)
  (if (trie-empty? base)
      (rwild r)
      (canonicalize (collapse (struct-copy branch base [wild r])))))

;; Natural ParenType -> OpenParenthesis
;; Canonicalized ctor for open-parenthesis.
(define (canonical-open-parenthesis arity type)
  (canonicalize (open-parenthesis arity type)))

;; StructType -> OpenParenthesis
(define (struct-type->parenthesis st)
  (canonical-open-parenthesis (struct-type-constructor-arity st) st))

;; OpenParenthesis Trie -> Trie
;; Prepends an open-parenthesis edge to r, if r is non-empty
(define (ropen* paren r)
  (if (trie-empty? r)
      r
      (canonicalize (branch (treap-insert empty-omap paren r) trie-empty empty-smap))))

;; Natural ParenType Trie -> Trie
;; Prepends an open-parenthesis edge to r, if r is non-empty
(define (ropen arity type r)
  (ropen* (canonical-open-parenthesis arity type) r))

;; (U Sigma OpenParenthesis) Trie -> Trie
;; User-accessible rsigma / ropen*.
(define (trie-prepend key r)
  (if (open-parenthesis? key)
      (ropen* key r)
      (rsigma (canonicalize key) r)))

;; Natural Trie -> Trie
;; Prepends n wildcard edges to r, if r is non-empty.
(define (prepend-wilds n r)
  (if (trie-empty? r)
      r
      (let loop ((n n) (r r))
        (if (zero? n)
            r
            (loop (- n 1) (rwild r))))))

;; Natural Trie Trie -> Boolean
;; True iff r1 could have been the output of (prepend-wilds n r2).
(define (equal-upto-wilds? n r1 r2)
  (let loop ((n n) (r r1))
    (if (or (zero? n) (trie-empty? r))
        (requal? r r2)
        (match r
          [(branch (== empty-omap eq?) inner-r (== empty-smap eq?))
           (loop (- n 1) inner-r)]
          [_ #f]))))

;; Trie Sigma -> Trie
;; r must be a branch. Retrieves the continuation after accepting key.
;; If key is absent, returns wild-edge-value.
(define (rlookup-sigma r key)
  (treap-get (branch-sigmas r) key (lambda () (branch-wild r))))

;; Trie Natural ParenType -> Trie
;; r must be a branch. Retrieves the continuation after accepting an open-parenthesis.
(define (rlookup-open* r arity type)
  (rlookup-open r (canonical-open-parenthesis arity type)))

;; Trie OpenParenthesis -> Trie
;; r must be a branch. Retrieves the continuation after accepting an open-parenthesis.
(define (rlookup-open r op)
  (treap-get (branch-opens r)
             op
             (lambda () (prepend-wilds (open-parenthesis-arity op) (branch-wild r)))))

;; Natural Trie Treap Any Trie -> Treap
;; Updates (installs or removes) an edge in the Treap h.
;; Preserves the invariant that a key is never added if its
;; continuation is the same as the wildcard's continuation, modulo
;; `arity`-count wrappings in wildcard edges.
(define (rupdate arity w h key k)
  (if (equal-upto-wilds? arity k w)
      (treap-delete h key)
      (treap-insert h key k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern compilation

;; Value (Listof Pattern) -> Trie
;; Compiles a sequence of patterns into a trie that accepts input
;; matching that sequence, yielding v.
(define (pattern->trie* v ps0)
  ;; Pattern Trie -> Trie
  ;; acc is the continuation-trie for the trie created from ps.
  (define (walk-pair-chain ps acc)
    (foldr walk acc ps))

  ;; Pattern Trie -> Trie
  ;; acc is the continuation-trie for the trie created from p.
  (define (walk p acc)
    (match p
      [(capture sub) (error 'pattern->trie* "Embedded capture in one of the patterns ~v" ps0)]
      [(== ?) (rwild acc)]
      [(? list? ps) (ropen (length ps) 'list (walk-pair-chain ps acc))]
      [(? vector? v) (ropen (vector-length v) 'vector (vector-foldr walk acc v))]
      [(embedded-trie m) (trie-append m (lambda (_mv) acc))]
      ;; TODO: consider options for treating treaps as compounds
      ;; rather than (useless) atoms
      [(? treap?) (error 'pattern->trie "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (define fields (cdr (vector->list (struct->vector p))))
       (ropen (length fields) (struct->struct-type p) (walk-pair-chain fields acc))]
      [other (rsigma (canonicalize other) acc)]))

  (walk-pair-chain ps0 (rsuccess v)))

;; Value Pattern* -> Trie
;; Convenience form of pattern->trie*.
(define (pattern->trie v . ps)
  (pattern->trie* v ps))

;; (A B -> B) B (Vectorof A) -> B
(define (vector-foldr kons knil v)
  (for/fold [(acc knil)] [(elem (in-vector v (- (vector-length v) 1) -1 -1))]
    (kons elem acc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trie combinators

;; Trie Trie -> Nothing
(define (asymmetric-trie-error where lhs rhs)
  (error where "Asymmetric tries: lhs ~v, rhs ~v" lhs rhs))

;; ... -> Trie
;; Generic combiner, used by union, intersection, difference etc.
(define (trie-combine combine-success ;; Trie Trie -> Trie
                      left-empty      ;; Trie -> Trie
                      right-empty     ;; Trie -> Trie
                      left-base       ;; Trie -> Trie
                      right-base      ;; Trie -> Trie
                      r1              ;; Trie
                      r2              ;; Trie
                      )
  (let walk ((r1 r1) (r2 r2))
    (collapse
     (cond
       [(and (branch? r1) (branch? r2)) (fold-over-keys r1 r2 walk (left-base r1) (right-base r2))]
       [(or (success? r1) (success? r2)) (combine-success r1 r2)]
       [(trie-empty? r1) (left-empty r2)]
       [(trie-empty? r2) (right-empty r1)]))))

;; ... -> Trie
;; Combines two branches.
(define (fold-over-keys r1         ;; Trie
                        r2         ;; Trie
                        combine    ;; Trie Trie -> Trie
                        left-base  ;; Trie
                        right-base ;; Trie
                        )
  (match-define (branch os1 w1 h1) r1)
  (match-define (branch os2 w2 h2) r2)

  (define w (combine w1 w2))

  (define (process key-set base updater)
    (for/fold [(acc base)] [(key (in-set key-set))]
      (updater key acc)))

  (define (rupdate-open key acc)
    (define arity (open-parenthesis-arity key))
    (rupdate arity w acc key (combine (rlookup-open r1 key) (rlookup-open r2 key))))

  (define (rupdate-sigma key acc)
    (rupdate 0 w acc key (combine (rlookup-sigma r1 key) (rlookup-sigma r2 key))))

  (canonicalize
   (branch
    (cond
      [(and (trie-non-empty? w1) (trie-non-empty? w2))
       (process (set-union (treap-keys os1) (treap-keys os2)) empty-omap rupdate-open)]
      [(or (trie-non-empty? w1) (and (trie-empty? w2) (>= (treap-size os1) (treap-size os2))))
       (process (treap-keys os2) (branch-opens (expand left-base)) rupdate-open)]
      [else
       (process (treap-keys os1) (branch-opens (expand right-base)) rupdate-open)])
    w
    (cond
      [(and (trie-non-empty? w1) (trie-non-empty? w2))
       (process (set-union (treap-keys h1) (treap-keys h2)) empty-smap rupdate-sigma)]
      [(or (trie-non-empty? w1) (and (trie-empty? w2) (>= (treap-size h1) (treap-size h2))))
       (process (treap-keys h2) (branch-sigmas (expand left-base)) rupdate-sigma)]
      [else
       (process (treap-keys h1) (branch-sigmas (expand right-base)) rupdate-sigma)]))))

(define (tset-union-combiner s1 s2)
  (success (tset-union s1 s2)))

;; Trie Trie [#:combiner (Any Any -> Trie)] -> Trie
;; Computes the union of the tries passed in. Treats them as multimaps by default.
(define (trie-union re1 re2 #:combiner [combiner tset-union-combiner])
  (define (combine-success r1 r2)
    (match* (r1 r2)
      [((success v1) (success v2)) (canonicalize (combiner v1 v2))]
      [((? trie-empty?) r) r]
      [(r (? trie-empty?)) r]
      [(_ _) (asymmetric-trie-error 'trie-union r1 r2)]))
  (trie-combine combine-success values values values values re1 re2))

;; (Listof Trie) [#:combiner (Any Any -> Trie)] -> Trie
;; n-ary trie-union.
(define (trie-union-all tries #:combiner [combiner tset-union-combiner])
  (foldr (lambda (t acc) (trie-union t acc #:combiner combiner)) trie-empty tries))

;; Any -> Trie
(define (->empty t) trie-empty)

;; Trie Trie -> Trie
;; Computes the intersection of the tries passed in. Treats them as multimaps by default.
(define (trie-intersect re1 re2 #:combiner [combiner tset-union-combiner])
  (define (combine-success r1 r2)
    (match* (r1 r2)
      [((success v1) (success v2)) (canonicalize (combiner v1 v2))]
      [((? trie-empty?) _) trie-empty]
      [(_ (? trie-empty?)) trie-empty]
      [(_ _) (asymmetric-trie-error 'trie-intersect r1 r2)]))
  (trie-combine combine-success ->empty ->empty ->empty ->empty re1 re2))

(define (empty-tset-guard s)
  (if (tset-empty? s) trie-empty (success s)))

(define (tset-subtract-combiner s1 s2)
  (empty-tset-guard (tset-subtract s1 s2)))

;; Trie Trie -> Trie
;; Removes re2's mappings from re1.
(define (trie-subtract re1 re2 #:combiner [combiner tset-subtract-combiner])
  (define (combine-success r1 r2)
    (match* (r1 r2)
      [((success v1) (success v2)) (canonicalize (combiner v1 v2))]
      [((? trie-empty?) _) trie-empty]
      [(r (? trie-empty?)) r]
      [(_ _) (asymmetric-trie-error 'trie-subtract r1 r2)]))
  (trie-combine combine-success ->empty values values ->empty re1 re2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching single keys into a trie

;; Trie InputValue Value -> Value
;; Converts the nested structure v on-the-fly into a sequence of
;; Sigmas and OpenParentheses and runs them through the Trie r. If v
;; leads to a success Trie, returns the values contained in the
;; success Trie; otherwise, returns failure-result.
(define (trie-lookup r v failure-result #:wildcard-union [wildcard-union #f])
  (define (walk vs r)
    (match r
      [(? trie-empty?) failure-result]
      [(success result)
       (if (null? vs)
	   result
	   failure-result)]
      [(branch os w h)
       (define (get-open arity type)
         (treap-get os (canonical-open-parenthesis arity type) (lambda () 'missing)))
       (match vs
         ['() failure-result]
         [(cons (== ?) vs1)
          (when (not wildcard-union) (error 'trie-lookup "Cannot match wildcard as a value"))
          (let* ((seed (walk vs1 w))
                 (seed (for/fold [(seed seed)] [(entry (in-list (treap-to-alist os)))]
                         (match-define (cons (open-parenthesis arity _) k) entry)
                         (wildcard-union seed (walk (append (make-list arity ?) vs1) k))))
                 (seed (for/fold [(seed seed)] [(k (in-list (treap-values h)))]
                         (wildcard-union seed (walk vs1 k)))))
            seed)]
         [(cons (? list? l) vs1)
          (match (get-open (length l) 'list)
            ['missing (walk vs1 w)]
            [k (walk (append l vs1) k)])]
         [(cons (vector vv ...) vs1)
          (match (get-open (length vv) 'vector)
            ['missing (walk vs1 w)]
            [k (walk (append vv vs1) k)])]
         [(cons (? non-object-struct? s) vs1)
          (define fields (cdr (vector->list (struct->vector s))))
          (match (get-open (length fields) (struct->struct-type s))
            ['missing (walk vs1 w)]
            [k (walk (append fields vs1) k)])]
         [(cons v vs1)
          (walk vs1 (rlookup-sigma r (canonicalize v)))])]))
  (walk (list v) r))

;; Trie Trie Value (Any Any Value -> Value) -> Value
;;
;; Similar to trie-lookup, but instead of a single key,
;; accepts a Trie serving as *multiple* simultaneously-examined
;; keys. Returns the union of all successful values reached by the
;; probe. Logically similar to a kind of *intersection* of re1 and re2.
(define (trie-match-trie re1 re2 #:seed seed #:combiner combiner)
  (let walk ((re1 re1) (re2 re2) (acc seed))
    (match* (re1 re2)
      [((? trie-empty?) _) acc]
      [(_ (? trie-empty?)) acc]

      [((branch os1 w1 h1) (branch os2 w2 h2))
       (define (keys-from x1 x2)
         (cond [(and w1 w2) (set-union (treap-keys x1) (treap-keys x2))]
               [w1 (treap-keys x2)]
               [w2 (treap-keys x1)]
               [(< (treap-size x1) (treap-size x2)) (treap-keys x1)]
               [else (treap-keys x2)]))
       (let* ((acc (walk w1 w2 acc))
              (acc (for/fold [(acc acc)]
                             [(key (in-set (keys-from os1 os2)))]
                     (walk (rlookup-open re1 key) (rlookup-open re2 key) acc)))
              (acc (for/fold [(acc acc)]
                             [(key (in-set (keys-from h1 h2)))]
                     (walk (rlookup-sigma re1 key) (rlookup-sigma re2 key) acc))))
         acc)]

      [((success v1) (success v2)) (combiner v1 v2 acc)]
      [(_ _) (asymmetric-trie-error 'trie-match-trie re1 re2)])))

;; Trie × (Value → Trie) → Trie
;; Since Tries accept *sequences* of input values, this appends two
;; tries into a single trie that accepts their concatenation.
;; Because tries map inputs to values, the second trie is
;; expressed as a function from success-values from the first trie
;; to a second trie.
(define (trie-append m0 m-tail-fn)
  (let walk ((m m0))
    (match m
      [(? trie-empty?) trie-empty]
      [(success v) (canonicalize (m-tail-fn v))]
      [(branch os w0 h)
       (define w (walk w0))
       (canonicalize
        (collapse
         (branch (for/fold [(acc empty-omap)] [(entry (treap-to-alist os))]
                   (match-define (cons (and key (open-parenthesis arity _)) k) entry)
                   (rupdate arity w acc key (walk k)))
                 w
                 (for/fold [(acc empty-smap)] [(entry (treap-to-alist h))]
                   (match-define (cons key k) entry)
                   (rupdate 0 w acc key (walk k))))))])))

;; Trie (Value -> (Option Value)) -> Trie
;; Maps f over success values in m. If f returns #f, turns the success
;; into a failure.
(define (trie-relabel t f)
  (trie-append t (lambda (v)
                   (match (f v)
                     [#f trie-empty]
                     [result (success result)]))))

;; DANGEROUS: doesn't adjust any wild edge. So if you give it m=★, it
;; will give you the wrong answer. Note that trie-step uses
;; rlookup-open, which deals with the wild edges, so doesn't have this
;; problem.
;;
;; ;; Trie (U OpenParenthesis Sigma) -> Trie
;; ;; Outright removes tries reachable from m via edges labelled with key.
;; ;; Useful for removing (at-meta *) when the success value along that
;; ;; branch doesn't matter.
;; (define (trie-prune-branch m key)
;;   (match* (m key)
;;     [((branch os w h) (open-parenthesis arity _))
;;      (canonicalize (collapse (struct-copy branch m [opens (rupdate arity w os key trie-empty)])))]
;;     [((branch os w h) _)
;;      (canonicalize (collapse (struct-copy branch m [sigmas (rupdate 0 w h key trie-empty)])))]
;;     [(_ _) m]))

;; Trie (U OpenParenthesis Sigma) -> Trie
(define (trie-step m key)
  (match* (m key)
    [((? branch?) (? open-parenthesis?))
     (rlookup-open m key)]
    [((? branch?) _)
     (rlookup-sigma m (canonicalize key))]
    [(_ _) trie-empty]))

;; Trie (Listof (U OpenParenthesis Sigma)) -> Trie
(define (trie-step* t keys)
  (foldl (lambda (key t) (trie-step t key)) t keys))

;; Trie -> Trie
(define (trie-step-wild t)
  ;; Trie Natural -> Trie
  (define (walk t n)
    (match* (t n)
      [(_ 0) t]
      [((branch os w rs) _)
       (define n-1 (sub1 n))
       (define w-k (walk w n-1))
       (define o-ks
         (for/fold ([acc w-k])
                   ([entry (treap-to-alist os)])
           (match-define (cons (open-parenthesis arity _) k) entry)
           (trie-union acc (walk (walk k arity) n-1))))
       (for/fold ([acc o-ks])
                 ([entry (treap-to-alist rs)])
         (match-define (cons _ k) entry)
         (trie-union acc (walk k n-1)))]
      [(_ _)
       trie-empty]))

  (walk t 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projection

;; Projection -> Pattern
;; Strips captures from its argument, returning an equivalent non-capturing pattern.
(define (projection->pattern p)
  (let walk ((p p))
    (match p
      [(capture sub) sub] ;; TODO: maybe enforce non-nesting here too?
      [(cons p1 p2) (cons (walk p1) (walk p2))]
      [(? vector? v) (for/vector [(e (in-vector v))] (walk e))]
      ;; TODO: consider options for treating treaps as compounds
      ;; rather than (useless) atoms
      [(? treap?) (error 'projection->pattern "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (apply (struct-type-make-constructor (struct->struct-type p))
	      (map walk (cdr (vector->list (struct->vector p)))))]
      [other other])))

;; Projection (Listof Pattern) -> Pattern
;; Instantiates captures in its first argument with values from its second.
;; ASSUMPTION: that each captured val matches the subpattern in each capture
;; ASSUMPTION: (length captured-vals) == number of captures in p
(define (instantiate-projection p captured-vals)
  (define (consume-capture!)
    (begin0 (car captured-vals)
      (set! captured-vals (cdr captured-vals))))
  (let walk ((p p))
    (match p
      [(capture sub) (consume-capture!)]
      [(cons p1 p2)
       (define s1 (walk p1))
       (define s2 (walk p2))
       (cons s1 s2)]
      [(? vector? v) (for/vector [(e (in-vector v))] (walk e))]
      ;; TODO: consider options for treating treaps as compounds
      ;; rather than (useless) atoms
      [(? treap?) (error 'projection->pattern "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (apply (struct-type-make-constructor (struct->struct-type p))
	      (map walk (cdr (vector->list (struct->vector p)))))]
      [other other])))

;; Projection -> Natural
;; Counts the number of captures in its argument.
(define (projection-arity p)
  (let walk ((p p))
    (match p
      [(capture sub) 1] ;; TODO: maybe enforce non-nesting here too?
      [(cons p1 p2) (+ (walk p1) (walk p2))]
      [(? vector? v) (for/sum [(e (in-vector v))] (walk e))]
      ;; TODO: consider options for treating treaps as compounds
      ;; rather than (useless) atoms
      [(? treap?) (error 'projection->pattern "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (for/sum [(e (in-list (cdr (vector->list (struct->vector p)))))] (walk e))]
      [other 0])))

;; Trie Projection [#:project-success (Any -> Trie)] [#:combiner (Any Any -> Trie)] -> Trie
;; The result matches a sequence of inputs of length equal to the number of captures.
(define (trie-project whole-t whole-spec
                      #:project-success [project-success success]
                      #:combiner [combiner tset-union-combiner])

  (define (error-embedded-trie)
    (error 'trie-project "Cannot embed trie in projection ~v" whole-spec))

  (define (error-treap-atom)
    (error 'trie-project "Unsupported treap in projection ~v" whole-spec))

  ;; Trie (Listof Projection) ParenType (Listof Projection) (Trie -> Trie) -> Trie
  ;; Descends (with capture) through an open-parenthesis edge from `t`, which must be a branch.
  (define (walk-open/capture t inner-specs type specs-rest kont)
    (define arity (length inner-specs))
    (ropen arity type (walk/capture (rlookup-open* t arity type) inner-specs
                                    (lambda (intermediate)
                                      (walk/capture intermediate specs-rest kont)))))

  ;; Trie (Listof Projection) ParenType (Listof Projection) (Trie -> Trie) -> Trie
  ;; As walk-open/capture, but without capturing.
  (define (walk-open t inner-specs type specs-rest kont)
    (define arity (length inner-specs))
    (walk (rlookup-open* t arity type) inner-specs
          (lambda (intermediate)
            (walk intermediate specs-rest kont))))

  ;; Trie (Listof Projection) (Trie -> Trie) -> Trie
  ;; Capture specified items from `t`, and then invoke `kont` on each remnant branch.
  (define (walk/capture t specs kont)
    (match specs
      ['() (kont t)]
      [(cons spec specs-rest)
       (match t
         [(branch os w0 h)
          (match spec
            [(? capture?) (error 'trie-project "Nested capture in projection ~v" whole-spec)]
            [(== ?)
             (define w (walk/capture w0 specs-rest kont))
             (canonicalize
              (collapse
               (branch (for/fold [(acc empty-omap)] [(entry (treap-to-alist os))]
                         (match-define (cons (and key (open-parenthesis arity _)) k) entry)
                         (define inner-specs (make-list arity ?))
                         (rupdate arity w acc key
                                  (walk/capture k inner-specs
                                                (lambda (intermediate)
                                                  (walk/capture intermediate specs-rest kont)))))
                       w
                       (for/fold [(acc empty-smap)] [(entry (treap-to-alist h))]
                         (match-define (cons key k) entry)
                         (rupdate 0 w acc key (walk/capture k specs-rest kont))))))]
            [(? list? inner-specs)
             (walk-open/capture t inner-specs 'list specs-rest kont)]
            [(vector inner-specs ...)
             (walk-open/capture t inner-specs 'vector specs-rest kont)]
            [(embedded-trie _) (error-embedded-trie)]
            [(? treap?) (error-treap-atom)]
            [(? non-object-struct?)
             (define fields (cdr (vector->list (struct->vector spec))))
             (walk-open/capture t fields (struct->struct-type spec) specs-rest kont)]
            [other0
             (define other (canonicalize other0))
             (rsigma other (walk/capture (rlookup-sigma t other) specs-rest kont))])]
         [_ trie-empty])]))

  ;; Trie (Listof Projection) (Trie -> Trie) -> Trie
  ;; As walk/capture, but without capturing.
  (define (walk t specs kont)
    (match specs
      ['() (kont t)]
      [(cons spec specs-rest)
       (match t
         [(branch os w0 h)
          (match spec
            [(capture sub)
             (walk/capture t (list sub)
                           (lambda (intermediate)
                             (walk intermediate specs-rest kont)))]
            [(== ?)
             (trie-union-all
              #:combiner combiner
              (cons (walk w0 specs-rest kont)
                    (append (for/list [(entry (treap-to-alist os))]
                              (match-define (cons (and key (open-parenthesis arity _)) k) entry)
                              (define inner-specs (make-list arity ?))
                              (walk k inner-specs (lambda (intermediate)
                                                    (walk intermediate specs-rest kont))))
                            (for/list [(entry (treap-to-alist h))]
                              (match-define (cons key k) entry)
                              (walk k specs-rest kont)))))]
            [(? list? inner-specs)
             (walk-open t inner-specs 'list specs-rest kont)]
            [(vector inner-specs ...)
             (walk-open t inner-specs 'vector specs-rest kont)]
            [(embedded-trie _) (error-embedded-trie)]
            [(? treap?) (error-treap-atom)]
            [(? non-object-struct?)
             (define fields (cdr (vector->list (struct->vector spec))))
             (walk-open t fields (struct->struct-type spec) specs-rest kont)]
            [other0
             (define other (canonicalize other0))
             (walk (rlookup-sigma t other) specs-rest kont)])]
         [_ trie-empty])]))

  (walk whole-t (list whole-spec)
        (match-lambda
          [(success v) (canonicalize (project-success v))]
          [_ trie-empty])))

;; ParenType (Listof Value) -> Value
;; Wraps a sequence of values in the given parenthesis type, reconstructing the "original" value.
(define (reconstruct-sequence type items)
  (match type
    ['list items]
    ['vector (list->vector items)]
    [(? struct-type?) (apply (struct-type-make-constructor type) items)]))

;; Trie #:take Natural → (Option (Setof (Listof Value)))
;; Extracts `take-count` "keys" from `m`, representing sequences as
;; lists. Multiplies out unions. Returns `#f` if any dimension of `m`
;; is infinite.
(define (trie-key-set m #:take take-count0)

  (define (walk m take-count vals-rev kont)
    (if (zero? take-count)
        (kont (reverse vals-rev) m)
        (match m
          [(? trie-empty?) (set)]
          [(success _)
           (error 'trie-key-set "Trie contains fewer than the requested ~v items" take-count0)]
          [(branch _ (? trie-non-empty?) _) #f]
          [(branch os _ h)
           (maybe-set-union
            (for/fold [(acc (set))] [(entry (in-list (treap-to-alist os))) #:break (not acc)]
              (match-define (cons (open-parenthesis arity type) k) entry)
              (maybe-set-union acc
                               (walk k arity '()
                                     (lambda (items m1)
                                       (define item (reconstruct-sequence type items))
                                       (walk m1 (- take-count 1) (cons item vals-rev) kont)))))
            (for/fold [(acc (set))] [(entry (in-list (treap-to-alist h))) #:break (not acc)]
              (match-define (cons key k) entry)
              (maybe-set-union acc (walk k (- take-count 1) (cons key vals-rev) kont))))])))

  ;; (Option (Setof A)) (Option (Setof A)) -> (Option (Setof A))
  (define (maybe-set-union s1 s2) (and s1 s2 (set-union s1 s2)))

  (walk m take-count0 '()
        (lambda (items tail)
          (match tail
            [(? trie-empty?) (set)]
            [(success _) (set items)]
            [(? branch?)
             (error 'trie-key-set "Trie contains more than the requested ~v items" take-count0)]))))

;; Trie → (Option (Setof Value))
;; As trie-key-set, but extracts just the first captured subvalue.
(define (trie-key-set/single m)
  (define vss (trie-key-set m #:take 1))
  (and vss (for/set [(vs (in-set vss))] (car vs))))

;; Convenience forms for the common operation of projecting a Trie
;; followed by converting the result to a Racket set (possibly
;; containing just the first captured subvalue).
(define-syntax-rule (trie-project/set #:take take-count arg ...)
  (trie-key-set #:take take-count (trie-project arg ...)))
(define-syntax-rule (trie-project/set/single arg ...)
  (trie-key-set/single (trie-project arg ...)))

;; Ultra-convenience form.
(define (project-assertions m p)
  (trie-project/set/single m p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (trie-value-fold kons seed m)
  (let walk ((seed seed) (m m))
    (match m
      [(? trie-empty?) seed]
      [(success v) (kons v seed)]
      [(branch os w h)
       (let* ((seed (walk seed w))
              (seed (for/fold [(seed seed)] [(entry (in-list (treap-to-alist os)))]
                      (walk seed (cdr entry))))
              (seed (for/fold [(seed seed)] [(k (in-list (treap-values h)))]
                      (walk seed k))))
         seed)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: if we don't collapse the success-paths that the failure-paths
;; are taking chunks out of, is it even worth returning the
;; failure-paths? i.e. this function might be silly as written
(define (trie-decompose t #:take [take-count 1])
  (define (prepend pat1) (match-lambda [(list patN t) (list (cons pat1 patN) t)]))
  (define (walk t take-count)
    (if (zero? take-count)
        (list (list '() t))
        (match t
          [(? trie-empty?) (map (prepend ?) (walk t (- take-count 1)))]
          [(? success?) '()]
          [(branch os w h)
           (append (append-map (match-lambda
                                 [(and key (open-parenthesis arity type))
                                  (append-map
                                   (match-lambda
                                     [(list patN t)
                                      (define item (reconstruct-sequence type patN))
                                      (map (prepend item) (walk t (- take-count 1)))])
                                   (walk (rlookup-open t key) arity))])
                               (set->list (treap-keys os)))
                   (append-map (lambda (key)
                                 (map (prepend key) (walk (rlookup-sigma t key) (- take-count 1))))
                               (set->list (treap-keys h)))
                   (if (trie-empty? w)
                       '()
                       (map (prepend ?) (walk w (- take-count 1)))))])))
  (walk t take-count))

(define (trie->patterns t)
  (define-values (added removed)
    (for/fold [(added '()) (removed '())] [(entry (in-list (trie-decompose t)))]
      (match entry
        [(list (list p) (? success?)) (values (cons p added) removed)]
        [(list (list p) (? trie-empty?)) (values added (cons p removed))])))
  `((added ,@added)
    (removed ,@removed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Trie [OutputPort] [#:indent (U Nat String)] [#:with-parens Boolean] -> Trie
;; Pretty-prints the given trie on the given port, with
;; second-and-subsequent lines indented by the given amount (or the
;; given prefix). Returns the trie.
(define (pretty-print-trie m [port (current-output-port)]
                           #:indent [initial-indent 0]
                           #:with-parens [with-parens #f])
  (define (d x) (display x port))
  (define (walk prefix m)
    (match m
      [(? trie-empty?)
       (d " ::: nothing")]
      [(success vs)
       (d " {")
       (d (if (tset? vs) (cons 'tset (tset->list vs)) vs))
       (d "}")]
      [(branch os w h)
       (define need-sep? #f)
       (define (dump-one key k)
         (cond [need-sep?
                (newline port)
                (d prefix)]
               [else (set! need-sep? #t)])
         (d " ")
         (define keystr (call-with-output-string
                         (lambda (p)
                           (match key
                             [(open-parenthesis arity (? symbol? type))
                              (fprintf p "<~a/~a" type arity)]
                             [(open-parenthesis arity (? struct-type? type))
                              (fprintf p "<s:~a/~a" (struct-type-name type) arity)]
                             [_
                              (write key p)]))))
         (d keystr)
         (walk (string-append prefix " " keystr) k))
       (define (dump tr)
         (for [(entry (treap-to-alist tr))]
           (match-define (cons key k) entry)
           (dump-one key k)))
       (dump os)
       (dump h)
       (when (trie-non-empty? w) (dump-one '★ w))]))
  (when with-parens
    (display "{{" port)
    (newline port))
  (walk (if (string? initial-indent)
            initial-indent
            (make-string initial-indent #\space))
        m)
  (newline port)
  (if with-parens
      (display "}}" port)
      (newline port))
  m)

(define (trie->pretty-string m #:indent [initial-indent 0] #:with-parens [with-parens #f])
  (with-output-to-string
    (lambda () (pretty-print-trie m #:indent initial-indent #:with-parens with-parens))))

(define (trie->abstract-graph m #:transform-success [transform-success values])
  (define nodes (hasheq))
  (define edges '())
  (define (add-edge! source-id label target)
    (set! edges (cons (list source-id label (walk target)) edges)))
  (define (walk m)
    (car
     (hash-ref nodes m
               (lambda ()
                 (define node-info
                   (match m
                     [(? trie-empty?) (list 'fail)]
                     [(success v) (list 'ok (transform-success v))]
                     [(? branch?) (list 'branch)]))
                 (define source-id (gensym 'i))
                 (define entry (cons source-id node-info))
                 (set! nodes (hash-set nodes m entry))
                 (match m
                   [(? trie-empty?) (void)]
                   [(success _) (void)]
                   [(branch os w h)
                    (treap-fold os (lambda (seed k v) (add-edge! source-id k v)) (void))
                    (when (trie-non-empty? w) (add-edge! source-id ? w))
                    (treap-fold h (lambda (seed k v) (add-edge! source-id k v)) (void))])
                 entry))))
  (walk m)
  (list (hash-values nodes) edges))

(define (abstract-graph->dot g)
  (match-define (list nodes edges) g)
  (with-output-to-string
    (lambda ()
      (printf "digraph Trie {\n")
      (for ((n nodes))
        (match n
          [(list id type) (printf "  ~a [label=\"~a\"];\n" id type)]
          [(list id type x) (printf "  ~a [label=\"~a ~v\"];\n" id type x)]))
      (for ((e edges))
        (match e
          [(list s #f t) (printf "  ~a -> ~a;\n" s t)]
          [(list s label t) (printf "  ~a -> ~a [label=\"~v\"];\n" s t label)]))
      (printf "}\n"))))

(define (trie->dot m #:transform-success [transform-success values])
  (abstract-graph->dot (trie->abstract-graph m #:transform-success transform-success)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ParenType -> String
(define (paren-type->string type)
  (match type
    ['list "L"]
    ['vector "V"]
    [(? struct-type?)
     (string-append ":" (symbol->string (struct-type-name type)))]))

;; Natural String (String -> (Option struct-type)) -> ParenType
(define (string->paren-type arity s lookup-struct-type)
  (match s
    ["L" 'list]
    ["V" 'vector]
    [_ (if (char=? (string-ref s 0) #\:)
           (or (lookup-struct-type arity (substring s 1))
               (error 'string->paren-type "Unexpected struct type name ~v" (substring s 1)))
           (error 'string->paren-type "Invalid paren-type string representation ~v" s))]))

;; Trie (Any -> JSExpr) [#:serialize-atom (Any -> JSExpr)] -> JSExpr
(define (trie->jsexpr m success->jsexpr #:serialize-atom [serialize-atom values])
  (let walk ((m m))
    (match m
      [(? trie-empty?) '()]
      [(success v) (list (success->jsexpr v))]
      [(branch opens wild sigmas)
       (list (for/list [(kv (treap-to-alist opens))]
               (match-define (cons (open-parenthesis arity type) v) kv)
               (list arity
                     (paren-type->string type)
                     (walk v)))
             (walk wild)
             (for/list [(kv (treap-to-alist sigmas))]
               (match-define (cons k v) kv)
               (list (serialize-atom k)
                     (walk v))))])))

;; JSExpr (JSExpr -> Any) [String -> (Option struct-type)] [#:deserialize-atom (JSExpr -> Any)]
;;   -> Trie
;; Deserializes a matcher from a JSON expression.
(define (jsexpr->trie j
                      jsexpr->success
                      [lookup-struct-type (lambda (arity t) #f)]
                      #:deserialize-atom [deserialize-atom values])
  (let walk ((j j))
    (match j
      ['() #f]
      [(list vj) (rsuccess (jsexpr->success vj))]
      [(list jopens jwild jsigmas)
       (canonicalize
        (collapse
         (branch (for/fold [(acc empty-omap)] [(jopen (in-list jopens))]
                   (match-define (list arity type-str vj) jopen)
                   (define type (string->paren-type arity type-str lookup-struct-type))
                   (treap-insert acc (canonical-open-parenthesis arity type) (walk vj)))
                 (walk jwild)
                 (for/fold [(acc empty-smap)] [(jsigma (in-list jsigmas))]
                   (match-define (list atom vj) jsigma)
                   (treap-insert acc (canonicalize (deserialize-atom atom)) (walk vj))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (struct test-foo (bar) #:transparent)
  (struct test-bar (zot quux) #:transparent)

  (define tset datum-tset)

  (define SA (tset 'A))
  (define SB (tset 'B))
  (define SC (tset 'C))
  (define SD (tset 'D))
  (define Sfoo (tset 'foo))
  (define S+ (tset '+))
  (define SX (tset 'X))

  (check-equal? (trie 123 SA) (rsigma 123 (rsuccess SA)))
  (check-equal? (trie (list 1 2) SA) (ropen 2 'list (rsigma 1 (rsigma 2 (rsuccess SA)))))
  (check-equal? (trie (list ? 2) SA) (ropen 2 'list (rwild (rsigma 2 (rsuccess SA)))))
  (check-equal? (trie (test-foo 123) SA) (ropen 1 struct:test-foo (rsigma 123 (rsuccess SA))))
  (check-equal? (trie ? SA) (rwild (rsuccess SA)))
  )

(module+ test
  (define (check-matches trie . tests)
    (let walk ((tests tests))
      (match tests
	['() (void)]
	[(list* message expectedstr rest)
	 (define actualset (trie-lookup trie message (tset)))
	 (log-trie-test-debug "~v ==> ~v" message (tset->list actualset))
	 (check-equal? actualset
		       (apply tset (map (lambda (c) (string->symbol (string c)))
					(string->list expectedstr))))
	 (walk rest)])))

  (check-matches
   trie-empty
   (list 'z 'x) ""
   'foo ""
   (list (list 'z (list 'z))) "")

  (let ((t (trie-subtract (trie ? SA) (trie (list 'a) SA))))
    (log-trie-test-debug "~a\n" (trie->pretty-string t #:with-parens #t))
    (check-matches t
                   'b "A"
                   'a "A"
                   (list 'b) "A"
                   (list 'a) ""))

  (define (pretty-print-trie* m)
    (log-trie-test-debug "~a" (trie->pretty-string m #:with-parens #t))
    m)

  (define (pretty-print-trie*/dot m)
    (log-trie-test-debug "\n~a"
                         (trie->dot m
                                    #:transform-success
                                    (lambda (v)
                                      (if (treap? v) (set->list (treap-keys v)) v))))
    m)

  (void (pretty-print-trie*
	 (trie-union (pattern->trie SA (list (list ?) 'x))
		     (pattern->trie SB (list (list ?) 'y)))))

  (void (pretty-print-trie*
	 (trie-union (pattern->trie SA (list (list 'a 'b) 'x))
		     (pattern->trie SB (list (list 'c 'd) 'y)))))

  (void (pretty-print-trie*
	 (trie-union (pattern->trie SA (list (list 'a 'b) 'x))
		     (pattern->trie SB (list (list  ?  ?) 'y)))))

  (check-matches
   (pretty-print-trie*
    (trie-union (pattern->trie SA (list (list 'a 'b) 'x))
		(pattern->trie SB (list (list  ?  ?) 'x))))
   (list 'z 'x) ""
   (list (list 'z 'z) 'x) "B"
   (list (list 'z (list 'z)) 'x) "B"
   (list (list 'a 'b) 'x) "AB")

  (check-matches
   (pretty-print-trie*
    (trie-union (pattern->trie SA (list (list 'a 'b) 'x))
		(pattern->trie SB (list (list ?)     'y))))
   (list 'z 'y) ""
   (list (list 'z 'z) 'y) ""
   (list (list 'z 'z) 'x) ""
   (list (list 'a 'b) 'x) "A")

  (check-matches
   (pretty-print-trie*
    (trie-union (pattern->trie SA (list (list 'a 'b) 'x))
		(pattern->trie SB (list ? 'y))))
   (list 'z 'y) "B"
   (list (list 'z 'z) 'y) "B"
   (list (list 'a 'b) 'x) "A")

  (check-matches
   (pretty-print-trie*
    (trie-union (pattern->trie SA (list 'a 'b))
		(pattern->trie SB (list 'c 'd))))
   (list 'a 'b) "A"
   (list 'c 'd) "B"
   (list 'a 'd) ""
   (list 'c 'b) "")

  (check-matches
   (pretty-print-trie*
    (trie-union (pattern->trie SA (list (list 'a 'b) 'x))
		(pattern->trie SB ?)))
   (list (list 'a 'b) 'x) "AB"
   'p "B"
   (list 'p) "B")

  (check-matches
   (pretty-print-trie*
    (trie-union (pattern->trie SA (list 'a ?))
		(pattern->trie SB (list 'a (list 'b)))))

   (list 'a (list 'b)) "AB"
   (list 'a (list 'b 'b)) "A"
   (list 'a (list 'c 'c)) "A"
   (list 'a (list 'c)) "A"
   (list 'a (list (list))) "A"
   (list 'a (list)) "A"
   (list 'a 'x) "A")

  (check-matches
   (pretty-print-trie*
    (trie-union (trie-union (pattern->trie SA (list 'a ?))
			    (pattern->trie SA (list 'q ?)))
		(pattern->trie SB (list 'a (list 'b)))))
   (list 'a (list 'b)) "AB"
   (list 'q (list 'b)) "A"
   (list 'a 'x) "A"
   (list 'q 'x) "A"
   (list 'a (list)) "A"
   (list 'q (list)) "A"
   (list 'z (list)) "")

  (define (bigdemo)
    (define ps
      (for/list ((c (in-string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
	(define csym (string->symbol (string c)))
	(pattern->trie (tset csym) (list csym ?))))
    (trie-union (trie-union-all ps)
		(pattern->trie S+ (list 'Z (list ? '- ?)))))

  (log-trie-test-debug "Plain bigdemo")

  (void (pretty-print-trie* (bigdemo)))
  (check-matches
   (bigdemo)
   (list 'a '-) "a"
   (list 'Z '-) "Z"
   (list '? '-) ""
   (list 'a (list '- '- '-)) "a"
   (list 'a (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "a"
   (list 'Z) ""
   (list 'Z 'x) "Z"
   (list 'Z (list)) "Z"
   (list 'Z (list '-)) "Z"
   (list 'Z (list '- '-)) "Z"
   (list 'Z (list '- '- '-)) "Z+"
   (list 'Z (list '- '- '- '-)) "Z"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "Z"
   (list 'Z '((()) - -)) "Z+"
   (list '? (list '- '- '-)) "")

  ;; ;; Having switched from pair-based matching to list-based matching,
  ;; ;; it's no longer supported to match with a wildcard in the cdr of a
  ;; ;; pair. Or rather, it is, but it won't work reliably: when the
  ;; ;; value to be matched is a proper list, it will fail to match.
  ;; ;; Consequently: Don't Do That.
  ;; (check-matches (pretty-print-trie* (pattern->trie SA (list* 'a 'b ?)))
  ;; 		 (list 'a 'b 'c 'd 'e 'f) "A"
  ;; 		 (list 'b 'c 'd 'e 'f 'a) ""
  ;; 		 3 "")

  (log-trie-test-debug "bigdemo with trie-intersect 'a -> SA | 'b -> SB")

  (void (pretty-print-trie* (trie-intersect (pattern->trie SA (list 'a))
					    (pattern->trie SB (list 'b)))))

  (log-trie-test-debug "various unions and intersections")

  (let ((r1 (trie-union (pattern->trie SA (list  ? 'b))
			(pattern->trie SA (list  ? 'c))))
	(r2 (trie-union (pattern->trie SB (list 'a  ?))
			(pattern->trie SB (list 'b  ?)))))
    (pretty-print-trie* (trie-union r1 r2))
    (pretty-print-trie* (trie-union r1 r1))
    (pretty-print-trie* (trie-union r2 r2))
    (pretty-print-trie* (trie-intersect r1 r2))
    (pretty-print-trie* (trie-intersect r1 r1))
    (pretty-print-trie* (trie-intersect r2 r2))
    (void))

  (log-trie-test-debug "bigdemo with trie-intersect ('m 'n) -> SX")

  (check-matches
   (pretty-print-trie* (trie-intersect (bigdemo) (pattern->trie SX (list 'm 'n))))
   (list 'm '-) ""
   (list 'm 'n) "mX"
   (list 'x '-) ""
   (list 'x 'n) "")

  (log-trie-test-debug "bigdemo with trie-intersect ('Z ?) -> SX")

  (check-matches
   (pretty-print-trie* (trie-intersect (bigdemo) (pattern->trie SX (list 'Z ?))))
   (list 'a '-) ""
   (list 'Z '-) "XZ"
   (list '? '-) ""
   (list 'a (list '- '- '-)) ""
   (list 'a (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) ""
   (list 'Z) ""
   (list 'Z 'x) "XZ"
   (list 'Z (list)) "XZ"
   (list 'Z (list '-)) "XZ"
   (list 'Z (list '- '-)) "XZ"
   (list 'Z (list '- '- '-)) "XZ+"
   (list 'Z (list '- '- '- '-)) "XZ"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "XZ"
   (list 'Z '((()) - -)) "XZ+"
   (list '? (list '- '- '-)) "")

  (log-trie-test-debug "bigdemo with trie-intersect ('Z ?) -> SX and changed success function")

  (check-matches
   (pretty-print-trie* (trie-intersect (bigdemo) (pattern->trie SX (list 'Z ?))
				       #:combiner (lambda (a b) (success b))))
   (list 'a '-) ""
   (list 'Z '-) "X"
   (list '? '-) ""
   (list 'a (list '- '- '-)) ""
   (list 'a (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) ""
   (list 'Z) ""
   (list 'Z 'x) "X"
   (list 'Z (list)) "X"
   (list 'Z (list '-)) "X"
   (list 'Z (list '- '-)) "X"
   (list 'Z (list '- '- '-)) "X"
   (list 'Z (list '- '- '- '-)) "X"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "X"
   (list 'Z '((()) - -)) "X"
   (list '? (list '- '- '-)) "")

  (log-trie-test-debug "bigdemo with trie-intersect ? -> SX and changed success function")

  (check-matches
   (pretty-print-trie* (trie-intersect (bigdemo) (pattern->trie SX ?)
				       #:combiner (lambda (a b) (success b))))
   (list 'a '-) "X"
   (list 'Z '-) "X"
   (list '? '-) ""
   (list 'a (list '- '- '-)) "X"
   (list 'a (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "X"
   (list 'Z) ""
   (list 'Z 'x) "X"
   (list 'Z (list)) "X"
   (list 'Z (list '-)) "X"
   (list 'Z (list '- '-)) "X"
   (list 'Z (list '- '- '-)) "X"
   (list 'Z (list '- '- '- '-)) "X"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "X"
   (list 'Z '((()) - -)) "X"
   (list '? (list '- '- '-)) "")

  (log-trie-test-debug "subtraction basics")

  (let* ((r1 (pattern->trie SA (list  ? 'b)))
	 (r2 (pattern->trie SB (list 'a  ?)))
	 (r12 (trie-union r1 r2)))
    (log-trie-test-debug "\n-=-=-=-=-=-=-=-=- erase1")
    (pretty-print-trie* r1)
    (pretty-print-trie* r2)
    (pretty-print-trie* r12)
    (pretty-print-trie* (trie-subtract r12 r1))
    (pretty-print-trie* (trie-subtract r12 r2))
    (void))

  (let* ((r1 (trie-union (pattern->trie SA (list 'a ?))
			 (pattern->trie SA (list 'b ?))))
	 (r2 (pattern->trie SB (list 'b ?)))
	 (r12 (trie-union r1 r2)))
    (log-trie-test-debug "\n-=-=-=-=-=-=-=-=- erase2")
    (pretty-print-trie* r12)
    (pretty-print-trie* (trie-subtract r12 r1))
    (pretty-print-trie* (trie-subtract r12 r2))
    (pretty-print-trie* (trie-subtract r12 (trie ? SA)))
    (void))

  )

(module+ test
  (struct a (x) #:prefab)
  (struct b (x) #:transparent)

  (define (intersect a b)
    (trie-intersect (pattern->trie SA a)
		    (pattern->trie SB b)))

  (define EAB (rsuccess (tset 'A 'B)))

  (define (rlist n r) (ropen n 'list r))
  (define (rvector n r) (ropen n 'vector r))

  (define (rsigma* x . xs)
    (let walk ((xs (cons x xs)))
      (match xs
	[(list r) r]
	[(cons e xs1) (rsigma e (walk xs1))])))

  (define-syntax-rule (check-requal? actual expected)
    (check-eq? actual expected))

  (check-requal? (intersect ? ?) (rwild EAB))
  (check-requal? (intersect 'a ?) (rsigma 'a EAB))
  (check-requal? (intersect 123 ?) (rsigma 123 EAB))
  (check-requal? (intersect (list ? 2) (list 1 ?)) (rlist 2 (rsigma* 1 2 EAB)))
  (check-requal? (intersect (list 1 2) ?) (rlist 2 (rsigma* 1 2 EAB)))
  (check-requal? (intersect 1 2) trie-empty)
  (check-requal? (intersect (list 1 2) (list ? 2)) (rlist 2 (rsigma* 1 2 EAB)))
  (check-requal? (intersect (vector 1 2) (vector 1 2)) (rvector 2 (rsigma* 1 2 EAB)))
  (check-requal? (intersect (vector 1 2) (vector 1 2 3)) trie-empty)

  (check-requal? (intersect (a 'a) (a 'b)) trie-empty)
  (check-requal? (intersect (a 'a) (a 'a)) (ropen 1 struct:a (rsigma* 'a EAB)))
  (check-requal? (intersect (a 'a) (a ?)) (ropen 1 struct:a (rsigma* 'a EAB)))
  (check-requal? (intersect (a 'a) ?) (ropen 1 struct:a (rsigma* 'a EAB)))
  (check-requal? (intersect (b 'a) (b 'b)) trie-empty)
  (check-requal? (intersect (b 'a) (b 'a)) (ropen 1 struct:b (rsigma* 'a EAB)))
  (check-requal? (intersect (b 'a) (b ?)) (ropen 1 struct:b (rsigma* 'a EAB)))
  (check-requal? (intersect (b 'a) ?) (ropen 1 struct:b (rsigma* 'a EAB)))

  (check-requal? (intersect (a 'a) (b 'a)) trie-empty)

  (check-exn #px"Cannot match on treaps at present"
	     (lambda ()
	       (define (h a b c d)
		 (treap-insert (treap-insert empty-smap a b) c d))
	       (intersect (h 'a 1 'b ?)
			  (h 'a ? 'b 2))))

  (let ((H rsigma-multi))
    (log-trie-test-debug "Checking that intersection with wildcard is identity-like")
    (define m1 (pretty-print-trie* (trie (list 'a ?) SA
                                         (list 'b ?) SB
                                         (list 'b 'c) SC)))
    (define m2 (pretty-print-trie* (trie ? SD)))
    (define mi (pretty-print-trie* (trie-intersect m1 m2)))
    (check-requal? mi
		   (rlist 2 (H 'a (rwild (rsuccess (tset 'A 'D)))
                               'b (rwild* (H 'c (rsuccess (tset 'B 'C 'D)))
                                          (rsuccess (tset 'B 'D))))))
    (check-requal? (pretty-print-trie* (trie-intersect m1 m2
                                                       #:combiner (lambda (v1 v2) (success v1))))
		   m1))
  )

(module+ test
  (define (trie-match-trie-list m1 m2)
    (match-define (cons s1 s2)
      (trie-match-trie m1 m2
                       #:seed (cons (tset) (tset))
                       #:combiner (lambda (v1 v2 acc)
                                    (cons (tset-union v1 (car acc))
                                          (tset-union v2 (cdr acc))))))
    (list s1 s2))

  (let ((abc (trie (list 'a ?) SA
                   (list 'b ?) SB
                   (list 'c ?) SC))
	(bcd (trie (list 'b ?) SB
                   (list 'c ?) SC
                   (list 'd ?) SD)))
    (check-equal? (trie-match-trie-list abc abc)
		  (list (tset 'A 'B 'C) (tset 'A 'B 'C)))
    (check-equal? (trie-match-trie abc abc
				   #:seed (tset)
				   #:combiner (lambda (v1 v2 a) (tset-union v2 a)))
		  (tset 'A 'B 'C))
    (check-equal? (trie-match-trie-list abc (trie-relabel bcd (lambda (old) (tset #t))))
		  (list (tset 'B 'C) (tset #t)))
    (check-equal? (trie-match-trie-list abc (pattern->trie Sfoo ?))
		  (list (tset 'A 'B 'C) (tset 'foo)))
    (check-equal? (trie-match-trie-list abc (pattern->trie Sfoo (list ? ?)))
		  (list (tset 'A 'B 'C) (tset 'foo)))
    (check-equal? (trie-match-trie-list abc (pattern->trie Sfoo (list ? 'x)))
		  (list (tset 'A 'B 'C) (tset 'foo)))
    (check-equal? (trie-match-trie-list abc (pattern->trie Sfoo (list ? 'x ?)))
		  (list (tset) (tset)))))

(module+ test
  (let* ((Sok (tset 'ok))
         (trie-project (lambda (m spec)
                         (trie-project m spec
                                       #:project-success (lambda (v) (success Sok))
                                       #:combiner (lambda (v1 v2) (success Sok)))))
         (trie-12-14-34 (trie (list 1 2) SA
                              (list 1 4) SB
                              (list 3 4) SC)))
    (check-requal? (trie-project (trie (list 'a 'a) SA
                                       (list 'a 'b) SB)
				 (list 'a (?!)))
                   (trie 'a Sok
                         'b Sok))

    (check-requal? (trie-project (trie (list 'a 'a) SA
                                       (list 'a (vector 'b 'c 'd)) SB)
				 (list 'a (?!)))
                   (trie 'a Sok
                               (vector 'b 'c 'd) Sok))

    (check-requal? (trie-project (trie (list 'a 'a) SA
                                       (list 'a (vector 'b ? 'd)) SB)
				 (list 'a (?!)))
		   (trie 'a Sok
                         (vector 'b ? 'd) Sok))

    (check-equal? (trie-key-set #:take 1
		   (trie-project (trie (list 'a 'a) SA
                                       (list 'a 'b) SB)
				 (list 'a (?!))))
		  (set '(a) '(b)))

    (check-equal? (trie-key-set #:take 1
		   (trie-project (trie (list 'a 'a) SA
                                       (list 'a (vector 'b 'c 'd)) SB)
				 (list 'a (?!))))
		  (set '(a) '(#(b c d))))

    (check-equal? (trie-key-set #:take 1
		   (trie-project (trie (list 'a 'a) SA
                                       (list 'a (vector 'b ? 'd)) SB)
				 (list 'a (?!))))
		  #f)

    (check-equal? (trie-key-set #:take 1
		   (trie-project (trie (list 'a 'a) SA
                                       (list 'a (vector 'b ? 'd)) SB)
				 (list 'a (?! 'a))))
		  (set '(a)))

    (check-requal? (trie-project trie-12-14-34 (list (?!) (?!)))
                   (trie-union-all (list (pattern->trie Sok 1 2)
                                         (pattern->trie Sok 1 4)
                                         (pattern->trie Sok 3 4))))

    (check-requal? (trie-project trie-12-14-34 (?! (list ? ?)))
                   (trie (list 1 2) Sok
                         (list 1 4) Sok
                         (list 3 4) Sok))

    (check-requal? (trie-project trie-12-14-34 (?! (list 1 ?)))
                   (trie (list 1 2) Sok
                         (list 1 4) Sok))

    (check-requal? (trie-project trie-12-14-34 (list (?! 1) (?!)))
                   (trie-union (pattern->trie Sok 1 2)
                               (pattern->trie Sok 1 4)))

    (check-requal? (trie-project trie-12-14-34 (list (?!) (?! 4)))
                   (trie-union (pattern->trie Sok 1 4)
                               (pattern->trie Sok 3 4)))

    (check-equal? (trie-key-set #:take 2
		   (trie-project (trie (list 1 2) SA
                                       (list ? 3) SC
                                       (list 3 4) SB)
				 (list (?!) (?!))))
		  #f)

    (check-equal? (trie-key-set #:take 1
		   (trie-project (trie (list ? 2) SA
                                       (list 1 3) SC
                                       (list 3 4) SB)
				 (list ? (?!))))
		  (set '(2) '(3) '(4)))

    (check-equal? (trie-key-set #:take 2
		   (trie-project (trie (list 1 2) SA
                                       (list 3 4) SB)
				 (list (?!) (?!))))
		  (set '(1 2) '(3 4)))

    (check-requal? (trie-project (trie ? SA
                                       (list 'a) SB)
                                 (?! (list (list ?))))
                   (trie (list (list ?)) Sok)))

  (check-equal? (projection->pattern (list 'a 'b)) (list 'a 'b))
  (check-equal? (projection->pattern (list 'a ?)) (list 'a ?))
  (check-equal? (projection->pattern (list 'a (?!))) (list 'a ?))
  (check-equal? (projection->pattern (list 'a (?! 'b))) (list 'a 'b))
  (check-equal? (projection->pattern (list 'a (?! (vector 'b)))) (list 'a (vector 'b)))
  (check-equal? (projection->pattern (list 'a (?! (vector ? ?)))) (list 'a (vector ? ?)))
  )

(module+ test
  (check-equal? (trie-append (trie 'a SA) (lambda (_v) (trie 'b SB)))
                (rsigma 'a (rsigma 'b (rsuccess SB)))))

(module+ test
  (log-trie-test-debug "Checking that subtraction from union is identity-like")

  (let ((A (trie ? SA))
	(B (trie (list (list (list (list 'foo)))) SB)))
    (check-requal? (pretty-print-trie* (trie-subtract (trie-union A B) B))
		   A))
  (let ((A (trie ? SA))
	(B (trie (list (list (list (list 'foo)))) SB
                 (list (list (list (list 'bar)))) SB)))
    (check-requal? (pretty-print-trie* (trie-subtract (trie-union A B) B))
		   A))
  (let ((A (trie ? SA))
	(B (trie (list (list (list (list 'foo)))) SB
                 (list (list (list (list 'bar)))) SB)))
    (check-requal? (pretty-print-trie* (trie-subtract (trie-union A B) A))
		   B)))

(module+ test
  (log-trie-test-debug "Checking embedded-trie pattern->trie operation")
  (check-requal? (pretty-print-trie*
		  (trie (list 1 (embedded-trie (trie (list 2 3) SB)) 4) SA))
		 (trie (list 1 (list 2 3) 4) SA))

  (check-requal? (pretty-print-trie*
		  (trie (list (embedded-trie (trie (list 1 2) SB))
                              (embedded-trie (trie (list 3 4) SC)))
                        SA))
		 (trie (list (list 1 2) (list 3 4)) SA)))

(module+ test
  (void
   (let ((m (trie ? SA
                  (list ? '- ?) SB)))
     (pretty-print-trie* m)
     (pretty-print-trie*/dot m))))

(module+ test
  (let ()
    (log-trie-test-debug "Biased-intersection test")
    (struct obs (val) #:prefab)
    (let ((object (trie 1 #t
                        2 #t))
          (subject (trie 99 #t
                         (obs ?) #t)))
      (pretty-print-trie* object)
      ;; The default, slow way of computing a biased intersection:
      (define slow-result
        (pretty-print-trie*
         (trie-project (trie-intersect (trie (obs (embedded-trie object)) #t)
                                       subject
                                       #:combiner (lambda (v1 v2) (success #t)))
                       (obs (?!))
                       #:project-success (lambda (v) (success #t))
                       #:combiner (lambda (v1 v2) (success #t)))))
      ;; A hopefully quicker way of doing the same:
      (define fast-result
        (trie-intersect object
                        (trie-step subject (canonical-open-parenthesis 1 struct:obs))
                        #:combiner (lambda (v1 v2) (success #t))))
      (check-requal? slow-result fast-result)
      (check-requal? slow-result object))))

(module+ test
  (require data/enumerate)
  (require data/enumerate/lib)
  (require "random-test.rkt")

  (define limited-symbol/e (fin/e 'x 'y 'z))

  (define-syntax-rule (message-like/e self extra-clauses ...)
    (letrec ((self (delay/e
                    (or/e limited-symbol/e
                          (single/e '())
                          extra-clauses ...
                          (or/e (list/e self)
                                (list/e self self)
                                (list/e self self self)
                                (list/e self self self self))))))
      self))

  (define pattern/e (message-like/e self (single/e ?)))
  (define message/e (message-like/e self))

  (define default-label SA)

  (define full (trie ? default-label))

  (define positive-trie/e
    (pam/e (lambda (pats) (trie-union-all (map (lambda (pat) (trie pat default-label)) pats)))
           #:contract trie?
           (listof/e pattern/e)))

  (define negative-trie/e
    (pam/e (lambda (pats) (foldr (lambda (p t) (trie-subtract t (trie p default-label)))
                                 (trie ? default-label)
                                 pats))
           #:contract trie?
           (listof/e pattern/e)))

  ;; (define complex-trie/e
  ;;   (pam/e (lambda (deltas)
  ;;            (for/fold [(acc (let ((delta (car deltas)))
  ;;                              (match (cdr delta)
  ;;                                ['+ (trie (car delta) default-label)]
  ;;                                ['- (trie-subtract (trie ? default-label) (trie (car delta) default-label))])))]
  ;;                      [(delta (cdr deltas))]
  ;;              (match (cdr delta)
  ;;                ['+ (trie-union acc (trie (car delta) default-label))]
  ;;                ['- (trie-subtract acc (trie (car delta) default-label))])))
  ;;          #:contract trie?
  ;;          (non-empty-listof/e (cons/e pattern/e (fin/e '+ '-)))))

  (define complex-trie/e
    (pam/e (lambda (t1 t2) (ropen 2 'list (trie-append t1 (lambda (_vs) t2))))
           #:contract trie?
           positive-trie/e
           negative-trie/e))

  ;; (check-property (lambda (xs)
  ;;                   (==> (= (length xs) 5)
  ;;                        (andmap (lambda (x) (not (negative? x))) xs)))
  ;;                 (listof/e (nat+/e 0)))

  (define (is-wild? trie)
    (match trie
      [(branch (? treap-empty?) _ (? treap-empty?)) #t]
      [_ #f]))

  (define (contains? trie element)
    (if (trie-lookup trie element #f) #t #f))

  (define (combine-basics name trie-f bool-f)
    (procedure-rename (lambda (trie1 trie2 element)
                        (define combined (trie-f trie1 trie2))
                        (define p (contains? combined element))
                        (define q1 (contains? trie1 element))
                        (define q2 (contains? trie2 element))
                        (define q (bool-f q1 q2))
                        (==> (and (not (is-wild? trie1))
                                  (not (is-wild? trie2))
                                  (trie-non-empty? trie1)
                                  (trie-non-empty? trie2)
                                  (trie-non-empty? combined)
                                  (or p q1 q2))
                             (equal? p q)))
                      name))

  (define union-basics
    (combine-basics 'union-basics trie-union (lambda (a b) (or a b))))
  (define intersect-basics
    (combine-basics 'intersect-basics trie-intersect (lambda (a b) (and a b))))
  (define subtract-basics
    (combine-basics 'subtract-basics trie-subtract (lambda (a b) (and a (not b)))))

  ;; (check-property union-basics positive-trie/e positive-trie/e message/e)
  ;; (check-property intersect-basics positive-trie/e positive-trie/e message/e)
  ;; (check-property subtract-basics positive-trie/e positive-trie/e message/e)

  (parameterize ((random-test:index-limit 10000)
                 (random-test:max-rejected-ratio 25))
    (check-property union-basics complex-trie/e complex-trie/e message/e)
    (check-property intersect-basics complex-trie/e complex-trie/e message/e)
    (check-property subtract-basics complex-trie/e complex-trie/e message/e))

  (define (reconstruct t)
    (match-define `((added ,a ...) (removed ,r ...)) (trie->patterns t))
    (foldr (lambda (p t) (trie-subtract t (trie p default-label)))
           (foldr (lambda (p t) (trie-union t (trie p default-label))) trie-empty a)
           r))

  ;; (newline) (for ((i 15)) (void (time (reconstruct (random-instance positive-trie/e)))))
  ;; (newline) (for ((i 15)) (void (time (reconstruct (random-instance negative-trie/e)))))
  ;; (newline) (for ((i 15)) (void (time (reconstruct (random-instance complex-trie/e)))))

  (check-property #:index-limit 10000 ;; TODO: why do large instances take so long?
                  #:name 'reconstruct
                  (lambda (t) (==> (trie-non-empty? t) (requal? t (reconstruct t))))
                  complex-trie/e)

  (parameterize ((random-test:index-limit 10000)
                 (random-test:max-tests 1000))
    (check-property #:name 'union-symmetric
                    (lambda (t1 t2) (requal? (trie-union t1 t2) (trie-union t2 t1)))
                    complex-trie/e
                    complex-trie/e)
    (check-property #:name 'intersection-symmetric
                    (lambda (t1 t2) (requal? (trie-intersect t1 t2) (trie-intersect t2 t1)))
                    complex-trie/e
                    complex-trie/e)
    (check-property #:name 'empty-is-identity-for-union
                    (lambda (t) (and (requal? t (trie-union t trie-empty))
                                     (requal? t (trie-union trie-empty t))))
                    complex-trie/e)
    (check-property #:name 'full-is-zero-for-union
                    (lambda (t) (and (requal? full (trie-union t full))
                                     (requal? full (trie-union full t))))
                    complex-trie/e)
    (check-property #:name 'empty-is-zero-for-intersection
                    (lambda (t) (and (requal? trie-empty (trie-intersect t trie-empty))
                                     (requal? trie-empty (trie-intersect trie-empty t))))
                    complex-trie/e)
    (check-property #:name 'full-is-identity-for-intersection
                    (lambda (t) (and (requal? t (trie-intersect t full))
                                     (requal? t (trie-intersect full t))))
                    complex-trie/e)
    (check-property #:name 'add-and-remove-unrelated-is-identity
                    (lambda (t)
                      (define other (trie ? SB)) ;; NB. must be different to default-label above.
                      (requal? t (trie-subtract (trie-union t other) other)))
                    complex-trie/e))
  )
