#lang racket/base
;; Implements a nested-word-like automaton mapping sets of messages to sets of other values.
;; A kind of "regular-expression"-keyed multimap.

;; TODO: More global purpose statement.
;; TODO: Some examples showing the idea(s).

;; TODO: rename to trie.rkt or similar.
;; TODO: Ontology

;; TODO: (generally) interpretations for data definitions

(provide ;; Patterns and Projections
         ?
	 wildcard?
	 ?!
	 (struct-out capture)
	 (struct-out embedded-trie)

	 trie? ;; expensive; see implementation
	 trie-empty
	 trie-empty?
	 trie-non-empty?
	 pattern->trie
	 pattern->trie*
	 trie-union
	 trie-intersect
	 empty-tset-guard
	 trie-subtract-combiner
	 trie-subtract
	 trie-lookup
	 trie-match-trie
	 trie-append
	 trie-relabel

	 SOL
	 SOV
	 ILM
	 EOS
	 trie-step
	 success?
	 success-value

	 ;; Projections
	 compile-projection
	 compile-projection*
	 projection->pattern
	 trie-project
	 trie-key-set
	 trie-key-set/single
	 trie-project/set
	 trie-project/set/single
	 project-assertions ;; composition of trie-project/set/single with compile-projection

	 ;; Printing and Serialization
	 pretty-print-trie
	 trie->abstract-graph
	 abstract-graph->dot
	 trie->dot
	 trie->pretty-string
	 trie->jsexpr
	 jsexpr->trie)

(require racket/set)
(require racket/match)
(require (only-in racket/port call-with-output-string with-output-to-string))
(require (only-in racket/class object?))
(require "canonicalize.rkt")
(require "treap.rkt")
(require "tset.rkt")
(require "hash-order.rkt")

(require rackunit)

;; TODO: perhaps avoid the parameters on the fast-path, if they are
;; causing measurable slowdown.
;; TODO: should these even be parameterizable?

;; Constructs a structure type and a singleton instance of it.
(define-syntax-rule (define-singleton-struct singleton-name struct-name print-representation)
  (begin
    (struct struct-name ()
	    #:transparent
	    #:property prop:custom-write
	    (lambda (v port mode) (display print-representation port)))
    (define singleton-name (struct-name))))

;; A Trie is either
;; - #f, indicating no further matches possible
;; - (success Any), representing a successful match (if the end of
;;   the input has been reached)
;; - (Treap (U Sigma Wildcard) Trie), {TODO}
;; TODO::: reimplement to use (ordinary-state (Option Trie) (Treap Sigma Trie)), {TODO}
;; - (wildcard-sequence Trie), {TODO}
;; If, in a treap trie, a wild key is present, it is intended
;; to catch all and ONLY those keys not otherwise present in the
;; table.
;; INVARIANT: if a key is present in a treap, then the
;;            corresponding value MUST NOT be equal to the wildcard
;;            continuation, bearing in mind that
;;             - if the wildcard is absent, it is implicitly #f;
;;             - (key-open?) keys imply rwildseq of the wild continuation
;;             - (key-close?) keys imply runwildseq of the wild continuation
;; INVARIANT: success only appears right at the end. Never in the middle. Never unbalanced parens. TODO
;;            TODO as part of this: figure out whether we can get rid of the seemingly mandatory EOS-success
;;                 pattern that always shows up
(struct success (value) #:transparent)
(struct wildcard-sequence (trie) #:transparent)

;; A Sigma is, roughly, a token in a value being matched. It is one of:
;;  - a struct-type, signifying the start of a struct.
;;  - SOL, signifying the start of a list.
;;  - SOV, signifying the start of a vector.
;;  - ILM, signifying the transition into the cdr position of a pair
;;  - EOS, signifying the notional close-paren at the end of a compound.
;;  - any other value, representing itself.
;; N.B. treaps cannot be Sigmas at present.
(define-singleton-struct SOL start-of-list "<")
(define-singleton-struct SOV start-of-vector "<vector")
(define-singleton-struct ILM improper-list-marker "|")
(define-singleton-struct EOS end-of-sequence ">")

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

;; A CompiledProjection is a (Listof (U Sigma ? SOC EOC)). Compiled
;; projections include start-of-capture and end-of-capture elements.
(define-singleton-struct SOC start-of-capture "{{")
(define-singleton-struct EOC end-of-capture "}}")

;; Any -> Boolean
;; Predicate recognising Tries. Expensive!
(define (trie? x)
  (or (eq? x #f)
      (success? x)
      (wildcard-sequence? x)
      (and (treap? x)
	   (for/and ([v (treap-values x)])
	     (trie? v)))))

;; -> Trie
;; The empty Trie
(define (trie-empty) #f)

;; Trie -> Boolean
;; True iff the argument is the empty trie
(define (trie-empty? r) (not r))

;; Trie -> Boolean
;; True iff the argument is NOT the empty trie
(define (trie-non-empty? r) (not (trie-empty? r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart constructors & accessors
;;
;; Maintain this INVARIANT: A Trie is non-empty iff it contains
;; some keys that map to some Values. Essentially, don't bother
;; prepending tokens to a Trie unless there's some possibility it
;; can map to one or more Values.

;; Trie Trie -> Boolean
;; Exploits canonicalization to replace an expensive equal? check with eq?.
(define (requal? a b)
  (eq? a b))

;; (Option Value) -> Trie
;; If the argument is #f, returns the empty trie; otherwise, a success Trie.
(define (rsuccess v)
  (and v (canonicalize (success v))))

;; Order for sigmas
(define (sigma-order a b)
  (define sta? (struct-type? a))
  (define stb? (struct-type? b))
  (cond
   [(and sta? stb?) (hash-order (struct-type-name a) (struct-type-name b))]
   [sta? '<]
   [stb? '>]
   [else (hash-order a b)]))

;; (Treap (U Sigma Wildcard) Trie)
;; The empty branch-trie
(define empty-smap (treap-empty sigma-order))

;; (U Sigma Wildcard) Trie -> Trie
;; Prepends e to r, if r is non-empty.
(define (rseq e r)
  (if (trie-empty? r)
      r
      (treap-insert empty-smap e r)))

;; [ (U Sigma Wildcard) Trie ] ... -> Trie
(define (rseq-multi . ers)
  (let walk ((ers ers))
    (match ers
      [(list* e r rest) (treap-insert (walk rest) e r)]
      [(list) empty-smap])))

;; Trie -> Trie
;; Prepends the wildcard pseudo-Sigma to r, if r is non-empty.
(define (rwild r)
  (rseq ? r))

;; Trie -> Trie
;; If r is non-empty, returns a trie that consumes input up to and
;; including EOS, then continuing with r.
(define (rwildseq r)
  (if (trie-empty? r) r (canonicalize (wildcard-sequence r))))

;; Trie -> Trie
;; If r is a wildcard-sequence, return the continuation expected after
;; the wilds and EOS. Otherwise, return the empty/failing trie.
(define (runwildseq r)
  (match r
    [(wildcard-sequence k) k]
    [_ #f]))

;; Trie (U Sigma Wildcard) Trie -> Trie
;; r must be a treap trie. Retrieves the continuation after
;; accepting key. If key is absent, returns wild-edge-value, modified
;; depending on key.
(define (rlookup r key wild-edge-value)
  (treap-get r key (lambda ()
		     (cond
		      [(key-open? key) (rwildseq wild-edge-value)]
		      [(key-close? key) (runwildseq wild-edge-value)]
		      [else wild-edge-value]))))

;; (Option (Treap (U Sigma Wildcard) Trie)) Sigma Trie -> Trie
;; Updates (installs or removes) a continuation in the Trie r. r
;; must be either #f or a treap trie. key MUST NOT be ?.
;; Preserves invariant that a key is never added if its continuation
;; is the same as the wildcard's continuation (which is implicitly #f
;; if absent, of course).
(define (rupdate r0 key k)
  (when (eq? key ?) (error 'rupdate "Internal error: supplied wildcard as key"))
  (define r (or r0 empty-smap))
  (empty-smap-guard
   (let ((old-wild (treap-get r ? (lambda () #f))))
     (if (cond [(key-open? key)
		(if (wildcard-sequence? k)
		    (requal? (wildcard-sequence-trie k) old-wild)
		    (trie-empty? k))]
	       [(key-close? key)
		(if (wildcard-sequence? old-wild)
		    (requal? (wildcard-sequence-trie old-wild) k)
		    (trie-empty? k))]
	       [else
		(requal? k old-wild)])
	 (treap-delete r key)
	 (treap-insert r key k)))))

;; Treap -> Trie
;; If the argument is empty, returns the canonical empty trie;
;; otherwise, returns the argument.
(define (empty-smap-guard h)
  (and (positive? (treap-size h)) h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern compilation

;; Value (Listof Pattern) -> Trie
;; Compiles a sequence of patterns into a trie that accepts input
;; matching that sequence, yielding v.
(define (pattern->trie* v ps0)
  ;; Pattern Trie -> Trie
  ;; acc is the continuation-trie for the trie created from ps.
  (define (walk-pair-chain ps acc)
    (match ps
      ['() (rseq EOS acc)]
      [(cons p ps) (walk p (walk-pair-chain ps acc))]
      [other (rseq ILM (walk other (rseq EOS acc)))]))

  ;; Pattern Trie -> Trie
  ;; acc is the continuation-trie for the trie created from p.
  (define (walk p acc)
    (match p
      [(capture sub) (error 'pattern->trie* "Embedded capture in one of the patterns ~v" ps0)]
      [(== ?) (rwild acc)]
      [(cons p1 p2) (rseq SOL (walk p1 (walk-pair-chain p2 acc)))]
      [(? vector? v) (rseq SOV (vector-foldr walk (rseq EOS acc) v))]
      [(embedded-trie m) (trie-append m (lambda (_mv) acc))]
      ;; TODO: consider options for treating treaps as compounds
      ;; rather than (useless) atoms
      [(? treap?) (error 'pattern->trie "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (rseq (struct->struct-type p)
	     (walk-pair-chain (cdr (vector->list (struct->vector p)))
			      acc))]
      [other (rseq (canonicalize other) acc)]))

  (walk-pair-chain ps0 (rsuccess v)))

;; Value Pattern* -> Trie
;; Convenience form of pattern->trie*.
(define (pattern->trie v . ps)
  (pattern->trie* v ps))

;; Structure -> StructType
;; Errors when given any struct that isn't completely transparent/prefab.
(define (struct->struct-type p)
  (define-values (t skipped?) (struct-info p))
  (when skipped? (error 'struct->struct-type "Cannot reflect on struct instance ~v" p))
  t)

;; Any -> Boolean
;; Racket objects are structures, so we reject them explicitly for
;; now, leaving them opaque to unification.
(define (non-object-struct? x)
  (and (struct? x)
       (not (object? x))))

;; (A B -> B) B (Vectorof A) -> B
(define (vector-foldr kons knil v)
  (for/fold [(acc knil)] [(elem (in-vector v (- (vector-length v) 1) -1 -1))]
    (kons elem acc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trie combinators

(define (default-short v r)
  (error 'default-short "Asymmetric tries; value ~v, trie ~v" v r))

;; Trie Trie -> Trie
;; Computes the union of the multimaps passed in.
(define (trie-union re1 re2 #:combiner [combiner tset-union])
  (trie-recurse re1
		re2
		combiner
		values
		values
		values
		values
		default-short
		default-short))

;; (A B -> C) -> A B -> B A -> C
(define ((flip f) a b) (f b a))

;; Trie Trie -> Trie
;; Computes the intersection of the multimaps passed in.
(define (trie-intersect re1 re2
			#:combiner [combiner tset-union]
			#:left-short [left-short default-short]
			#:right-short [right-short default-short])
  (trie-recurse re1
		re2
		combiner
		(lambda (r) #f)
		(lambda (r) #f)
		(lambda (h) #f)
		(lambda (h) #f)
		left-short
		right-short))

(define (empty-tset-guard s)
  (if (tset-empty? s) #f s))

(define (trie-subtract-combiner s1 s2)
  (empty-tset-guard (tset-subtract s1 s2)))

;; Trie Trie -> Trie
;; Removes re2's mappings from re1.
;; The combine-successes function should return #f to signal "no remaining success values".
(define (trie-subtract re1 re2 #:combiner [combiner trie-subtract-combiner])
  (trie-recurse re1
		re2
		combiner
		(lambda (r) #f)
		values
		(lambda (h) #f)
		values
		default-short
		default-short))

(define (trie-recurse re1 re2 vf left-false right-false right-base left-base left-short right-short)
  (let f ((re1 re1) (re2 re2))
    (match* (re1 re2)
      [(#f r) (left-false r)]
      [(r #f) (right-false r)]

      [((? treap? h1) (? treap? h2))
       (fold-over-keys h1 h2 f (left-base h1) (right-base h2))]

      [((wildcard-sequence r1) (wildcard-sequence r2)) (rwildseq (f r1 r2))]
      [((wildcard-sequence r1) r2) (f (expand-wildseq r1) r2)]
      [(r1 (wildcard-sequence r2)) (f r1 (expand-wildseq r2))]

      [((success v1) (success v2)) (rsuccess (vf v1 v2))]
      [((success v) r) (left-short v r)]
      [(r (success v)) (right-short v r)])))

(define (fold-over-keys h1 h2 f left-base right-base)
  (define w1 (rlookup h1 ? #f))
  (define w2 (rlookup h2 ? #f))
  (collapse-wildcard-sequences
   (cond
    [(and w1 w2)
     (for/fold [(acc (rwild (f w1 w2)))]
	 [(key (set-remove (set-union (treap-keys h1) (treap-keys h2)) ?))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))]
    [w1
     (for/fold [(acc left-base)] [(key (treap-keys h2))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))]
    [w2
     (for/fold [(acc right-base)] [(key (treap-keys h1))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))]
    [(< (treap-size h1) (treap-size h2))
     (for/fold [(acc right-base)] [(key (treap-keys h1))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))]
    [else
     (for/fold [(acc left-base)] [(key (treap-keys h2))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))])))

;; Trie -> Trie
;; When a trie contains only entries for (EOS -> m') and (★ ->
;; (wildcard-sequence m')), it is equivalent to (wildcard-sequence m')
;; itself. This is the inverse of expand-wildseq.
;;
;; In addition, we rewrite (★ -> (wildcard-sequence m')) to
;; (wildcard-sequence m'), since trie-lookup will fall back to
;; ★ if EOS is missing, and rlookup adjusts appropriately.
(define (collapse-wildcard-sequences m)
  (if (treap? m)
      (case (treap-size m)
	[(2)
	 (if (and (treap-has-key? m ?)
		  (treap-has-key? m EOS))
	     (let ((w (treap-get m ?))
		   (k (treap-get m EOS)))
	       (if (and (wildcard-sequence? w)
			(requal? (wildcard-sequence-trie w) k))
		   w
		   m))
	     m)]
	[(1)
	 (if (treap-has-key? m ?)
	     (let ((w (treap-get m ?)))
	       (if (wildcard-sequence? w)
		   w
		   m))
	     m)]
	[else m])
      m))

;; Sigma -> Boolean
;; True iff k represents the start of a compound datum.
(define (key-open? k)
  (or (eq? k SOL)
      (eq? k SOV)
      (struct-type? k)))

;; Sigma -> Boolean
;; True iff k represents the end of a compound datum.
(define (key-close? k)
  (eq? k EOS))

;; Trie -> Trie
;; Unrolls the implicit recursion in a wildcard-sequence.
(define (expand-wildseq r)
  (treap-insert (treap-insert empty-smap ? (rwildseq r)) EOS r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching single keys into a multimap

;; (Listof Sigma) -> (Listof Sigma)
;; Hackish support for improper lists. TODO: revisit
;; Converts an improper list into a proper one with ILM in the penultimate position.
(define (transform-list-value xs)
  (match xs
    ['() '()]
    [(cons x xs) (cons x (transform-list-value xs))]
    [other (cons ILM (cons other '()))]))

;; Trie InputValue [Value] -> Value
;; Converts the nested structure v on-the-fly into a sequence of
;; Sigmas and runs them through the Trie r. If v leads to a success
;; Trie, returns the values contained in the success Trie;
;; otherwise, returns failure-result.
(define (trie-lookup r v failure-result)
  (let walk ((vs (list v)) (stack '(())) (r r))
    (match r
      [#f failure-result]
      [(wildcard-sequence k)
       (match stack
	 ['() failure-result]
	 [(cons rest stack1) (walk rest stack1 k)])]
      [(success result)
       (if (and (null? vs)
		(null? stack))
	   result
	   failure-result)]
      [(? treap?)
       (define (get key) (treap-get r key (lambda () #f)))
       (match vs
	 ['()
	  (match stack
	    ['() failure-result]
	    [(cons rest stack1)
	     (walk rest stack1 (rlookup r EOS (get ?)))])]
	 [(cons (== ?) rest)
	  (error 'trie-lookup "Cannot match wildcard as a value")]
	 [(cons (cons v1 v2) rest)
	  (match (get SOL)
	    [#f (walk rest stack (get ?))]
	    [k (walk (cons v1 (transform-list-value v2)) (cons rest stack) k)])]
	 [(cons (vector vv ...) rest)
	  (match (get SOV)
	    [#f (walk rest stack (get ?))]
	    [k (walk vv (cons rest stack) k)])]
	 [(cons (? non-object-struct? s) rest)
	  (match (get (struct->struct-type s))
	    [#f (walk rest stack (get ?))]
	    [k (walk (cdr (vector->list (struct->vector s))) (cons rest stack) k)])]
	 [(cons v rest)
	  (walk rest stack (rlookup r (canonicalize v) (get ?)))])])))

;; Trie Trie -> Value
;;
;; Similar to trie-lookup, but instead of a single key,
;; accepts a Trie serving as *multiple* simultaneously-examined
;; keys. Returns the union of all successful values reached by the
;; probe.
(define (trie-match-trie re1 re2
			 #:seed seed
			 #:combiner [combiner (lambda (v1 v2 a)
						(cons (tset-union (car a) v1)
						      (tset-union (cdr a) v2)))]
			 #:left-short [left-short (lambda (v r acc) acc)]
			 #:right-short [right-short (lambda (v r acc) acc)])
  (let walk ((re1 re1) (re2 re2) (acc seed))
    (match* (re1 re2)
      [(#f _) acc]
      [(_ #f) acc]

      [((? treap? h1) (? treap? h2))
       (define w1 (rlookup h1 ? #f))
       (define w2 (rlookup h2 ? #f))
       (define r (walk w1 w2 acc))
       (for/fold [(r r)]
	   [(key (cond
		  [(and w1 w2) (set-remove (set-union (treap-keys h1) (treap-keys h2)) ?)]
		  [w1 (treap-keys h2)]
		  [w2 (treap-keys h1)]
		  [(< (treap-size h1) (treap-size h2)) (treap-keys h1)]
		  [else (treap-keys h2)]))]
	 (walk (rlookup h1 key w1) (rlookup h2 key w2) r))]

      [((wildcard-sequence r1) (wildcard-sequence r2)) (walk r1 r2 acc)]
      [((wildcard-sequence r1) r2) (walk (expand-wildseq r1) r2 acc)]
      [(r1 (wildcard-sequence r2)) (walk r1 (expand-wildseq r2) acc)]

      [((success v1) (success v2)) (combiner v1 v2 acc)]
      [((success v) r) (left-short v r acc)]
      [(r (success v)) (right-short v r acc)])))

;; Trie × (Value → Trie) → Trie
;; Since Tries accept *sequences* of input values, this appends two
;; tries into a single trie that accepts their concatenation.
;; Because tries map inputs to values, the second trie is
;; expressed as a function from success-values from the first trie
;; to a second trie.
(define (trie-append m0 m-tail-fn)
  (let walk ((m m0))
    (match m
      [#f #f]
      [(success v) (error 'trie-append "Ill-formed trie: ~v" m0)]
      [(wildcard-sequence m1) (rwildseq (walk m1))]
      [(? treap?) (for/fold [(acc (rwild (walk (rlookup m ? #f))))]
		      [(kv (treap-to-alist m)) #:when (not (eq? (car kv) ?))]
		    (match-define (cons k v) kv)
		    (if (and (key-close? k) (success? v))
			(trie-union acc (m-tail-fn (success-value v))
				    #:combiner (lambda (v1 v2)
						 (error 'trie-append
							"Conflicting success-values ~v/~v"
							v1
							v2)))
			(rupdate acc k (walk v))))])))

;; Trie (Value -> (Option Value)) -> Trie
;; Maps f over success values in m.
(define (trie-relabel m f)
  (let walk ((m m))
    (match m
      [#f #f]
      [(success v) (rsuccess (f v))]
      [(wildcard-sequence m1) (rwildseq (walk m1))]
      [(? treap?) (for/fold [(acc (rwild (walk (rlookup m ? #f))))]
		      [(kv (treap-to-alist m)) #:when (not (eq? (car kv) ?))]
		    (rupdate acc (car kv) (walk (cdr kv))))])))

;; Trie Sigma -> Trie
(define (trie-step m s)
  (match m
    [#f #f]
    [(wildcard-sequence k) (if (key-close? s) k m)]
    [(success _) #f]
    [(? treap? h) (rlookup h s (treap-get h ? (lambda () #f)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projection

;; (Listof Projection) -> CompiledProjection
;; Compiles a sequence of projections into a single CompiledProjection
;; for use with trie-project.
(define (compile-projection* ps0)
  (define (walk-pair-chain ps acc)
    (match ps
      ['() (cons EOS acc)]
      [(cons p ps) (walk p (walk-pair-chain ps acc))]
      [other (cons ILM (walk other (cons EOS acc)))]))

  (define (walk p acc)
    (match p
      [(capture sub) (cons SOC (walk sub (cons EOC acc)))] ;; TODO: enforce non-nesting here
      [(== ?) (cons ? acc)]
      [(cons p1 p2) (cons SOL (walk p1 (walk-pair-chain p2 acc)))]
      [(? vector? v) (cons SOV (vector-foldr walk (cons EOS acc) v))]
      [(embedded-trie m) (error 'compile-projection "Cannot embed trie in projection")]
      ;; TODO: consider options for treating treaps as compounds rather than (useless) atoms
      [(? treap?) (error 'compile-projection "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (cons (struct->struct-type p)
	     (walk-pair-chain (cdr (vector->list (struct->vector p)))
			      acc))]
      [other (cons (canonicalize other) acc)]))

  (walk-pair-chain ps0 '()))

;; Projection* -> CompiledProjection
;; Convenience form of compile-projection*.
(define (compile-projection . ps)
  (compile-projection* ps))

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

;; Trie × CompiledProjection -> Trie
;; The result matches a sequence of inputs of length equal to the number of captures.
;; The project-success function should return #f to signal "no success values".
(define trie-project
  (let ()
    (define (general-balanced add-wildseq add-wild add-edge m k)
      (let walk ((m m) (k k))
	(match m
	  [(wildcard-sequence mk) (add-wildseq (k mk))]
	  [(? treap?)
	   (for/fold [(acc (add-wild (walk (rlookup m ? #f) k)))]
	       [(key-mk (treap-to-alist m)) #:when (not (eq? (car key-mk) ?))]
	     (match-define (cons key mk) key-mk)
	     (add-edge acc key (cond
				[(key-open? key) (walk mk (lambda (mk) (walk mk k)))]
				[(key-close? key) (k mk)]
				[else (walk mk k)])))]
	  [_ (trie-empty)])))

    (define (general-match add-wild add-edge add-sigma balanced m spec ps drop-match take-match)
      (let walk ((m m) (spec spec))
	(match spec
	  ['()
	   (match m
	     [(success v) (rseq EOS (rsuccess (ps v)))]
	     [_ (trie-empty)])]

	  [(cons (== EOC) k) (drop-match m k)]
	  [(cons (== SOC) k) (take-match m k)]

	  [(cons (== ?) k)
	   (match m
	     [(wildcard-sequence _) (add-wild (walk m k))]
	     [(? treap?)
	      (for/fold [(acc (add-wild (walk (rlookup m ? #f) k)))]
		  [(key-mk (treap-to-alist m)) #:when (not (eq? (car key-mk) ?))]
		(match-define (cons key mk) key-mk)
		(add-edge acc key (cond
				   [(key-open? key) (balanced mk (lambda (mk) (walk mk k)))]
				   [(key-close? key) #f]
				   [else (walk mk k)])))]
	     [_ (trie-empty)])]

	  [(cons sigma k)
	   (add-sigma sigma
		      (match m
			[(wildcard-sequence mk)
			 (cond
			  [(key-open? sigma) (walk (rwildseq m) k)]
			  [(key-close? sigma) (walk mk k)]
			  [else (walk m k)])]
			[(? treap?) (walk (rlookup m sigma (rlookup m ? #f)) k)]
			[_ (trie-empty)]))])))

    (lambda (m spec
	       #:project-success [project-success values]
	       #:combiner [combiner tset-union])
      (define (drop-match m spec) (general-match values drop-edge drop-sigma drop-bal m spec
                                                 project-success drop-match take-match))
      (define (take-match m spec) (general-match rwild  rupdate   rseq       take-bal m spec
                                                 project-success drop-match take-match))
      (define (drop-bal m k) (general-balanced values   values drop-edge m k))
      (define (take-bal m k) (general-balanced rwildseq rwild  rupdate   m k))
      (define (drop-edge acc key k) (trie-union acc k #:combiner combiner))
      (define (drop-sigma sigma k) k)
      (drop-match m spec))))

;; (Listof Sigma) -> (Listof Sigma)
;; Hackish support for improper lists. TODO: revisit
;; Undoes the transformation of transform-list-value, converting
;; ILM-marked proper lists back into improper ones.
(define (untransform-list-value vs)
  (match vs
    ['() '()]
    [(cons (== ILM) (cons v '())) v]
    [(cons (== ILM) _) (error 'untransform-list-value "Illegal use of ILM" vs)]
    [(cons v vs) (cons v (untransform-list-value vs))]))

;; Trie → (Option (Setof (Listof Value)))
;; Extracts the "keys" in its argument multimap m, representing input
;; sequences as lists. Multiplies out unions. Returns #f if any
;; dimension of m is infinite.
(define trie-key-set
  (let ()
    ;; Trie (Value Trie -> (Setof Value)) -> (Option (Setof Value))
    ;; Calls k with each possible atomic value at this trie
    ;; position, and accumulates the results.
    (define (walk m k)
      (match m
	[(wildcard-sequence _) #f]
	[(? treap?)
	 (and (not (treap-has-key? m ?))
	      (for/fold [(acc (set))] [(key-mk (treap-to-alist m))]
		(match-define (cons key mk) key-mk)
		(maybe-union
		 acc
		 (cond
		  [(key-open? key)
		   (walk-seq mk (lambda (vss vsk)
				  (for/fold [(acc (set))] [(vs (in-set vss))]
				    (maybe-union acc
						 (k (transform-seqs vs key) vsk)))))]
		  [(key-close? key)
		   (error 'trie-key-set "Internal error: unexpected key-close")]
		  [else
		   (k key mk)]))))]
	[_ (set)]))

    ;; Trie (Value Trie -> (Setof (Listof Value))) -> (Option (Setof (Listof Value)))
    ;; Calls k with each possible sequence of atomic values at this
    ;; trie position, and accumulates the results.
    (define (walk-seq m k)
      (match m
	[(wildcard-sequence _) #f]
	[(? treap?)
	 (and (not (treap-has-key? m ?))
	      (for/fold [(acc (set))] [(key-mk (treap-to-alist m))]
		(match-define (cons key mk) key-mk)
		(maybe-union acc (cond
				  [(key-close? key) (k (set '()) mk)]
				  [else (walk (rseq key mk)
					      (lambda (v vk)
						(walk-seq vk (lambda (vss vsk)
							       (k (for/set [(vs (in-set vss))]
								    (cons v vs))
								  vsk)))))]))))]
	[_ (k (set) #f)]))

    ;; (Listof Value) Sigma -> Value
    (define (transform-seqs vs opener)
      (cond
       [(eq? opener SOL) (untransform-list-value vs)]
       [(eq? opener SOV) (list->vector vs)]
       [(struct-type? opener) (apply (struct-type-make-constructor opener) vs)]))

    ;; (Option (Setof A)) (Option (Setof A)) -> (Option (Setof A))
    (define (maybe-union s1 s2) (and s1 s2 (set-union s1 s2)))

    (lambda (m)
      (walk-seq m (lambda (vss vsk) vss)))))

;; Trie → (Option (Setof Value))
;; As trie-key-set, but extracts just the first captured subvalue.
(define (trie-key-set/single m)
  (define vss (trie-key-set m))
  (and vss (for/set [(vs (in-set vss))] (car vs))))

;; Convenience forms for the common operation of projecting a Trie
;; followed by converting the result to a Racket set (possibly
;; containing just the first captured subvalue).
(define-syntax-rule (trie-project/set arg ...)
  (trie-key-set (trie-project arg ...)))
(define-syntax-rule (trie-project/set/single arg ...)
  (trie-key-set/single (trie-project arg ...)))

;; Ultra-convenience form.
(define (project-assertions m . ps)
  (trie-project/set/single m (compile-projection* ps)))

;; struct-type -> Symbol
;; Extract just the name of the given struct-type.
(define (struct-type-name st)
  (define-values (name x2 x3 x4 x5 x6 x7 x8) (struct-type-info st))
  name)

;; Trie [OutputPort] [#:indent Nat] -> Void
;; Pretty-prints the given trie on the given port, with
;; second-and-subsequent lines indented by the given amount.
(define (pretty-print-trie m [port (current-output-port)] #:indent [initial-indent 0])
  (define (d x) (display x port))
  (define (walk i m)
    (match m
      [#f
       (d "::: nothing")]
      [(wildcard-sequence k)
       (d " ...>")
       (walk (+ i 5) k)]
      [(success vs)
       (d "{")
       (d (if (tset? vs) (cons 'tset (tset->list vs)) vs))
       (d "}")]
      [(? treap? h)
       (if (zero? (treap-size h))
	   (d " ::: empty treap!")
	   (for/fold [(need-sep? #f)] [(key-k (treap-to-alist h))]
	     (match-define (cons key k) key-k)
	     (when need-sep?
	       (newline port)
	       (d (make-string i #\space)))
	     (d " ")
	     (define keystr (call-with-output-string
			     (lambda (p)
			       (cond
				[(struct-type? key)
				 (display "<s:" p)
				 (display (struct-type-name key) p)]
				[else
				 (write key p)]))))
	     (d keystr)
	     (walk (+ i 1 (string-length keystr)) k)
	     #t))]))
  (walk initial-indent m)
  (newline port)
  m)

(define (trie->pretty-string m #:indent [initial-indent 0])
  (with-output-to-string (lambda () (pretty-print-trie m #:indent initial-indent))))

(define (trie->abstract-graph m)
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
                     [#f (list 'fail)]
                     [(wildcard-sequence _) (list 'tail)]
                     [(success v) (list 'ok v)]
                     [(? treap?) (list 'branch)]))
                 (define source-id (gensym 'i))
                 (define entry (cons source-id node-info))
                 (set! nodes (hash-set nodes m entry))
                 (match m
                   [#f (void)]
                   [(wildcard-sequence k) (add-edge! source-id #f k)]
                   [(success _) (void)]
                   [(? treap? h) (treap-fold h
                                             (lambda (seed k v) (add-edge! source-id k v))
                                             (void))])
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

(define (trie->dot m)
  (abstract-graph->dot (trie->abstract-graph m)))

;; Trie (Value -> JSExpr) -> JSExpr
;; Serializes a trie to a JSON expression.
(define (trie->jsexpr m success->jsexpr)
  (let walk ((m m))
    (match m
      [#f '()]
      [(success v) (list "" (success->jsexpr v))]
      [(wildcard-sequence m1) (list "...)" (walk m1))]
      [(? treap?)
       (for/list [(kv (treap-to-alist m))]
	 (match-define (cons k v) kv)
	 (list (match k
		 [(== ?) (list "__")]
		 [(== SOL) (list "(")]
		 [(== SOV) (list "#(")]
		 [(== EOS) (list ")")]
		 [(? struct-type? t)
		  (list (string-append (symbol->string (struct-type-name t)) "("))]
		 [else k])
	       (walk v)))])))

;; String -> String
;; Undoes the encoding of struct-type names used in the JSON serialization of Tries.
(define (deserialize-struct-type-name stn)
  (define expected-paren-pos (- (string-length stn) 1))
  (and (char=? (string-ref stn expected-paren-pos) #\()
       (substring stn 0 expected-paren-pos)))

;; JSExpr (JSExpr -> Value) [String -> (Option struct-type)] -> Trie
;; Deserializes a trie from a JSON expression.
(define (jsexpr->trie j jsexpr->success [struct-type-name->struct-type (lambda () #f)])
  (let walk ((j j))
    (match j
      ['() #f]
      [(list "" vj) (rsuccess (jsexpr->success vj))]
      [(list "...)" j1) (rwildseq (walk j1))]
      [(list (list kjs vjs) ...)
       (for/fold [(acc empty-smap)]
	   [(kj kjs) (vj vjs)]
	 (treap-insert acc
		       (match kj
			 [(list "__") ?]
			 [(list "(") SOL]
			 [(list "#(") SOV]
			 [(list ")") EOS]
			 [(list (? string? s))
			  (match (deserialize-struct-type-name s)
			    [#f (error 'jsexpr->trie
				       "Illegal open-parenthesis mark ~v"
				       kj)]
			    [tn (match (struct-type-name->struct-type tn)
				  [#f (error 'jsexpr->trie
					     "Unexpected struct type ~v"
					     tn)]
				  [t t])])]
			 [other other])
		       (walk vj)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require racket/pretty)

  (define tset datum-tset)

  (define SA (tset 'A))
  (define SB (tset 'B))
  (define SC (tset 'C))
  (define SD (tset 'D))
  (define Sfoo (tset 'foo))
  (define S+ (tset '+))
  (define SX (tset 'X))
  (define (E v) (rseq EOS (rsuccess v)))
  (check-equal? (pattern->trie SA 123) (rseq 123 (E SA)))
  (check-equal? (pattern->trie SA (cons 1 2))
		(rseq SOL (rseq 1 (rseq ILM (rseq 2 (rseq EOS (E SA)))))))
  (check-equal? (pattern->trie SA (cons ? 2))
		(rseq SOL (rseq ? (rseq ILM (rseq 2 (rseq EOS (E SA)))))))
  (check-equal? (pattern->trie SA (list 1 2)) (rseq SOL (rseq 1 (rseq 2 (rseq EOS (E SA))))))
  (check-equal? (pattern->trie SA (list ? 2)) (rseq SOL (rseq ? (rseq 2 (rseq EOS (E SA))))))
  (check-equal? (pattern->trie SA SOL) (rseq struct:start-of-list (rseq EOS (E SA))))
  (check-equal? (pattern->trie SA ?) (rseq ? (E SA)))
  )

(module+ test
  (define (check-matches trie . tests)
    (let walk ((tests tests))
      (match tests
	['() (void)]
	[(list* message expectedstr rest)
	 (define actualset (trie-lookup trie message (tset)))
	 (printf "~v ==> ~v\n" message actualset)
	 (check-equal? actualset
		       (apply tset (map (lambda (c) (string->symbol (string c)))
					(string->list expectedstr))))
	 (walk rest)])))

  (check-matches
   #f
   (list 'z 'x) ""
   'foo ""
   (list (list 'z (list 'z))) "")

  (define (pretty-print-trie* m)
    (newline)
    (pretty-print-trie m)
    (flush-output)
    m)

  (define (pretty-print-trie*/dot m)
    (newline)
    (display (trie->dot (trie-relabel m (lambda (v)
					  (if (treap? v)
					      (map car (treap-to-alist v))
					      v)))))
    (flush-output)
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

  (void (pretty-print-trie* (trie-union (pattern->trie SA (list (list 'a 'b) 'x))
					;; Note: this is a largely nonsense trie,
					;; since it expects no input at all
					(rseq EOS (rsuccess (tset 'B))))))

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
    (trie-union (foldr trie-union (trie-empty) ps)
		(pattern->trie S+ (list 'Z (list ? '- ?)))))

  (newline)
  (printf "Plain bigdemo\n")

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

  (newline)
  (printf "bigdemo with trie-intersect 'a -> SA | 'b -> SB\n")

  (void (pretty-print-trie* (trie-intersect (pattern->trie SA (list 'a))
					    (pattern->trie SB (list 'b)))))

  (newline)
  (printf "various unions and intersections\n")

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

  (newline)
  (printf "bigdemo with trie-intersect ('m 'n) -> SX\n")

  (check-matches
   (pretty-print-trie* (trie-intersect (bigdemo) (pattern->trie SX (list 'm 'n))))
   (list 'm '-) ""
   (list 'm 'n) "mX"
   (list 'x '-) ""
   (list 'x 'n) "")

  (newline)
  (printf "bigdemo with trie-intersect ('Z ?) -> SX\n")

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

  (newline)
  (printf "bigdemo with trie-intersect ('Z ?) -> SX and changed success function\n")

  (check-matches
   (pretty-print-trie* (trie-intersect (bigdemo) (pattern->trie SX (list 'Z ?))
				       #:combiner (lambda (a b) b)))
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

  (newline)
  (printf "bigdemo with trie-intersect ? -> SX and changed success function\n")

  (check-matches
   (pretty-print-trie* (trie-intersect (bigdemo) (pattern->trie SX ?)
				       #:combiner (lambda (a b) b)))
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

  (newline)
  (printf "subtraction basics\n")

  (let* ((r1 (pattern->trie SA (list  ? 'b)))
	 (r2 (pattern->trie SB (list 'a  ?)))
	 (r12 (trie-union r1 r2)))
    (printf "\n-=-=-=-=-=-=-=-=- erase1\n")
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
    (printf "\n-=-=-=-=-=-=-=-=- erase2\n")
    (pretty-print-trie* r12)
    (pretty-print-trie* (trie-subtract r12 r1))
    (pretty-print-trie* (trie-subtract r12 r2))
    (pretty-print-trie* (trie-subtract r12 (pattern->trie SA ?)))
    (void))

  )

(module+ test
  (struct a (x) #:prefab)
  (struct b (x) #:transparent)

  (define (intersect a b)
    (trie-intersect (pattern->trie SA a)
		    (pattern->trie SB b)))

  (define EAB (E (tset 'A 'B)))

  (define (rseq* x . xs)
    (let walk ((xs (cons x xs)))
      (match xs
	[(list r) r]
	[(cons e xs1) (rseq e (walk xs1))])))

  (define-syntax-rule (check-requal? actual expected)
    (check-eq? actual expected))

  (check-requal? (intersect ? ?) (rwild EAB))
  (check-requal? (intersect 'a ?) (rseq 'a EAB))
  (check-requal? (intersect 123 ?) (rseq 123 EAB))
  (check-requal? (intersect (cons ? 2) (cons 1 ?)) (rseq* SOL 1 ILM 2 EOS EAB))
  (check-requal? (intersect (list ? 2) (list 1 ?)) (rseq* SOL 1 2 EOS EAB))
  (check-requal? (intersect (cons 1 2) ?) (rseq* SOL 1 ILM 2 EOS EAB))
  (check-requal? (intersect (list 1 2) ?) (rseq* SOL 1 2 EOS EAB))
  (check-requal? (intersect 1 2) #f)
  (check-requal? (intersect (cons 1 2) (cons ? 2)) (rseq* SOL 1 ILM 2 EOS EAB))
  (check-requal? (intersect (list 1 2) (list ? 2)) (rseq* SOL 1 2 EOS EAB))
  (check-requal? (intersect (cons 1 2) (cons 3 2)) #f)
  (check-requal? (intersect (cons 1 2) (cons 1 3)) #f)
  (check-requal? (intersect (vector 1 2) (vector 1 2)) (rseq* SOV 1 2 EOS EAB))
  (check-requal? (intersect (vector 1 2) (vector 1 2 3)) #f)

  (check-requal? (intersect (a 'a) (a 'b)) #f)
  (check-requal? (intersect (a 'a) (a 'a)) (rseq* struct:a 'a EOS EAB))
  (check-requal? (intersect (a 'a) (a ?)) (rseq* struct:a 'a EOS EAB))
  (check-requal? (intersect (a 'a) ?) (rseq* struct:a 'a EOS EAB))
  (check-requal? (intersect (b 'a) (b 'b)) #f)
  (check-requal? (intersect (b 'a) (b 'a)) (rseq* struct:b 'a EOS EAB))
  (check-requal? (intersect (b 'a) (b ?)) (rseq* struct:b 'a EOS EAB))
  (check-requal? (intersect (b 'a) ?) (rseq* struct:b 'a EOS EAB))

  (check-requal? (intersect (a 'a) (b 'a)) #f)

  (check-exn #px"Cannot match on treaps at present"
	     (lambda ()
	       (define (h a b c d)
		 (treap-insert (treap-insert empty-smap a b) c d))
	       (intersect (h 'a 1 'b ?)
			  (h 'a ? 'b 2))))

  (let ((H rseq-multi))
    (newline)
    (printf "Checking that intersection with wildcard is identity-like\n")
    (define m1 (pretty-print-trie*
		(foldr trie-union (trie-empty)
		       (list (pattern->trie SA (list 'a ?))
			     (pattern->trie SB (list 'b ?))
			     (pattern->trie SC (list 'b 'c))))))
    (define m2 (pretty-print-trie* (pattern->trie SD ?)))
    (define mi (pretty-print-trie* (trie-intersect m1 m2)))
    (check-requal? mi
		   (H SOL (H 'a (H  ? (H EOS (E (tset 'A 'D))))
			     'b (H  ? (H EOS (E (tset 'B 'D)))
				    'c (H EOS (E (tset 'B 'C 'D)))))))
    (check-requal? (pretty-print-trie* (trie-intersect m1 m2 #:combiner (lambda (v1 v2) v1)))
		   m1))
  )

(module+ test
  (define (trie-match-trie-list m1 m2)
    (match-define (cons s1 s2) (trie-match-trie m1 m2 #:seed (cons (tset) (tset))))
    (list s1 s2))
  (define (trie-union* a b)
    (trie-union a b #:combiner (lambda (v1 v2)
				 (match* (v1 v2)
				   [(#t v) v]
				   [(v #t) v]
				   [(v1 v2) (tset-union v1 v2)]))))
  (let ((abc (foldr trie-union* (trie-empty)
		    (list (pattern->trie SA (list 'a ?))
			  (pattern->trie SB (list 'b ?))
			  (pattern->trie SC (list 'c ?)))))
	(bcd (foldr trie-union* (trie-empty)
		    (list (pattern->trie SB (list 'b ?))
			  (pattern->trie SC (list 'c ?))
			  (pattern->trie SD (list 'd ?))))))
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
  (check-equal? (compile-projection (cons 'a 'b))
		(list SOL 'a ILM 'b EOS EOS))
  (check-equal? (compile-projection (cons 'a (?!)))
		(list SOL 'a ILM SOC ? EOC EOS EOS))
  (check-equal? (compile-projection (list 'a 'b))
		(list SOL 'a 'b EOS EOS))
  (check-equal? (compile-projection (list 'a (?!)))
		(list SOL 'a SOC ? EOC EOS EOS))

  (let ((trie-project (lambda (m spec)
			(trie-project m spec
				      #:project-success (lambda (v) #t)
				      #:combiner (lambda (v1 v2) #t)))))
    (check-requal? (trie-project (trie-union (pattern->trie SA (list 'a 'a))
					     (pattern->trie SB (list 'a 'b)))
				 (compile-projection (list 'a (?!))))
		   (trie-union* (pattern->trie #t 'a)
				(pattern->trie #t 'b)))

    (check-requal? (trie-project (trie-union (pattern->trie SA (list 'a 'a))
					     (pattern->trie SB (list 'a (vector 'b 'c 'd))))
				 (compile-projection (list 'a (?!))))
		   (trie-union* (pattern->trie #t 'a)
				(pattern->trie #t (vector 'b 'c 'd))))

    (check-requal? (trie-project (trie-union (pattern->trie SA (list 'a 'a))
					     (pattern->trie SB (list 'a (vector 'b ? 'd))))
				 (compile-projection (list 'a (?!))))
		   (trie-union* (pattern->trie #t 'a)
				(pattern->trie #t (vector 'b ? 'd))))

    (check-equal? (trie-key-set
		   (trie-project (trie-union (pattern->trie SA (list 'a 'a))
					     (pattern->trie SB (list 'a 'b)))
				 (compile-projection (list 'a (?!)))))
		  (set '(a) '(b)))

    (check-equal? (trie-key-set
		   (trie-project (trie-union (pattern->trie SA (list 'a 'a))
					     (pattern->trie SB (list 'a (vector 'b 'c 'd))))
				 (compile-projection (list 'a (?!)))))
		  (set '(a) '(#(b c d))))

    (check-equal? (trie-key-set
		   (trie-project (trie-union (pattern->trie SA (list 'a 'a))
					     (pattern->trie SB (list 'a (vector 'b ? 'd))))
				 (compile-projection (list 'a (?!)))))
		  #f)

    (check-equal? (trie-key-set
		   (trie-project (trie-union (pattern->trie SA (list 'a 'a))
					     (pattern->trie SB (list 'a (vector 'b ? 'd))))
				 (compile-projection (list 'a (?! 'a)))))
		  (set '(a)))

    (check-requal? (trie-project (trie-union (pattern->trie SA (cons 1 2))
					     (pattern->trie SB (cons 3 4)))
				 (compile-projection (cons (?!) (?!))))
		   (trie-union* (pattern->trie #t 1 2)
				(pattern->trie #t 3 4)))

    (check-requal? (trie-project (foldr trie-union (trie-empty)
					(list (pattern->trie SA (cons 1 2))
					      (pattern->trie SB (cons 1 4))
					      (pattern->trie SC (cons 3 4))))
				 (compile-projection (cons (?!) (?!))))
		   (foldr trie-union* (trie-empty)
			  (list (pattern->trie #t 1 2)
				(pattern->trie #t 1 4)
				(pattern->trie #t 3 4))))

    (check-requal? (trie-project (foldr trie-union (trie-empty)
					(list (pattern->trie SA (cons 1 2))
					      (pattern->trie SB (cons 1 4))
					      (pattern->trie SC (cons 3 4))))
				 (compile-projection (?! (cons ? ?))))
		   (foldr trie-union* (trie-empty)
			  (list (pattern->trie #t (cons 1 2))
				(pattern->trie #t (cons 1 4))
				(pattern->trie #t (cons 3 4)))))

    (check-requal? (trie-project (foldr trie-union (trie-empty)
					(list (pattern->trie SA (cons 1 2))
					      (pattern->trie SB (cons 1 4))
					      (pattern->trie SC (cons 3 4))))
				 (compile-projection (?! (cons 1 ?))))
		   (foldr trie-union* (trie-empty)
			  (list (pattern->trie #t (cons 1 2))
				(pattern->trie #t (cons 1 4)))))

    (check-requal? (trie-project (foldr trie-union (trie-empty)
					(list (pattern->trie SA (cons 1 2))
					      (pattern->trie SB (cons 1 4))
					      (pattern->trie SC (cons 3 4))))
				 (compile-projection (cons (?! 1) (?!))))
		   (foldr trie-union* (trie-empty)
			  (list (pattern->trie #t 1 2)
				(pattern->trie #t 1 4))))

    (check-requal? (trie-project (foldr trie-union (trie-empty)
					(list (pattern->trie SA (cons 1 2))
					      (pattern->trie SB (cons 1 4))
					      (pattern->trie SC (cons 3 4))))
				 (compile-projection (cons (?!) (?! 4))))
		   (foldr trie-union* (trie-empty)
			  (list (pattern->trie #t 1 4)
				(pattern->trie #t 3 4))))

    (check-equal? (trie-key-set
		   (trie-project (foldr trie-union (trie-empty)
					(list (pattern->trie SA (cons 1 2))
					      (pattern->trie SC (cons ? 3))
					      (pattern->trie SB (cons 3 4))))
				 (compile-projection (cons (?!) (?!)))))
		  #f)

    (check-equal? (trie-key-set
		   (trie-project (foldr trie-union (trie-empty)
					(list (pattern->trie SA (cons ? 2))
					      (pattern->trie SC (cons 1 3))
					      (pattern->trie SB (cons 3 4))))
				 (compile-projection (cons ? (?!)))))
		  (set '(2) '(3) '(4)))

    (check-equal? (trie-key-set
		   (trie-project (trie-union (pattern->trie SA (cons 1 2))
					     (pattern->trie SB (cons 3 4)))
				 (compile-projection (cons (?!) (?!)))))
		  (set '(1 2) '(3 4))))

  (check-requal? (trie-project (trie-union (pattern->trie SA ?)
					   (pattern->trie SB (list 'a)))
			       (compile-projection (?! (list (list ?)))))
		 (pattern->trie SA (list (list ?))))

  (check-equal? (projection->pattern (list 'a 'b)) (list 'a 'b))
  (check-equal? (projection->pattern (list 'a ?)) (list 'a ?))
  (check-equal? (projection->pattern (list 'a (?!))) (list 'a ?))
  (check-equal? (projection->pattern (list 'a (?! 'b))) (list 'a 'b))
  (check-equal? (projection->pattern (list 'a (?! (vector 'b)))) (list 'a (vector 'b)))
  (check-equal? (projection->pattern (list 'a (?! (vector ? ?)))) (list 'a (vector ? ?)))
  )

(module+ test
  (newline)
  (printf "Checking that subtraction from union is identity-like\n")

  (let ((A (pattern->trie SA ?))
	(B (pattern->trie SB (list (list (list (list 'foo)))))))
    (check-requal? (pretty-print-trie* (trie-subtract (trie-union A B) B))
		   A))
  (let ((A (pattern->trie SA ?))
	(B (trie-union (pattern->trie SB (list (list (list (list 'foo)))))
		       (pattern->trie SB (list (list (list (list 'bar))))))))
    (check-requal? (pretty-print-trie* (trie-subtract (trie-union A B) B))
		   A))
  (let ((A (pattern->trie SA ?))
	(B (trie-union (pattern->trie SB (list (list (list (list 'foo)))))
		       (pattern->trie SB (list (list (list (list 'bar))))))))
    (check-requal? (pretty-print-trie* (trie-subtract (trie-union A B) A))
		   B)))

(module+ test
  (let ((M (foldr trie-union (trie-empty)
		  (list (pattern->trie SA (list ? 2))
			(pattern->trie SC (list 1 3))
			(pattern->trie SD (list ? 3))
			(pattern->trie SB (list 3 4)))))
	(S '((("(")
	      ((1      ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("C" "D")))))))))
	       (3      ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("D")))))))
			(4 (((")") (((")") ("" ("B")))))))))
	       (("__") ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("D"))))))))))))))
    (check-equal? (trie->jsexpr M (lambda (v) (map symbol->string (tset->list v)))) S)
    (check-requal? (jsexpr->trie S (lambda (v) (make-tset hash-order (map string->symbol v)))) M)))

(module+ test
  (check-requal? (pretty-print-trie*
		  (pattern->trie SA (list 1
					  (embedded-trie
					   (pattern->trie SB (list 2 3)))
					  4)))
		 (pattern->trie SA (list 1 (list 2 3) 4)))

  (check-requal? (pretty-print-trie*
		  (pattern->trie SA
				 (list (embedded-trie (pattern->trie SB (list 1 2)))
				       (embedded-trie (pattern->trie SC (list 3 4))))))
		 (pattern->trie SA (list (list 1 2) (list 3 4)))))

(module+ test
  (void
   (pretty-print-trie* (trie-union (rwild (rsuccess SA))
				   (rseq-multi ? (rsuccess SB)
					       3 (rsuccess SC))))))

(module+ test
  (void
   (let ((m (trie-union (pattern->trie SA ?)
			(pattern->trie SB (list ? '- ?)))))
     (pretty-print-trie* m)
     (pretty-print-trie*/dot m))))

(module+ test
  (let ()
    (newline)
    (printf "Biased-intersection test\n")
    (struct obs (val) #:prefab)
    (let ((object (trie-union (pattern->trie #t 1)
			      (pattern->trie #t 2)))
          (subject (trie-union (pattern->trie #t 99)
			       (pattern->trie #t (obs ?)))))
      (pretty-print-trie* object)
      ;; The default, slow way of computing a biased intersection:
      (pretty-print-trie*
       (trie-project (trie-intersect (pattern->trie #t (obs (embedded-trie object)))
				     subject
				     #:combiner (lambda (v1 v2) #t))
		     (compile-projection (obs (?!)))
		     #:project-success (lambda (v) #t)
		     #:combiner (lambda (v1 v2) #t)))
      ;; A hopefully quicker way of doing the same:
      (define intersection (trie-intersect object
					   (trie-step subject struct:obs)
					   #:combiner (lambda (v1 v2) #t)
					   #:left-short (lambda (v r)
							  (trie-step r EOS))))
      (pretty-print-trie* intersection))
    (void)))
