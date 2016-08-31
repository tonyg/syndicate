#lang racket/base
;; Patches to sets of interests

(provide (struct-out patch)
         (struct-out observe)
         observe-parenthesis
         patch-empty
         patch-empty?
         patch-non-empty?
         patch/added?
         patch/removed?
         strip-interests
         label-interests
         strip-patch
         label-patch
         limit-patch
         limit-patch/routing-table
         patch-pruned-by
         patch-step
         patch-step*
         compute-aggregate-patch
         apply-patch
         update-interests
         unapply-patch
         compose-patch
         patch-seq
         patch-seq*
         compute-patch
         biased-intersection
         view-patch
         patch-union
         patch-project
         patch-project/set
         patch-project/set/single

         pretty-print-patch
         patch->pretty-string
         extract-patch-pids

         patch->jsexpr
         jsexpr->patch)

(require racket/set)
(require racket/match)
(require (only-in racket/string string-trim))
(require "trie.rkt")
(require "tset.rkt")
(require "pretty.rkt")
(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Patches
(struct patch (added removed)
  #:transparent
  #:methods gen:syndicate-pretty-printable
  [(define (syndicate-pretty-print d [p (current-output-port)])
     (pretty-print-patch d p))])

;; Interests
(struct observe (claim) #:prefab)

(define patch-empty (patch trie-empty trie-empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define observe-parenthesis (open-parenthesis 1 struct:observe))

(define (patch-empty? p)
  (and (patch? p)
       (trie-empty? (patch-added p))
       (trie-empty? (patch-removed p))))

(define (patch-non-empty? p)
  (and (patch? p)
       (or (trie-non-empty? (patch-added p))
           (trie-non-empty? (patch-removed p)))))

(define (patch/added? p) (and (patch? p) (trie-non-empty? (patch-added p))))
(define (patch/removed? p) (and (patch? p) (trie-non-empty? (patch-removed p))))

(define (strip-interests g)
  (trie-relabel g (lambda (v) '<strip-interests>)))

(define (label-interests g label)
  (trie-relabel g (lambda (v) label)))

(define (strip-patch p)
  (patch (strip-interests (patch-added p))
         (strip-interests (patch-removed p))))

(define (label-patch p label)
  (patch (label-interests (patch-added p) label)
         (label-interests (patch-removed p) label)))

;; When given a set-labelled p and bound, assumes that the label sets
;; only ever contain one element, thereby acting as if given a
;; #t-labelled p and bound.
;;
;; Doesn't work in general with a mix of set- and #t-labelled
;; arguments.
(define (limit-patch p bound)
  (match-define (patch in out) p)
  (patch (trie-subtract in bound #:combiner (lambda (v1 v2) trie-empty))
         (trie-intersect out bound #:combiner (lambda (v1 v2) (trie-success v1)))))

;; Like limit-patch, but for use when the precise bound for p's label
;; isn't known (such as when a process terminates with remaining
;; queued actions), so we have to examine the whole area of the
;; routing table touched by p.
;;
;; Unlike limit-patch, expects set-labelled patch and bound, with
;; label sets allowed to contain arbitrary elements.
(define (limit-patch/routing-table p bound)
  (match-define (patch in out) p)
  (patch (trie-subtract in bound)
         (trie-intersect out bound
			 #:combiner (lambda (v1 v2) (empty-tset-guard (tset-intersect v1 v2))))))

;; Completely ignores success-values in t.
(define (patch-pruned-by p t)
  (match-define (patch added removed) p)
  (patch (trie-subtract #:combiner (lambda (v1 v2) trie-empty) added t)
         (trie-subtract #:combiner (lambda (v1 v2) trie-empty) removed t)))

;; Steps both added and removes sets
(define (patch-step p key)
  (match-define (patch added removed) p)
  (patch (trie-step added key)
         (trie-step removed key)))

(define (patch-step* p keys)
  (foldl (lambda (key p) (patch-step p key)) p keys))

;; Entries labelled with `label` may already exist in `base`; the
;; patch `p` MUST already have been limited to add only where no
;; `label`-labelled portions of `base` exist, and to remove only where
;; `label`-labelled portions of `base` exist. `base` must, then, be a
;; "pre-modification" routing table. Use `limit-patch` to compute a
;; suitable `p`, where the bound is known; otherwise, use
;; `limit-patch/routing-table`.
;;
;; The goal here is to say "here is the effect of `p` on the overall
;; coverage established by the non-`label` participants in the
;; interest-set `base`". While `p` might add quite a bit of coverage
;; to `label`'s interests, it might overlap with coverage previously
;; established by others, in which case those portions would
;; experience /no effect/ from p. Likewise, `p` may remove interests
;; from `label`'s own interests, but where interest remains from other
;; peers, the overall effect will be nil.
;;
;; PRECONDITION: `p` is (set label)-labelled
;; PRECONDITION: `base` is (set ...)-labelled
(define (compute-aggregate-patch p label base)
  (define (add-combiner v1 v2)
    ;; Keep only points where `p` would add, where no `label` interest
    ;; is present*, and where no non-`label` interest is present. That
    ;; is, keep only points where no existing interest is present at
    ;; all. Since add-combiner is called only for points where v2 is
    ;; non-empty, meaning that some existing interest is present (and
    ;; furthermore, we know that a previous patch-limiting operation
    ;; has established that no `label` interest is present at these
    ;; points), we can always discard such points by returning a
    ;; constant trie-empty.
    trie-empty)
  (define (rem-combiner v1 v2)
    ;; Keep only points where `p` would remove, where `label` interest
    ;; is present, and where no non-`label` interest is present. We
    ;; know that a previous patch-limiting operation has ensured that
    ;; `label` interest is present, so we only need to check whether
    ;; any other interest exists at each point.
    (if (= (tset-count v2) 1)
        (trie-success v1) ;; only `label` interest (previously established) exists here.
        trie-empty))
  (patch (trie-subtract (patch-added p) base #:combiner add-combiner)
         (trie-subtract (patch-removed p) base #:combiner rem-combiner)))

;; For use by Tries leading to (Setof Label).
(define (apply-patch base p)
  (match-define (patch in out) p)
  (trie-union (trie-subtract base out) in))

;; Like apply-patch, but for use by Tries leading to True.
(define (update-interests base p)
  (match-define (patch in out) p)
  (trie-union (trie-subtract base out #:combiner (lambda (v1 v2) trie-empty)) in
	      #:combiner (lambda (v1 v2) (trie-success '<update-interests>))))

(define (unapply-patch base p)
  (match-define (patch in out) p)
  (trie-union (trie-subtract base in) out))

(define (compose-patch p2 p1) ;; p2 after p1
  ;; Can be defined as (patch (apply-patch in1 p2) (unapply-patch out1 p2)),
  ;; except for problems arising from use of set-subtract by default in {un,}apply-patch
  (match-define (patch in1 out1) p1)
  (match-define (patch in2 out2) p2)
  (patch (update-interests in1 p2)
         (trie-union (trie-subtract out1 in2 #:combiner (lambda (v1 v2) trie-empty)) out2
		     #:combiner (lambda (v1 v2) (trie-success '<compose-patch>)))))

(define (patch-seq . patches) (patch-seq* patches))

(define (patch-seq* patches)
  (match patches
    ['() patch-empty]
    [(cons p rest) (compose-patch (patch-seq* rest) p)]))

(define (compute-patch old-base new-base)
  (patch (trie-subtract new-base old-base #:combiner (lambda (v1 v2) trie-empty))
         (trie-subtract old-base new-base #:combiner (lambda (v1 v2) trie-empty))))

(define (biased-intersection object subject)
  (trie-intersect object
		  (trie-step subject observe-parenthesis)
		  #:combiner (lambda (v1 v2) (trie-success v1))))

(define (view-patch p interests)
  (patch (biased-intersection (patch-added p) interests)
         (biased-intersection (patch-removed p) interests)))

(define (patch-union p1 p2)
  (patch (trie-union (patch-added p1) (patch-added p2))
         (trie-union (patch-removed p1) (patch-removed p2))))

(define (patch-project p spec)
  (match-define (patch in out) p)
  (patch (trie-project in spec) (trie-project out spec)))

(define (patch-project/set p spec #:take take-count)
  (match-define (patch in out) p)
  (values (trie-project/set #:take take-count in spec)
          (trie-project/set #:take take-count out spec)))

(define (patch-project/set/single p spec)
  (match-define (patch in out) p)
  (values (trie-project/set/single in spec) (trie-project/set/single out spec)))

(define (pretty-print-patch p [port (current-output-port)])
  (display (patch->pretty-string p) port))

(define (patch->pretty-string p)
  (match-define (patch in out) p)
  (format "-~a\n+~a\n"
          (string-trim #:left? #f (trie->pretty-string out #:indent "-"))
          (string-trim #:left? #f (trie->pretty-string in #:indent "+"))))

(define (extract-patch-pids p)
  (match-define (patch in out) p)
  (trie-value-fold tset/set-union (trie-value-fold tset/set-union (set) in) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch->jsexpr p success->jsexpr #:serialize-atom [serialize-atom values])
  (match-define (patch in out) p)
  (list (trie->jsexpr in success->jsexpr #:serialize-atom serialize-atom)
        (trie->jsexpr out success->jsexpr #:serialize-atom serialize-atom)))

(define (jsexpr->patch pj
                       jsexpr->success
                       [lookup-struct-type (lambda (arity t) #f)]
                       #:deserialize-atom [deserialize-atom values])
  (match-define (list ij oj) pj)
  (patch (jsexpr->trie ij jsexpr->success lookup-struct-type #:deserialize-atom deserialize-atom)
         (jsexpr->trie oj jsexpr->success lookup-struct-type #:deserialize-atom deserialize-atom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define (set->trie label xs)
    (for/fold [(acc trie-empty)] [(x (in-set xs))]
      (trie-union acc (pattern->trie label x))))

  ;; Retains only entries in R labelled with any subset of the labels in label-set.
  (define (project-routing-table R label-set)
    (trie-intersect R
		    (pattern->trie label-set ?)
		    #:combiner (lambda (v1 v2) (empty-tset-guard (tset-intersect v1 v2)))))

  (define tset datum-tset)

  (define (sanity-check-examples)
    (define SP (tset 'P))
    (define m0 trie-empty)
    (define ma (pattern->trie SP 'a))
    (define mb (pattern->trie SP 'b))
    (define mc (pattern->trie SP 'c))
    (define mab (trie-union ma mb))
    (define mbc (trie-union mb mc))
    (define m* (pattern->trie SP ?))

    (printf "\nmab:\n")
    (void (pretty-print-trie mab))

    (printf "\ncompute-patch ma mb:\n")
    (void (pretty-print-patch (compute-patch ma mb)))

    (printf "\nlimit-patch m*/m0 mab:\n")
    (void (pretty-print-patch (limit-patch (patch m* m0) mab)))

    (printf "\nlimit-patch m0/m* mab:\n")
    (void (pretty-print-patch (limit-patch (patch m0 m*) mab)))

    (printf "\napply mb (limit m*/m0 mab):\n")
    (void (pretty-print-trie (apply-patch mb (limit-patch (patch m* m0) mab))))

    (printf "\nlimit mbc/ma ma:\n")
    (void (pretty-print-patch (limit-patch (patch mbc ma) ma)))

    (printf "\nlimit mab/mc ma:\n")
    (void (pretty-print-patch (limit-patch (patch mab mc) ma)))

    (printf "\nlimit mc/mab ma:\n")
    (void (pretty-print-patch (limit-patch (patch mc mab) ma)))

    (printf "\ncompute-aggregate-patch m*/m0 Q mab:\n")
    (void (pretty-print-patch (compute-aggregate-patch (patch m* m0) 'Q mab)))

    (printf "\ncompute-aggregate-patch m0/m* Q mab:\n")
    (void (pretty-print-patch (compute-aggregate-patch (patch m0 m*) 'Q mab)))

    (printf "\ncompute-aggregate-patch m*/m0 P mab:\n")
    (void (pretty-print-patch (compute-aggregate-patch (patch m* m0) 'P mab)))

    (printf "\ncompute-aggregate-patch m0/m* P mab:\n")
    (void (pretty-print-patch (compute-aggregate-patch (patch m0 m*) 'P mab)))

    (printf "\ncompute-aggregate-patch m*/m0 Q m*:\n")
    (void (pretty-print-patch (compute-aggregate-patch (patch m* m0) 'Q m*)))

    (printf "\ncompute-aggregate-patch m0/m* Q m*:\n")
    (void (pretty-print-patch (compute-aggregate-patch (patch m0 m*) 'Q m*)))

    (printf "\ncompute-aggregate-patch m*/m0 P m*:\n")
    (void (pretty-print-patch (compute-aggregate-patch (patch m* m0) 'P m*)))

    (printf "\ncompute-aggregate-patch m0/m* P m*:\n")
    (void (pretty-print-patch (compute-aggregate-patch (patch m0 m*) 'P m*)))

    (printf "\ncompose mbc/m0 after mc/mab:\n")
    (void (pretty-print-patch (compose-patch (patch mbc m0) (patch mc mab))))

    (printf "\ncompose mc/mab after mbc/m0:\n")
    (void (pretty-print-patch (compose-patch (patch mc mab) (patch mbc m0))))

    (printf "\ncompose mc/m* (not disjoint) after mbc/m0:\n")
    (void (pretty-print-patch (compose-patch (patch mc m*) (patch mbc m0))))

    (printf "\ncompose mbc/m0 after mc/m* (not disjoint):\n")
    (void (pretty-print-patch (compose-patch (patch mbc m0) (patch mc m*))))

    (printf "\nstripped compose mc/m* (not disjoint) after mbc/m0:\n")
    (void (pretty-print-patch (compose-patch (strip-patch (patch mc m*))
                                             (strip-patch (patch mbc m0)))))
    )

  ;; (sanity-check-examples)

  (let* ((pre-patch-a-keys                      (set   1   3   5   7))
         (pre-patch-b-keys                      (set     2 3     6 7))
         (pre-patch-keys                        (set   1 2 3   5 6 7))
         (ma (set->trie (tset 'a) pre-patch-a-keys))
         (mb (set->trie (tset 'b) pre-patch-b-keys))
         (R (trie-union ma mb))
         (pa-raw (patch (set->trie (tset 'a) (set 0 1 2 3        ))
                        (set->trie (tset 'a) (set         4 5 6 7))))
         (pa1 (limit-patch pa-raw ma))
         (pa2 (limit-patch/routing-table pa-raw R))
         (post-patch-a-keys                     (set 0 1 2 3        ))
         (post-patch-b-keys pre-patch-b-keys)
         (post-patch-keys                       (set 0 1 2 3     6 7))
         (aggregate-added                       (set 0              ))
         (aggregate-removed                     (set           5    ))
         (p-aggregate1 (compute-aggregate-patch pa1 'a R))
         (p-aggregate2 (compute-aggregate-patch pa2 'a R))
         (R1 (apply-patch R pa1))
         (R2 (apply-patch R pa2))
         (R-relabeled (trie-relabel R (lambda (v) (tset 'x))))
         (R1-relabeled (apply-patch R-relabeled (label-patch (strip-patch p-aggregate1) (tset 'x))))
         (R2-relabeled (apply-patch R-relabeled (label-patch (strip-patch p-aggregate2) (tset 'x)))))
    (check-equal? pa1 pa2)
    (check-equal? (trie-lookup R 0 (tset)) (tset))
    (check-equal? (trie-lookup R 1 (tset)) (tset 'a))
    (check-equal? (trie-lookup R 2 (tset)) (tset 'b))
    (check-equal? (trie-lookup R 3 (tset)) (tset 'a 'b))
    (check-equal? (trie-lookup R 4 (tset)) (tset))
    (check-equal? (trie-lookup R 5 (tset)) (tset 'a))
    (check-equal? (trie-lookup R 6 (tset)) (tset 'b))
    (check-equal? (trie-lookup R 7 (tset)) (tset 'a 'b))
    (check-equal? (trie-key-set/single (project-routing-table R (tset 'a))) pre-patch-a-keys)
    (check-equal? (trie-key-set/single (project-routing-table R (tset 'b))) pre-patch-b-keys)
    (check-equal? (trie-key-set/single R) pre-patch-keys)
    (check-equal? (trie-key-set/single R-relabeled) pre-patch-keys)

    (define (post-checks R* R*-relabeled p-aggregate)
      (check-equal? (trie-key-set/single (project-routing-table R* (tset 'a))) post-patch-a-keys)
      (check-equal? (trie-key-set/single (project-routing-table R* (tset 'b))) post-patch-b-keys)
      (check-equal? (trie-key-set/single R*) post-patch-keys)
      (check-equal? (trie-key-set/single R*-relabeled) post-patch-keys)
      (check-equal? (trie-key-set/single (patch-added p-aggregate)) aggregate-added)
      (check-equal? (trie-key-set/single (patch-removed p-aggregate)) aggregate-removed))

    (post-checks R1 R1-relabeled p-aggregate1)
    (post-checks R2 R2-relabeled p-aggregate2)
    )

  (let* ((ma (set->trie (tset 'a) (set 1)))
         (mb (set->trie (tset 'b) (set 1)))
         (mmeta (set->trie (tset 'meta) (set 1)))
         (R0 trie-empty)
         (R1 mmeta)
         (R2 mb)
         (R3 (trie-union mb mmeta))
         (R4 ma)
         (R5 (trie-union ma mmeta))
         (R6 (trie-union ma mb))
         (R7 (trie-union (trie-union ma mb) mmeta))
         (p0 patch-empty)
         (p+ (patch (set->trie (tset 'a) (set 1)) trie-empty))
         (p- (patch trie-empty (set->trie (tset 'a) (set 1)))))
    (check-equal? (compute-aggregate-patch p0 'a R0) p0)
    (check-equal? (compute-aggregate-patch p0 'a R1) p0)
    (check-equal? (compute-aggregate-patch p0 'a R2) p0)
    (check-equal? (compute-aggregate-patch p0 'a R3) p0)
    (check-equal? (compute-aggregate-patch p0 'a R4) p0)
    (check-equal? (compute-aggregate-patch p0 'a R5) p0)
    (check-equal? (compute-aggregate-patch p0 'a R6) p0)
    (check-equal? (compute-aggregate-patch p0 'a R7) p0)
    (check-equal? (compute-aggregate-patch p+ 'a R0) p+)
    (check-equal? (compute-aggregate-patch p+ 'a R1) p0)
    (check-equal? (compute-aggregate-patch p+ 'a R2) p0)
    (check-equal? (compute-aggregate-patch p+ 'a R3) p0)
    (check-equal? (compute-aggregate-patch p- 'a R4) p-)
    (check-equal? (compute-aggregate-patch p- 'a R5) p0)
    (check-equal? (compute-aggregate-patch p- 'a R6) p0)
    (check-equal? (compute-aggregate-patch p- 'a R7) p0)
    )

  (let ((m1 (set->trie '<m1> (set 1 2)))
        (m2 (set->trie (tset 'a) (set 1 2)))
        (p1 (patch (set->trie '<p1> (set 2 3)) trie-empty))
        (p2 (patch (set->trie (tset 'a) (set 2 3)) trie-empty)))
    (check-equal? (limit-patch p1 m1) (patch (set->trie '<p1> (set 3)) trie-empty))
    (check-equal? (limit-patch p1 m2) (patch (set->trie '<p1> (set 3)) trie-empty))
    (check-equal? (limit-patch p2 m1) (patch (set->trie (tset 'a) (set 3)) trie-empty))
    (check-equal? (limit-patch p2 m2) (patch (set->trie (tset 'a) (set 3)) trie-empty))
    )

  (let ((m1 (set->trie '<m1> (set 1 2)))
        (m2 (set->trie (tset 'a) (set 1 2)))
        (p1 (patch trie-empty (set->trie '<p1> (set 2 3))))
        (p2 (patch trie-empty (set->trie (tset 'a) (set 2 3)))))
    (check-equal? (limit-patch p1 m1) (patch trie-empty (set->trie '<p1> (set 2))))
    (check-equal? (limit-patch p1 m2) (patch trie-empty (set->trie '<p1> (set 2))))
    (check-equal? (limit-patch p2 m1) (patch trie-empty (set->trie (tset 'a) (set 2))))
    (check-equal? (limit-patch p2 m2) (patch trie-empty (set->trie (tset 'a) (set 2))))
    )
  )
