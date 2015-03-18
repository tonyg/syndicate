#lang racket/base
;; Patches to sets of interests

(provide (struct-out patch)
         (struct-out observe)
         (struct-out at-meta)
         (struct-out advertise)
         empty-patch
         patch-empty?
         patch-non-empty?
         patch/added?
         patch/removed?
         lift-patch
         drop-patch
         strip-interests
         label-interests
         strip-patch
         label-patch
         limit-patch
         limit-patch/routing-table
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
         patch-project
         patch-project/set
         patch-project/set/single

         pretty-print-patch
         patch->pretty-string)

(require racket/set)
(require racket/match)
(require "route.rkt")
(module+ test (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Patches
(struct patch (added removed) #:prefab)

;; Claims, Interests, Locations, and Advertisements
(struct observe (claim) #:prefab)
(struct at-meta (claim) #:prefab)
(struct advertise (claim) #:prefab)

(define empty-patch (patch (matcher-empty) (matcher-empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define at-meta-proj (compile-projection (at-meta (?!))))

(define (patch-empty? p)
  (and (patch? p)
       (matcher-empty? (patch-added p))
       (matcher-empty? (patch-removed p))))

(define (patch-non-empty? p)
  (and (patch? p)
       (or (matcher-non-empty? (patch-added p))
           (matcher-non-empty? (patch-removed p)))))

(define (patch/added? p) (and (patch? p) (matcher-non-empty? (patch-added p))))
(define (patch/removed? p) (and (patch? p) (matcher-non-empty? (patch-removed p))))

(define (lift-patch p)
  (match-define (patch in out) p)
  (patch (pattern->matcher #t (at-meta (embedded-matcher in)))
         (pattern->matcher #t (at-meta (embedded-matcher out)))))

(define (drop-interests pi)
  (matcher-project pi at-meta-proj
                   #:project-success (lambda (v) #t)
                   #:combiner (lambda (v1 v2) #t)))

(define (drop-patch p)
  (match-define (patch in out) p)
  (patch (drop-interests in)
         (drop-interests out)))

(define (strip-interests g)
  (matcher-relabel g (lambda (v) #t)))

(define (label-interests g label)
  (matcher-relabel g (lambda (v) label)))

(define (strip-patch p)
  (patch (strip-interests (patch-added p))
         (strip-interests (patch-removed p))))

(define (label-patch p label)
  (patch (label-interests (patch-added p) label)
         (label-interests (patch-removed p) label)))

;; Requires set-labelled p and bound, but assumes that the label sets
;; only ever contain one element.
(define (limit-patch p bound)
  (match-define (patch in out) p)
  (patch (matcher-subtract in bound #:combiner (lambda (v1 v2) #f))
         (matcher-intersect out bound #:combiner (lambda (v1 v2) v1))))

;; Like limit-patch, but for use when the precise bound for p's label
;; isn't known (such as when a process terminates with remaining
;; queued actions), so we have to examine the whole area of the
;; routing table touched by p.
;;
;; Unlike limit-patch, accepts label sets with arbitrary elements.
(define (limit-patch/routing-table p bound)
  (match-define (patch in out) p)
  (patch (matcher-subtract in bound)
         (matcher-intersect out bound
                            #:combiner (lambda (v1 v2) (empty-set-guard (set-intersect v1 v2))))))

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
    ;; constant #f.
    #f)
  (define (rem-combiner v1 v2)
    ;; Keep only points where `p` would remove, where `label` interest
    ;; is present, and where no non-`label` interest is present. We
    ;; know that a previous patch-limiting operation has ensured that
    ;; `label` interest is present, so we only need to check whether
    ;; any other interest exists at each point.
    (if (= (set-count v2) 1)
        v1 ;; only `label` interest (previously established) exists here.
        #f)) ;; other interest exists here, so we should discard this removed-point.
  (patch (matcher-subtract (patch-added p) base #:combiner add-combiner)
         (matcher-subtract (patch-removed p) base #:combiner rem-combiner)))

;; For use by Matchers leading to (Setof Label).
(define (apply-patch base p)
  (match-define (patch in out) p)
  (matcher-union (matcher-subtract base out) in))

;; Like apply-patch, but for use by Matchers leading to True.
(define (update-interests base p)
  (match-define (patch in out) p)
  (matcher-union (matcher-subtract base out #:combiner (lambda (v1 v2) #f)) in
                 #:combiner (lambda (v1 v2) #t)))

(define (unapply-patch base p)
  (match-define (patch in out) p)
  (matcher-union (matcher-subtract base in) out))

(define (compose-patch p2 p1) ;; p2 after p1
  ;; Can be defined as (patch (apply-patch in1 p2) (unapply-patch out1 p2)),
  ;; except for problems arising from use of set-subtract by default in {un,}apply-patch
  (match-define (patch in1 out1) p1)
  (match-define (patch in2 out2) p2)
  (patch (update-interests in1 p2)
         (matcher-union (matcher-subtract out1 in2 #:combiner (lambda (v1 v2) #f)) out2
                        #:combiner (lambda (v1 v2) #t))))

(define (patch-seq . patches) (patch-seq* patches))

(define (patch-seq* patches)
  (match patches
    ['() empty-patch]
    [(cons p rest) (compose-patch (patch-seq* rest) p)]))

(define (compute-patch old-base new-base)
  (patch (matcher-subtract new-base old-base)
         (matcher-subtract old-base new-base)))

(define (biased-intersection object subject)
  (matcher-intersect object
                     (matcher-step subject struct:observe)
                     #:combiner (lambda (v1 v2) #t)
                     #:left-short (lambda (v r) (matcher-step r EOS))))

(define (view-patch p interests)
  (patch (biased-intersection (patch-added p) interests)
         (biased-intersection (patch-removed p) interests)))

(define (patch-project p spec)
  (match-define (patch in out) p)
  (patch (matcher-project in spec) (matcher-project out spec)))

(define (patch-project/set p spec)
  (match-define (patch in out) p)
  (values (matcher-project/set in spec) (matcher-project/set out spec)))

(define (patch-project/set/single p spec)
  (match-define (patch in out) p)
  (values (matcher-project/set/single in spec) (matcher-project/set/single out spec)))

(define (pretty-print-patch p [port (current-output-port)])
  (display (patch->pretty-string p) port))

(define (patch->pretty-string p)
  (match-define (patch in out) p)
  (format "<<<<<<<< Removed:\n~a======== Added:\n~a>>>>>>>>\n"
          (matcher->pretty-string out)
          (matcher->pretty-string in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define (set->matcher label xs)
    (for/fold [(acc (matcher-empty))] [(x (in-set xs))]
      (matcher-union acc (pattern->matcher label x))))

  ;; Retains only entries in R labelled with any subset of the labels in label-set.
  (define (project-routing-table R label-set)
    (matcher-intersect R
                       (pattern->matcher label-set ?)
                       #:combiner (lambda (v1 v2) (empty-set-guard (set-intersect v1 v2)))))

  (define (sanity-check-examples)
    (define SP (set 'P))
    (define m0 (matcher-empty))
    (define ma (pattern->matcher SP 'a))
    (define mb (pattern->matcher SP 'b))
    (define mc (pattern->matcher SP 'c))
    (define mab (matcher-union ma mb))
    (define mbc (matcher-union mb mc))
    (define m* (pattern->matcher SP ?))
    (define mA (pattern->matcher SP (at-meta 'a)))
    (define mAb (matcher-union mA mb))

    (printf "\nmab:\n")
    (void (pretty-print-matcher mab))

    (printf "\ncompute-patch ma mb:\n")
    (void (pretty-print-patch (compute-patch ma mb)))

    (printf "\nlimit-patch m*/m0 mab:\n")
    (void (pretty-print-patch (limit-patch (patch m* m0) mab)))

    (printf "\nlimit-patch m0/m* mab:\n")
    (void (pretty-print-patch (limit-patch (patch m0 m*) mab)))

    (printf "\napply mb (limit m*/m0 mab):\n")
    (void (pretty-print-matcher (apply-patch mb (limit-patch (patch m* m0) mab))))

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

    (printf "\nlift mc/mab:\n")
    (void (pretty-print-patch (lift-patch (patch mc mab))))

    (printf "\ndrop after lift mc/mab:\n")
    (void (pretty-print-patch (drop-patch (lift-patch (patch mc mab)))))

    (printf "\ncompose mbc/m0 after mc/mab:\n")
    (void (pretty-print-patch (compose-patch (patch mbc m0) (patch mc mab))))

    (printf "\ncompose mc/mab after mbc/m0:\n")
    (void (pretty-print-patch (compose-patch (patch mc mab) (patch mbc m0))))

    (printf "\ncompose mc/m* (not disjoint) after mbc/m0:\n")
    (void (pretty-print-patch (compose-patch (patch mc m*) (patch mbc m0))))

    (printf "\ncompose mbc/m0 after mc/m* (not disjoint):\n")
    (void (pretty-print-patch (compose-patch (patch mbc m0) (patch mc m*))))

    (printf "\ncompose mbc/m0 after lift mc/mab:\n")
    (void (pretty-print-patch (compose-patch (patch mbc m0)
                                             (lift-patch (patch mc mab)))))

    (printf "\ndrop (compose mbc/m0 after lift mc/mab):\n")
    (void (pretty-print-patch (drop-patch (compose-patch (patch mbc m0)
                                                         (lift-patch (patch mc mab))))))

    (printf "\nstripped compose mc/m* (not disjoint) after mbc/m0:\n")
    (void (pretty-print-patch (compose-patch (strip-patch (patch mc m*))
                                             (strip-patch (patch mbc m0)))))

    (printf "\ndrop mAb/m0:\n")
    (void (pretty-print-patch (drop-patch (patch mAb m0))))
    )

  ;; (sanity-check-examples)

  (let* ((pre-patch-a-keys                      (set   1   3   5   7))
         (pre-patch-b-keys                      (set     2 3     6 7))
         (pre-patch-keys                        (set   1 2 3   5 6 7))
         (ma (set->matcher (set 'a) pre-patch-a-keys))
         (mb (set->matcher (set 'b) pre-patch-b-keys))
         (R (matcher-union ma mb))
         (pa-raw (patch (set->matcher (set 'a)  (set 0 1 2 3        ))
                        (set->matcher (set 'a)  (set         4 5 6 7))))
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
         (R-relabeled (matcher-relabel R (lambda (v) (set 'x))))
         (R1-relabeled (apply-patch R-relabeled (label-patch (strip-patch p-aggregate1) (set 'x))))
         (R2-relabeled (apply-patch R-relabeled (label-patch (strip-patch p-aggregate2) (set 'x)))))
    (check-equal? pa1 pa2)
    (check-equal? (matcher-match-value R 0) (set))
    (check-equal? (matcher-match-value R 1) (set 'a))
    (check-equal? (matcher-match-value R 2) (set 'b))
    (check-equal? (matcher-match-value R 3) (set 'a 'b))
    (check-equal? (matcher-match-value R 4) (set))
    (check-equal? (matcher-match-value R 5) (set 'a))
    (check-equal? (matcher-match-value R 6) (set 'b))
    (check-equal? (matcher-match-value R 7) (set 'a 'b))
    (check-equal? (matcher-key-set/single (project-routing-table R (set 'a))) pre-patch-a-keys)
    (check-equal? (matcher-key-set/single (project-routing-table R (set 'b))) pre-patch-b-keys)
    (check-equal? (matcher-key-set/single R) pre-patch-keys)
    (check-equal? (matcher-key-set/single R-relabeled) pre-patch-keys)

    (define (post-checks R* R*-relabeled p-aggregate)
      (check-equal? (matcher-key-set/single (project-routing-table R* (set 'a))) post-patch-a-keys)
      (check-equal? (matcher-key-set/single (project-routing-table R* (set 'b))) post-patch-b-keys)
      (check-equal? (matcher-key-set/single R*) post-patch-keys)
      (check-equal? (matcher-key-set/single R*-relabeled) post-patch-keys)
      (check-equal? (matcher-key-set/single (patch-added p-aggregate)) aggregate-added)
      (check-equal? (matcher-key-set/single (patch-removed p-aggregate)) aggregate-removed))

    (post-checks R1 R1-relabeled p-aggregate1)
    (post-checks R2 R2-relabeled p-aggregate2)
    )
  )
