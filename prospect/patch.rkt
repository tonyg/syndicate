#lang racket/base
;; Patches to sets of interests

(provide (struct-out patch)
         (struct-out observe)
         (struct-out at-meta)
         (struct-out advertise)
         empty-patch
         patch-empty?
         lift-patch
         drop-patch
         strip-interests
         label-interests
         strip-patch
         label-patch
         limit-patch
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

         pretty-print-patch)

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
(define observe-proj (compile-projection (observe (?!))))

(define (patch-empty? p)
  (and (patch? p)
       (matcher-empty? (patch-added p))
       (matcher-empty? (patch-removed p))))

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

(define (limit-patch p bound)
  (match-define (patch in out) p)
  (patch (matcher-subtract in bound #:combiner (lambda (v1 v2) #f))
         (matcher-intersect out bound #:combiner (lambda (v1 v2) v1))))

(define (compute-aggregate-patch p label base)
  (define (combiner v1 v2) (matcher-subtract-combiner v1 (set-remove v2 label)))
  (patch (matcher-subtract (patch-added p) base #:combiner combiner)
         (matcher-subtract (patch-removed p) base #:combiner combiner)))

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
  (matcher-project (matcher-intersect (pattern->matcher #t (observe (embedded-matcher object)))
                                      subject
                                      #:combiner (lambda (v1 v2) #t))
                   observe-proj
                   #:project-success (lambda (v) #t)
                   #:combiner (lambda (v1 v2) #t)))

(define (view-patch p interests)
  (patch (biased-intersection (patch-added p) interests)
         (biased-intersection (patch-removed p) interests)))

(define (pretty-print-patch p [port (current-output-port)])
  (match-define (patch in out) p)
  (fprintf port "<<<<<<<< Removed:\n")
  (pretty-print-matcher out port)
  (fprintf port "======== Added:\n")
  (pretty-print-matcher in port)
  (fprintf port ">>>>>>>>\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (define SP (set 'P))
  (define m0 (matcher-empty))
  (define ma (pattern->matcher SP 'a))
  (define mb (pattern->matcher SP 'b))
  (define mc (pattern->matcher SP 'c))
  (define mab (matcher-union ma mb))
  (define mbc (matcher-union mb mc))
  (define m* (pattern->matcher SP ?))

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
  )
