#lang turnstile

(provide Hash
         (for-syntax ~Hash)
         hash
         hash-set
         hash-ref
         (typed-out [[hash-ref/failure- : (∀ (K V) (→fn (Hash K V) K V V))]
                     hash-ref/failure])
         hash-has-key?
         hash-update
         (typed-out [[hash-update/failure- : (∀ (K V) (→fn (Hash K V) K (→fn V V) V (Hash K V)))]
                     hash-update/failure])
         hash-remove
         hash-map
         hash-keys
         hash-values
         hash-keys-subset?
         hash-count
         hash-empty?
         hash-union
         (typed-out [[hash-union/combine- : (∀ (K V) (→fn (Hash K V) (Hash K V) (→fn V V V) (Hash K V)))]
                     hash-union/combine])
         )

(require "core-types.rkt")
(require (only-in "list.rkt" List))
(require (only-in "prim.rkt" Int Bool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immutable Hash Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-container-type Hash #:arity = 2)

(define-typed-syntax (hash (~seq key:expr val:expr) ...) ≫
  [⊢ key ≫ key- (⇒ : τ-k)] ...
  [⊢ val ≫ val- (⇒ : τ-val)] ...
  #:fail-unless (all-pure? #'(key- ... val- ...)) "gotta be pure"
  --------------------------------------------------
  [⊢ (#%app- hash- (~@ key val) ...) (⇒ : (Hash (U τ-k ...) (U τ-val ...)))])

(require/typed racket/base
  ;; don't have a type for ConsPair
  #;[make-hash : (∀ (K V) (→fn (List (ConsPair K V)) (Hash K V)))]
  [hash-set : (∀ (K V) (→fn (Hash K V) K V (Hash K V)))]
  [hash-ref : (∀ (K V) (→fn (Hash K V) K V))]
  ;; TODO hash-ref/failure
  [hash-has-key? : (∀ (K V) (→fn (Hash K V) K Bool))]
  [hash-update : (∀ (K V) (→fn (Hash K V) K (→fn V V) (Hash K V)))]
  ;; TODO hash-update/failure
  [hash-remove : (∀ (K V) (→fn (Hash K V) K (Hash K V)))]
  [hash-map : (∀ (K V R) (→fn (Hash K V) (→fn K V R) (List R)))]
  [hash-keys : (∀ (K V) (→fn (Hash K V) (List K)))]
  [hash-values : (∀ (K V) (→fn (Hash K V) (List V)))]
  ;; TODO hash->list makes cons pairs
  #;[hash->list : (∀ (K V) (→fn (Hash K V) (List (ConsPair K V))))]
  [hash-keys-subset? : (∀ (K1 V1 K2 V2) (→fn (Hash K1 V1) (Hash K2 V2) Bool))]
  [hash-count : (∀ (K V) (→fn (Hash K V) Int))]
  [hash-empty? : (∀ (K V) (→fn (Hash K V) Bool))])

(require/typed racket/hash
  [hash-union : (∀ (K1 V1 K2 V2) (→fn (Hash K1 V1) (Hash K2 V2) (Hash (U K1 K2) (U V1 V2))))]
  ;; TODO - hash-union with #:combine
  )

(define- (hash-ref/failure- h k err)
  (#%app- hash-ref- h k err))

(define- (hash-update/failure- h k u err)
  (#%app- hash-update- h k u err))

(define- (hash-union/combine- h1 h2 combine)
  (#%app- hash-union- h1 h2 #:combine combine))
