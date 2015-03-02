#lang racket/base
;; Treaps, which have the lovely property of *canonical representation*.
;;
;; We take care to preserve an additional invariant:
;;  - if n is a left child of m, then n's priority <= m's priority, and
;;  - if n is a right child of m, then n's priority < m's priority.
;;
;; Further, we explicitly canonicalize N instances, so eq? works to compare treaps by value.

(provide treap?
         treap-size
         treap-empty
         treap-empty?
         treap-insert
         treap-delete
         treap-get
         treap-keys
         treap-values
         treap-to-alist
         treap-has-key?

         treap-height)

(require racket/set)
(require racket/match)

(require "canonicalize.rkt")
;; (define canonicalize values)

(struct N (key value priority left right) #:transparent
	#:methods gen:equal+hash
	[(define (equal-proc a b =?)
	   (match-define (N ak av ap al ar) a)
	   (match-define (N bk bv bp bl br) b)
	   (and (eq? al bl)
		(eq? ar br)
		(= ap bp)
		(=? ak bk)
		(=? av bv)))
	 (define (hash-proc a h)
	   (match-define (N ak av ap al ar) a)
	   (+ (eq-hash-code al)
	      (eq-hash-code ar)
	      (h ap)
	      (h ak)
	      (h av)))
	 (define (hash2-proc a h)
	   (match-define (N ak av ap al ar) a)
	   (bitwise-xor (eq-hash-code al)
			(eq-hash-code ar)
			(h ap)
			(h ak)
			(h av)))])

(struct L () #:transparent)

(struct treap (order root size) #:transparent)

;; The singleton "empty" leaf sentinel
(define L0 (L))

(define (treap-empty o) (treap o L0 0))

(define (treap-empty? t) (zero? (treap-size t)))

(define (default-priority key)
  ;; Loosely based on a restriction of murmur32 v3
  (define c1 #xcc9e2d51)
  (define c2 #x1b873593)
  (define r1 15)
  (define r2 13)
  (define m 5)
  (define n #xe6546b64)
  (define k (* (equal-hash-code key) c1))
  (define hash0 (* c2 (bitwise-ior (arithmetic-shift k r1) (arithmetic-shift k (- 32 r1)))))
  (define hash1
    (+ n (* m (bitwise-ior (arithmetic-shift hash0 r2) (arithmetic-shift hash0 (- 32 r2))))))
  (define hash2
    (bitwise-and #xffffffff (* #x85ebca6b (bitwise-xor hash1 (arithmetic-shift hash1 -16)))))
  (define hash3
    (bitwise-and #xffffffff (* #xc2b2ae35 (bitwise-xor hash2 (arithmetic-shift hash2 -13)))))
  (bitwise-xor hash3 (arithmetic-shift hash3 -16)))

(define (treap-insert t key value [priority (default-priority key)])
  (match-define (treap order root oldsize) t)
  (define newsize (+ oldsize 1)) ;; WARNING: mutated below!
  (define newroot
    (let walk ((n root))
      (match n
        [(L)
         (canonicalize (N key value priority L0 L0))]
        [(N k v p left right)
         (case (order key k)
           [(<) (match (walk left)  [(N K V P l r) (rotate K V P k v p l r right)])]
           [(>) (match (walk right) [(N K V P l r) (rotate k v p K V P left l r)])]
           [(=)
            (set! newsize (- newsize 1)) ;; we are *REPLACING* an existing value
            (let merge ((left left) (right right))
              (cond
               [(priority>= priority left)
                (if (priority> priority right)
                    (canonicalize (N key value priority left right))
                    (replace-left right (merge left (N-left right))))]
               [(priority> priority right)
                (replace-right left (merge (N-right left) right))]
               [else
                (if (priority> (N-priority left) right)
                    (replace-right left (merge (N-right left) right))
                    (replace-left right (merge left (N-left right))))]))])])))
  (canonicalize (treap order newroot newsize)))

(define (replace-left n x)
  (canonicalize
   (match n
     [(N k v p _ r)
      (N k v p x r)])))

(define (replace-right n x)
  (canonicalize
   (match n
     [(N k v p l _)
      (N k v p l x)])))

(define (priority> p1 n)
  (match n
    [(L) #t]
    [(N _ _ p2 _ _) (> p1 p2)]))

(define (priority>= p1 n)
  (match n
    [(L) #t]
    [(N _ _ p2 _ _) (>= p1 p2)]))

(define (rotate k1 v1 p1 k2 v2 p2 tl tm tr)
  (if (> p1 p2)
      (canonicalize (N k1 v1 p1 tl (canonicalize (N k2 v2 p2 tm tr))))
      (canonicalize (N k2 v2 p2 (canonicalize (N k1 v1 p1 tl tm)) tr))))

(define (treap-delete t key)
  (match-define (treap order root oldsize) t)
  (define newsize oldsize)
  (define newroot
    (let walk ((n root))
      (match n
        [(L) L0]
        [(N k v p left right)
         (case (order key k)
           [(<) (canonicalize (N k v p (walk left) right))]
           [(>) (canonicalize (N k v p left (walk right)))]
           [(=)
            (set! newsize (- newsize 1)) ;; we found the value to remove
            (let merge ((left left) (right right))
              (cond
               [(L? left) right]
               [(L? right) left]
               [else
                (match-define (N lk lv lp ll lr) left)
                (match-define (N rk rv rp rl rr) right)
                (canonicalize
                 (if (< lp rp)
                     (N lk lv lp ll (merge lr right))
                     (N rk rv rp (merge left rl) rr)))]))])])))
  (canonicalize (treap order newroot newsize)))

(define (treap-get t key [on-missing (lambda () #f)])
  (define order (treap-order t))
  (let walk ((n (treap-root t)))
    (match n
      [(L) (on-missing)]
      [(N k v _ left right)
       (case (order key k)
         [(<) (walk left)]
         [(>) (walk right)]
         [(=) v])])))

(define (treap-keys t #:empty-set [empty-set (set)])
  (let walk ((n (treap-root t)) (acc empty-set))
    (match n
      [(L) acc]
      [(N k _ _ left right) (walk left (walk right (set-add acc k)))])))

(define (treap-values t)
  (let walk ((n (treap-root t)) (acc '()))
    (match n
      [(L) acc]
      [(N k _ _ left right) (walk left (cons k (walk right acc)))])))

(define (treap-to-alist t)
  (let walk ((n (treap-root t)) (acc '()))
    (match n
      [(L) acc]
      [(N k v _ left right) (walk left (cons (cons k v) (walk right acc)))])))

(define (treap-has-key? t key)
  (define order (treap-order t))
  (let walk ((n (treap-root t)))
    (match n
      [(L) #f]
      [(N k v _ left right)
       (case (order key k)
         [(<) (walk left)]
         [(>) (walk right)]
         [(=) #t])])))

(define (treap-height t)
  (let walk ((n (treap-root t)))
    (match n
      [(L) 0]
      [(N _ _ _ l r) (+ 1 (max (walk l) (walk r)))])))
