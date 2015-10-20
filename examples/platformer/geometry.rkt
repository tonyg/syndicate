#lang racket

(provide
 (struct-out posn)
 (struct-out circle)
 (struct-out line)
 (struct-out line-segment)
 (struct-out rect)
 intersection-circle-line
 line-through-points
 point-distance
 point-between?
 overlapping-rects?
 move-rect)


(struct posn (x y) #:transparent)

(struct circle (center radius) #:transparent)

;; ax + by = c
(struct line (a b c) #:transparent)

;; line * posn * posn
;; where p1 and p2 are both on line and p1-x <= p2-x
(struct line-segment (line p1 p2) #:transparent)

;; posn * pos-int * pos-int
(struct rect (top-left width height) #:transparent)

;; solve a quadratic formula of the form Ax^2 + Bx + C = 0
;; evaluates to #f if there is no solution
;; evalutes to n if n is the only solution
;; evaluates to (cons n m) with n >= m if there are two solutions
(define (solve-quadratic A B C)
  (define discriminant (- (expt B 2) (* 4 A C)))
  (cond
    [(< discriminant 0) #f]
    [(equal? discriminant 0) (/ (- B) (* 2 A))]
    [else (cons
           (/ (+ (- B) (sqrt discriminant)) (* 2 A))
           (/ (- (- B) (sqrt discriminant)) (* 2 A)))]))

(module+ test
  (require rackunit)
  ;; x^2 - 2x - 3
  (check-equal? (solve-quadratic 1 -2 -3)
                (cons 3 (- 1)))
  ;; x^2 + x - 4
  (check-equal? (solve-quadratic 1 1 -4)
                (cons (/ (+ -1 (sqrt 17)) 2)
                      (/ (- -1 (sqrt 17)) 2)))
  ;; x^2 - 3x - 4
  (check-equal? (solve-quadratic 1 -3 -4)
                (cons 4 -1))
  ;; x^2 - 4
  (check-equal? (solve-quadratic 1 0 -4)
                (cons 2 -2))
  ;; 6x^2 + 11x - 35
  (check-equal? (solve-quadratic 6 11 -35)
                (cons 5/3 -7/2))
  ;; x^2 - 3x + 29
  (check-equal? (solve-quadratic 1 -3 29)
                #f)
  ;; x^2 - 2x + 1
  (check-equal? (solve-quadratic 1 -2 1)
                1))

;; line int -> (U int #f)
;; calculate the y value from an x value on a line, if it exists
(define (line-y-at-x l x)
  (match-define (line a b c) l)
  (if (zero? b)
      #f
      (/ (- c (* a x)) b)))

(module+ test
  ;; y = 6/5x + 14 => -6/5x + y = 14
  (define line0 (line -6/5 1 14))
  (check-equal? (line-y-at-x line0 0)
                14)
  (check-equal? (line-y-at-x line0 4)
                (+ 24/5 14))
  (check-equal? (line-y-at-x line0 -10)
                2)
  ;; y = 4 => 0x + y = 4
  (define line1 (line 0 1 4))
  (check-equal? (line-y-at-x line1 921312941)
                4)
  (check-equal? (line-y-at-x line1 0)
                4))

;; compute the y value(s) of a circle for a given x value
;; returns #f if there are none
;; returns y if there is exactly one
;; returns (cons y1 y2) if there are two, with y1 > y2
(define (circle-y-at-x c x)
  (match-define (circle (posn x0 y0) r) c)
  (define y-y0^2 (- (expt r 2) (expt (- x x0) 2)))
  (cond
    [(< y-y0^2 0) #f]
    [(equal? y-y0^2 0) (+ y0 (sqrt y-y0^2))]
    [else (cons (+ y0 (sqrt y-y0^2))
                (- y0 (sqrt y-y0^2)))]))

(module+ test
  ;; x^2 + y^2 = 1
  (define circle0 (circle (posn 0 0) 1))
  (check-equal? (circle-y-at-x circle0 0)
                (cons 1 -1))
  (check-equal? (circle-y-at-x circle0 1)
                0)
  (check-equal? (circle-y-at-x circle0 2)
                #f))

;; compute the x value(s) of a circle for a given y value
;; returns #f if there are none
;; returns x if there is exactly one
;; returns (cons x1 x2) if there are two, with x1 > x2
(define (circle-x-at-y c y)
  (match-define (circle (posn x0 y0) r) c)
  (circle-y-at-x (circle (posn y0 x0) r) y))

(module+ test
  ;; (x - 3)^2 + (y - 4)^2 = 3^2
  (define circle1 (circle (posn 3 4) 3))
  (check-equal? (circle-x-at-y circle1 4)
                (cons 6 0))
  (check-equal? (circle-x-at-y circle1 1)
                3)
  (check-equal? (circle-x-at-y circle1 7)
                3))


;; compute the intersecting points of a circle c and line l
;; if there are no such points return #f
;; if there is one such point (x, y), return (posn x y)
;; if there are two such points (x1, y1) (x2, y2), return (cons (posn x1 y1) (posn x2 y2))
(define (intersection-circle-line circ l)
  (match-define (circle (posn x0 y0) r) circ)
  (match-define (line a b c) l)
  (cond
    [(zero? a) ;; horizontal line: y = c/b
     (define c/b (/ c b))
     (match (circle-x-at-y circ c/b)
       [#f #f]
       [(cons x1 x2) (cons (posn x1 c/b) (posn x2 c/b))]
       [x (posn x c/b)])]
    [(zero? b) ;; vertical line: x = c/a
     (define c/a (/ c a))
     (match (circle-y-at-x circ c/a)
       [#f #f]
       [(cons y1 y2) (cons (posn c/a y1) (posn c/a y2))]
       [y (posn c/a y)])]
    [else
     (define slope (/ (- a) b))
     (define y-int (/ c b))
     (define A (+ 1 (expt slope 2)))
     (define B (* 2 (- (* slope y-int) (* slope y0) x0)))
     (define C (+ (expt y0 2) (- (expt r 2)) (expt x0 2) (* -2 y-int y0) (expt y-int 2)))
     (match (solve-quadratic A B C)
       [#f #f]
       [(cons x1 x2) (cons (posn x1 (line-y-at-x l x1))
                           (posn x2 (line-y-at-x l x2)))]
       [x (posn x (line-y-at-x x))])]))

(module+ test
  (define unit-circle (circle (posn 0 0) 1))
  (define x=0 (line 1 0 0))
  (define y=0 (line 0 1 0))
  (check-equal? (intersection-circle-line unit-circle x=0)
                (cons (posn 0 1) (posn 0 -1)))
  (check-equal? (intersection-circle-line unit-circle y=0)
                (cons (posn 1 0) (posn -1 0)))
  (define y=x (line -1 1 0))
  (check-equal? (intersection-circle-line unit-circle y=x)
                (cons (posn (/ (sqrt 2) 2) (/ (sqrt 2) 2))
                      (posn (- (/ (sqrt 2) 2)) (- (/ (sqrt 2) 2)))))
  (define L (line 1.0 0.0 340.0))
  (define C (circle (posn 360 20) 40))
  (check-equal? (intersection-circle-line C L)
                (cons (posn 340.0 54.64101615137755) (posn 340.0 -14.64101615137755))))

;; Reduce the coefficients of a line to the smallest integer equivalents
;; and ensure that the y coefficient (b) is non-negative
(define (normalize-line l)
  (match-define (line a b c) l)
  (define d (gcd a b c))
  (define s (if (< b 0) -1 1))
  (line (/ a d s) (/ b d s) (/ c d s)))

;; construct the line passing through points p1 and p2
(define (line-through-points p1 p2)
  (match-define (cons (posn x1 y1) (posn x2 y2)) (cons p1 p2))
  (define y2-y1 (- y2 y1))
  (define x2-x1 (- x2 x1))
  (normalize-line (line (- y2-y1) x2-x1 (- (* y1 x2-x1) (* x1 y2-y1)))))

(module+ test
  (check-equal? (line-through-points (posn 0 0) (posn 1 0))
                (line 0 1 0))
  (check-equal? (line-through-points (posn 0 0) (posn 1 1))
                y=x)
  (check-equal? (line-through-points (posn -100 -100) (posn 12345 12345))
                y=x)
  (define y=-x (line 1 1 0))
  (check-equal? (line-through-points (posn -5 5) (posn 4 -4))
                y=-x))

;; calculate the distance between two points
(define (point-distance p1 p2)
  (match-define (cons (posn x1 y1) (posn x2 y2)) (cons p1 p2))
  (sqrt (+ (expt (- x1 x2) 2)
           (expt (- y1 y2) 2))))

(module+ test
  (check-equal? (point-distance (posn 0 0) (posn 0 0))
                0)
  (check-equal? (point-distance (posn 1 1) (posn 0 1))
                1)
  (check-equal? (point-distance (posn -2 4) (posn -2 0))
                4)
  (check-= (point-distance (posn -1 -1) (posn 1 1))
           (/ 4 (sqrt 2))
           .0002))

;; calculate if p is between p1 p2
(define (point-between? p1 p2 p)
  (match-define (list (posn x0 y0) (posn x1 y1) (posn x y)) (list p1 p2 p))
  (and (>= x (min x0 x1))
       (<= x (max x0 x1))
       (>= y (min y0 y1))
       (<= y (max y0 y1))))

(module+ test
  (check-true (point-between? (posn 0 0) (posn 0 0) (posn 0 0)))
  (check-true (point-between? (posn -1 -1) (posn 1 1) (posn 0 0)))
  (check-true (point-between? (posn -1 -1) (posn 1 1) (posn 0 1)))
  (check-false (point-between? (posn -1 -1) (posn 1 1) (posn 2 0)))
  (check-true (point-between? (posn 0 -3) (posn 0 4) (posn 0 0))))

;; line  line -> (U posn line #f)
;; find the intersection of two lines, if it exists
(define (intersection-lines l1 l2)
  (match-define (cons (line a1 b1 c1) (line a2 b2 c2)) (cons l1 l2))
  (cond
    [(equal? l1 l2) l1]
    [(zero? b1)
     (if (zero? b2)
         #f
         (let ([c1/a1 (/ c1 a1)])
           (posn c1/a1 (line-y-at-x l2 c1/a1))))]
    [else
     (define b2c1/b1 (/ (* b2 c1) b1))
     (define a1b2/b1 (/ (* a1 b2) b1))
     (define a2-a1b2/b1 (- a2 a1b2/b1))
     (if (zero? a2-a1b2/b1)
         #f
         (let [(x-int (/ (- c2 b2c1/b1) a2-a1b2/b1))]
           (posn x-int (line-y-at-x l1 x-int))))]))

(module+ test
  #;(define y=x (line -1 1 0))
  #;(define y=0 (line 0 1 0))
  (define y=3 (line 0 1 3))
  #;(define x=0 (line 1 0 0))
  (define x=-12 (line 1 0 -12))
  #;(define y=-x (line 1 1 0))
  (define y=2x-2 (line -2 1 -2))
  (define y=x-3 (line -1 1 -3))
  (check-equal? (intersection-lines y=x y=0)
                (posn 0 0))
  (check-equal? (intersection-lines y=x y=3)
                (posn 3 3))
  (check-equal? (intersection-lines y=x x=0)
                (posn 0 0))
  (check-equal? (intersection-lines x=0 y=x)
                (posn 0 0))
  (check-equal? (intersection-lines y=x x=-12)
                (posn -12 -12))
  (check-equal? (intersection-lines y=0 x=0)
                (posn 0 0))
  (check-equal? (intersection-lines y=0 x=-12)
                (posn -12 0))
  (check-equal? (intersection-lines y=3 x=0)
                (posn 0 3))
  (check-equal? (intersection-lines y=3 x=-12)
                (posn -12 3))
  (check-false (intersection-lines y=0 y=3))
  (check-false (intersection-lines x=0 x=-12))
  (check-false (intersection-lines y=x y=x-3))
  (check-equal? (intersection-lines y=x y=2x-2)
                (posn 2 2))
  (check-equal? (intersection-lines y=x y=x)
                y=x)
  (check-equal? (intersection-lines x=0 x=0)
                x=0)
  (check-equal? (intersection-lines y=0 y=0)
                y=0))

;; line-segment line-segment -> (U posn line-segment #f)
;; find the intersection of two line-segments, if it exists
(define (intersection-line-segments s1 s2)
  (match-define (cons (line-segment l1 p11 p21) (line-segment l2 p12 p22)) (cons s1 s2))
  (if (equal? l1 l2)
      (match-let ([(list _ p1 p2 _) (sort (list p11 p21 p12 p22)
                                          (lambda (p1 p2)
                                            (if (equal? (posn-x p1) (posn-x p2))
                                                (< (posn-y p1) (posn-y p2))
                                                (< (posn-x p1) (posn-x p2)))))])
        (if (equal? p1 p2)
            p1
            (line-segment l1 p1 p2)))
      (match (intersection-lines l1 l2)
        [#f #f]
        [p (if (and (point-between? p11 p21 p)
                    (point-between? p12 p22 p))
               p
               #f)])))

(module+ test
  (define seg0 (line-segment y=x (posn -1 -1) (posn 1 1)))
  (define seg1 (line-segment x=0 (posn 0 -3) (posn 0 4)))
  (define seg2 (line-segment x=0 (posn 0 1) (posn 0 8)))
  (define seg3 (line-segment y=x (posn 1/2 1/2) (posn 2 2)))
  (define seg4 (line-segment y=x (posn 2 2) (posn 3 3)))
  (define seg5 (line-segment y=-x (posn -3 3) (posn 3 -3)))
  (define seg6 (line-segment y=0 (posn -1 0) (posn 3 0)))
  (define seg7 (line-segment y=0 (posn 2 0) (posn 4 0)))
  (check-equal? (intersection-line-segments seg0 seg0)
                seg0)
  (check-equal? (intersection-line-segments seg0 seg1)
                (posn 0 0))
  (check-equal? (intersection-line-segments seg0 seg2)
                #f)
  (check-equal? (intersection-line-segments seg0 seg3)
                (line-segment y=x (posn 1/2 1/2) (posn 1 1)))
  (check-equal? (intersection-line-segments seg1 seg0)
                (posn 0 0))
  (check-equal? (intersection-line-segments seg1 seg1)
                seg1)
  (check-equal? (intersection-line-segments seg1 seg2)
                (line-segment x=0 (posn 0 1) (posn 0 4)))
  (check-false (intersection-line-segments seg1 seg3))
  (check-equal? (intersection-line-segments seg3 seg4)
                (posn 2 2))
  (check-equal? (intersection-line-segments seg0 seg5)
                (posn 0 0))
  (check-false (intersection-line-segments seg5 seg4))
  (check-equal? (intersection-line-segments seg6 seg7)
                (line-segment y=0 (posn 2 0) (posn 3 0)))
  (check-equal? (intersection-line-segments seg6 seg1)
                (posn 0 0))
  (check-false (intersection-line-segments seg7 seg1))
  (check-false (intersection-line-segments seg6 seg4)))

;; num -> line
;; create a vertical line at the given x
(define (line-x= x)
  (line 1 0 x))

;; num -> line
;; create a horizontal line at the given y
(define (line-y= y)
  (line 0 1 y))

;; rect -> (listof posn)
;; extract the four corners of a rectangle
;; ordered as (top-left top-right bottom-left bottom-right
(define (rect-corners r)
  (match-define (rect (posn x0 y0) w h) r)
  (list (posn x0 y0)
        (posn (+ x0 w) y0)
        (posn x0 (+ y0 h))
        (posn (+ x0 w) (+ y0 h))))

;; rect -> (listof line-segment)
;; extract the four line segments forming a rectangle
(define (rect-line-segments r)
  (match-define (rect (posn x0 y0) w h) r)
  (match-define (list tl tr bl br) (rect-corners r))
  (list (line-segment (line-x= x0) tl bl)
        (line-segment (line-x= (+ x0 w)) tr br)
        (line-segment (line-y= y0) tl tr)
        (line-segment (line-y= (+ y0 h)) bl br)))

;; rect rect -> bool
;; test if two rectangles are overlapping, where the area of overlap is greater than 0,
;; so two rectangles that meet at a corner are not considered to overlap
(define (overlapping-rects? r1 r2)
  (match-define (list (posn tl-x1 tl-y1) _ _ (posn br-x1 br-y1)) (rect-corners r1))
  (match-define (list (posn tl-x2 tl-y2) _ _ (posn br-x2 br-y2)) (rect-corners r2))
  (and (< tl-x1 br-x2)
       (> br-x1 tl-x2)
       (< tl-y1 br-y2)
       (> br-y1 tl-y2)))


(module+ test
  (check-false (overlapping-rects? (rect (posn 0 0) 1 1)
                                   (rect (posn 1 0) 1 1)))
  (check-false (overlapping-rects? (rect (posn 1 1) 2 3)
                                   (rect (posn 5 4) 9 10)))
  (check-true (overlapping-rects? (rect (posn 0 -1) 2 4)
                                  (rect (posn 1 0) 3 5)))
  (check-false (overlapping-rects? (rect (posn 0 0) 2 2)
                                   (rect (posn 3 3) 2 2)))
  (check-true (overlapping-rects? (rect (posn 0 0) 2 2)
                                  (rect (posn 1 1) 2 2)))
  (check-true (overlapping-rects? (rect (posn 0 0) 10 2)
                                  (rect (posn 5 -5) 2 10))))

;; rect num num -> rect
;; move a rectangle a given distance in the x and y directions
(define (move-rect r dx dy)
  (match-define (rect (posn x y) w h) r)
  (rect (posn (+ x dx) (+ y dy)) w h))

(module+ test
  (check-equal? (move-rect (rect (posn 3 4) 5 6) 4 0)
                (rect (posn 7 4) 5 6))
  (check-equal? (move-rect (rect (posn -2 6) 1 9) 0 -6)
                (rect (posn -2 0) 1 9))
  (check-equal? (move-rect (rect (posn 8 1) 2 5) -3 8)
                (rect (posn 5 9) 2 5)))