#lang typed/syndicate/core

(define-constructor (in-stock title quantity)
  #:type-constructor InStock)

(define-constructor (order title client id)
  #:type-constructor Order)

(define-constructor (club-member name)
  #:type-constructor ClubMember)

(define-constructor (book-interest title name answer)
  #:type-constructor BookInterest)

(define-constructor (book-of-the-month title)
  #:type-constructor BookOfTheMonth)

(define-type-alias τc
  (U (ClubMember String)
     (BookInterest String String Bool)
     (Observe (BookInterest String ★/t ★/t))
     (Observe (Observe (BookInterest ★/t ★/t ★/t)))
     (InStock String Int)
     (Observe (InStock String ★/t))
     (Observe (Observe (InStock ★/t ★/t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leader

(define-type-alias LeaderState
  (Tuple String (List String) (Set String) String (Set String) (Set String)))

(define (leader-state-current-title [ls : LeaderState] -> String)
  (select 0 ls))

(define (leader-state-interests [ls : LeaderState] -> (List String))
  (select 1 ls))

(define (leader-state-members [ls : LeaderState] -> (Set String))
  (select 2 ls))

(define (leader-state-conv [ls : LeaderState] -> String)
  (select 3 ls))

(define (leader-state-yays [ls : LeaderState] -> (Set String))
  (select 4 ls))

(define (leader-state-nays [ls : LeaderState] -> (Set String))
  (select 5 ls))

(define (leader-state [current-title : String]
                      [interests : (List String)]
                      [members : (Set String)]
                      [conv : String]
                      [yays : (Set String)]
                      [nays : (Set String)]
                      -> LeaderState)
  (tuple current-title interests members conv yays nays))

(define (update-members [members : (Set String)]
                        [added : (AssertionSet τc)]
                        [retracted : (AssertionSet τc)]
                        -> (Set String))
  (let ([as (project [(club-member (bind name String)) added] name)]
        [rs (project [(club-member (bind name String)) retracted] name)])
    (set-subtract (set-union members (list->set as)) (list->set rs))))

(define (next-book [books : (List String)]
                   [members : (Set String)]
                   -> (Instruction LeaderState τc τc))
  (if (empty? books)
      (begin (displayln "leader fails to find a suitable book")
             (quit))
      (let ([next (first books)]
            [remaining (rest books)])
        (transition (leader-state next remaining members "quote" (set) (set))
                    (list (patch-seq (unsub (in-stock ★ ★))
                                     (unsub (book-interest ★ ★ ★))
                                     (sub (in-stock next ★))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seller

(define-type-alias Inventory (List (Tuple String Int)))

(define (lookup/default [title : String]
                        [inv : Inventory]
                        [default : Int]
                        -> Int)
  (for/fold [answer default]
            [item inv]
    (if (equal? title (select 0 item))
        (select 1 item)
        answer)))

(define (answer-inquiries [e : (AssertionSet τc)]
                          [inventory : Inventory]
                          -> (Patch (InStock String Int) (U)))
  (patch-seq*
   (project [(observe (in-stock (bind title String) discard)) e]
     (assert (in-stock title (lookup/default title inventory 0))))))

(define (make-book-seller [initial-inventory : Inventory] -> (Actor τc))
  (actor τc
    (lambda ([e : (Event τc)]
             [inv : Inventory])
      (transition inv (list (answer-inquiries (patch-added e) inv))))
    initial-inventory
    (make-assertion-set (observe (observe (in-stock ★ ★))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Members

(define (make-club-member [name : String] [preferences : (List String)] -> (Actor τc))
  (actor τc
   (lambda ([e : (Event τc)]
            [s : ★/t])
     (let ([answers
            (project [(observe (book-interest (bind title String) discard discard)) (patch-added e)]
              (patch (make-assertion-set (book-interest title name (member? title preferences)))
                     (make-assertion-set)))])
       (transition s answers)))
   #f
   (make-assertion-set (club-member name)
                       (observe (observe (book-interest ★ ★ ★))))))