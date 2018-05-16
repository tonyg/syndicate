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
     (Observe (ClubMember ★/t))
     (BookInterest String String Bool)
     (Observe (BookInterest String ★/t ★/t))
     (Observe (Observe (BookInterest ★/t ★/t ★/t)))
     (InStock String Int)
     (Observe (InStock String ★/t))
     (Observe (Observe (InStock ★/t ★/t)))
     (BookOfTheMonth String)
     (Observe (BookOfTheMonth ★/t))))

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

(define (leader-learns [quantity : Int]
                       [title : String]
                       -> (Tuple))
  (displayln "leader learns that there are")
  (displayln quantity)
  (displayln "copies of")
  (displayln title)
  (tuple))

(define (respond-to-quotes [added : (AssertionSet τc)]
                           [title : String]
                           [interests : (List String)]
                           [members : (Set String)]
                           [changed? Bool]
                           -> (Instruction LeaderState τc τc))
  (let ([answers (project [(in-stock title (bind n Int)) added] n)])
    (if (empty? answers)
        (if changed?
            (transition (leader-state title interests members "quote" (set) (set)) (list))
            idle)
        (let ([quantity (first answers)])
          (leader-learns quantity title)
          (if (<= quantity (set-count members))
              (begin (displayln "there aren't enough copies to go around")
                     (next-book interests members))
              (transition (leader-state title interests members "poll" (set) (set))
                          (list (sub (book-interest title ★ ★)))))))))

(define (respond-to-interests [added : (AssertionSet τc)]
                              [title : String]
                              [books : (List String)]
                              [members : (Set String)]
                              [yays : (Set String)]
                              [nays : (Set String)]
                              -> (Instruction LeaderState τc τc))
  (let ([yups (set-union yays (list->set (project [(book-interest title (bind name String) #t) added]
                                                  name)))]
        [nups (set-union nays (list->set (project [(book-interest title (bind name String) #f) added]
                                                  name)))])
    (if (>= (set-count yups) (/ (set-count members) 2))
        (begin (displayln "leader finds enough affirmation for") (displayln title)
               (transition (leader-state title books members "complete" yays nays)
                           (list (patch-seq (assert (book-of-the-month title))
                                            (unsub (book-interest ★ ★ ★))))))
        (if (> (set-count nups) (/ (set-count members) 2))
            (begin (displayln "leader finds enough negative nancys for") (displayln title)
                   (next-book books members))
            (transition (leader-state title books members "poll" yups nups) (list))))))

(define (leader-behavior [e : (Event τc)]
                         [s : LeaderState]
                         -> (Instruction LeaderState τc τc))
  (let* ([added (patch-added e)]
         [retracted (patch-removed e)]
         [title (leader-state-current-title s)]
         [books (leader-state-interests s)]
         [members (leader-state-members s)]
         [state (leader-state-conv s)]
         [yays (leader-state-yays s)]
         [nays (leader-state-nays s)]
         [new-members (update-members members added retracted)]
         [changed? (not (equal? new-members members))])
    (if changed?
        (begin (displayln "leader knows about") (displayln new-members) #f)
        #f)
    (if (equal? state "quote")
        (respond-to-quotes added title books new-members changed?)
        (if (equal? state "poll")
            (respond-to-interests added title books new-members yays nays)
            idle))))

(define (make-leader [interests : (List String)] -> (Actor τc))
  (let ([first-book (first interests)]
        [books (rest interests)])
    (actor τc
      leader-behavior
      (leader-state first-book books (set) "quote" (set) (set))
      (make-assertion-set (observe (in-stock first-book ★))
                          (observe (club-member ★))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

(dataspace τc
  (list
   (make-book-seller (list (tuple "The Wind in the Willows" 5)
                          (tuple "Catch 22" 2)
                          (tuple "Candide" 3)))
   (make-leader (list "The Wind in the Willows"
                      "Catch 22"
                      "Candide"
                      "Encyclopaedia Brittannica"))
   (make-club-member "tony" (list "Candide"))
   (make-club-member "sam" (list "Encyclopaedia Brittannica" "Candide"))))