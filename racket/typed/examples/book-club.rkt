#lang typed/syndicate

;; Expected Output
;; leader learns that there are 5 copies of The Wind in the Willows
;; tony responds to suggested book The Wind in the Willows: #f
;; sam responds to suggested book The Wind in the Willows: #f
;; leader finds enough negative nancys for The Wind in the Willows
;; leader learns that there are 2 copies of Catch 22
;; leader learns that there are 3 copies of Candide
;; tony responds to suggested book Candide: #t
;; sam responds to suggested book Candide: #t
;; leader finds enough affirmation for Candide

(define-constructor (price v)
  #:type-constructor PriceT
  #:with Price (PriceT Int))

(define-constructor (book-quote title quantity)
  #:type-constructor BookQuoteT
  #:with BookQuote (BookQuoteT String Int))

(define-constructor (club-member name)
  #:type-constructor ClubMemberT
  #:with ClubMember (ClubMemberT String))

(define-constructor (book-interest title client id)
  #:type-constructor BookInterestT
  #:with BookInterest (BookInterestT String String Bool))

(define-constructor (book-of-the-month title)
  #:type-constructor BookOfTheMonthT
  #:with BookOfTheMonth (BookOfTheMonthT String))

(define-type-alias τc
  (U BookQuote
     (Observe (BookQuoteT String ★/t))
     (Observe (Observe★ BookQuoteT))
     ClubMember
     (Observe★ ClubMemberT)
     BookInterest
     (Observe (BookInterestT String ★/t ★/t))
     (Observe (Observe★ BookInterestT))
     BookOfTheMonth
     (Observe★ BookOfTheMonthT)))

(define-type-alias Inventory (List (Tuple String Int)))

(define (lookup [title : String]
                [inv : Inventory] -> Int)
  (for/fold ([stock : Int 0])
            ([item inv])
    (if (equal? title (select 0 item))
        (select 1 item)
        stock)))

(define-type-alias seller-role
  (Role (seller)
   (Reacts (Asserted (Observe (BookQuoteT String ★/t)))
           (Role (_)
                 ;; nb no mention of retracting this assertion
                 (Shares (BookQuoteT String Int))))))
(export-type "seller-role.rktd" seller-role)

(define (spawn-seller [inventory : Inventory])
  (spawn #:type τc
    (export-roles "seller-impl.rktd"
    (lift+define-role seller-impl
    (start-facet seller
      (field [books Inventory inventory])

      ;; Give quotes to interested parties.
      (during (observe (book-quote $title _))
        ;; TODO - lookup
        (assert (book-quote title (lookup title (! books))))))))))

(define-type-alias leader-role
  (Role (leader)
        (Reacts (Asserted (BookQuoteT String Int))
                (Role (poll)
                      (Reacts (Asserted (BookInterestT String String Bool))
                              ;; this is actually implemented indirectly through dataflow
                              (Branch (Stop leader
                                            (Role (_)
                                                  (Shares (BookOfTheMonthT String))))
                                      (Stop poll)))))))

(define-type-alias leader-actual
  (Role (get-quotes)
        (Reacts (Asserted (BookQuoteT String (Bind Int)))
                (Stop get-quotes)
                (Role (poll-members)
                      (Reacts OnDataflow
                              (Stop poll-members
                                    (Stop get-quotes))
                              (Stop get-quotes
                                    (Role (announce39)
                                          (Shares (BookOfTheMonthT String)))))
                      (Reacts (Retracted (BookInterestT String (Bind String) Bool)))
                      (Reacts (Asserted (BookInterestT String (Bind String) Bool)))
                      (Reacts (Retracted (BookInterestT String (Bind String) Bool)))
                      (Reacts (Asserted (BookInterestT String (Bind String) Bool)))))
        (Reacts (Retracted (ClubMemberT (Bind String))))
        (Reacts (Asserted (ClubMemberT (Bind String))))))

(define (spawn-leader [titles : (List String)])
  (spawn #:type τc
   (export-roles "leader-impl.rktd"
   (lift+define-role leader-impl
   (start-facet get-quotes
     (field [book-list (List String) (rest titles)]
            [title String (first titles)])
     (define (next-book)
       (cond
         [(empty? (! book-list))
          (printf "leader fails to find a suitable book\n")
          (stop get-quotes)]
         [#t
          (:= title (first (! book-list)))
          (:= book-list (rest (! book-list)))]))

     ;; keep track of book club members
     (define/query-set members (club-member $name) name
         #;#:on-add #;(printf "leader acknowledges member ~a\n" name))

     (on (asserted (book-quote (! title) $quantity))
         (printf "leader learns that there are ~a copies of ~a\n" quantity (! title))
         (cond
           [(< quantity (+ 1 (set-count (! members))))
            ;; not enough in stock for each member
            (next-book)]
           [#t
            ;; find out if at least half of the members want to read the book
            (start-facet poll-members
             (define/query-set yays (book-interest (! title) $name #t) name)
             (define/query-set nays (book-interest (! title) $name #f) name)
             (on (asserted (book-interest (! title) $name _))
                 ;; count the leader as a 'yay'
                 (when (>= (set-count (! yays))
                           (/ (set-count (! members)) 2))
                   (printf "leader finds enough affirmation for ~a\n" (! title))
                   (stop get-quotes
                         (start-facet announce
                                      (assert (book-of-the-month (! title))))))
                 (when (> (set-count (! nays))
                          (/ (set-count (! members)) 2))
                   (printf "leader finds enough negative nancys for ~a\n" (! title))
                   (stop poll-members (next-book))))
             ;; begin/dataflow is a problem for simulation checking
             #;(begin/dataflow
               ;; count the leader as a 'yay'
               (when (>= (set-count (! yays))
                         (/ (set-count (! members)) 2))
                 (printf "leader finds enough affirmation for ~a\n" (! title))
                 (stop get-quotes
                       (start-facet announce
                         (assert (book-of-the-month (! title))))))
               (when (> (set-count (! nays))
                        (/ (set-count (! members)) 2))
                 (printf "leader finds enough negative nancys for ~a\n" (! title))
                 (stop poll-members (next-book)))))])))))))

(define-type-alias member-role
  (Role (member)
        (Shares (ClubMemberT String))
        ;; should this be the type of the pattern? or lowered to concrete types?
        (Reacts (Asserted (Observe (BookInterestT String ★/t ★/t)))
                (Role (_)
                      (Shares (BookInterestT String String Bool))))))

(define (spawn-club-member [name : String]
                           [titles : (List String)])
  (spawn #:type τc
   (export-roles "member-impl.rktd"
   (lift+define-role member-impl
   (start-facet member
     ;; assert our presence
     (assert (club-member name))
     ;; respond to polls
     (during (observe (book-interest $title _ _))
       (define answer (member? title titles))
       (printf "~a responds to suggested book ~a: ~a\n" name title answer)
       (assert (book-interest title name answer))))))))

(run-ground-dataspace τc
  (spawn-seller (list (tuple "The Wind in the Willows" 5)
                      (tuple "Catch 22" 2)
                      (tuple "Candide" 3)))
  (spawn-leader (list "The Wind in the Willows"
                      "Catch 22"
                      "Candide"
                      "Encyclopaedia Brittannica"))
  (spawn-club-member "tony" (list "Candide"))
  (spawn-club-member "sam" (list "Encyclopaedia Brittannica" "Candide")))

(module+ test
  (verify-actors (And (Eventually (A BookQuote))
                      (Always (Implies (A (Observe (BookQuoteT String ★/t)))
                                       (Eventually (A BookQuote))))
                      (Always (Implies (A (Observe (BookInterestT String ★/t ★/t)))
                                       (Eventually (A BookInterest)))))
                 leader-impl
                 seller-impl
                 member-impl))

(module+ test
  (check-simulates leader-impl leader-impl)
  (check-has-simulating-subgraph leader-impl leader-role)
  (check-simulates seller-impl seller-impl)
  (check-has-simulating-subgraph seller-impl seller-role))
