#lang syndicate/actor
;; An extended two-buyer book-purchase protocol, based loosely on that
;; given in Honda/Yoshida/Carbone 2008, "Multiparty Asynchronous
;; Session Types".

;; SAMPLE OUTPUT:
;;---------------------------------------------------------------------------
;; A learns that the price of "Catch 22" is 2.22
;; A makes an offer to split the price of "Catch 22", contributing 1.11
;; B is being asked to contribute 1.11 toward "Catch 22" at price 2.22
;; B accepts the offer, leaving them with 3.8899999999999997 remaining funds
;; A learns that the split-proposal for "Catch 22" was accepted, leaving them with 33.89 remaining
;; The order for "Catch 22" has id 10001483, and will be delivered on March 9th
;; A learns that "Encyclopaedia Brittannica" is out-of-stock.
;; A learns that the price of "Candide" is 34.95
;; A makes an offer to split the price of "Candide", contributing 17.475
;; B is being asked to contribute 17.475 toward "Candide" at price 34.95
;; B hasn't enough funds (3.8899999999999997 remaining)
;; A learns that the split-proposal for "Candide" was rejected
;; A makes an offer to split the price of "Candide", contributing 26.212500000000002
;; B is being asked to contribute 8.7375 toward "Candide" at price 34.95
;; B hasn't enough funds (3.8899999999999997 remaining)
;; A learns that the split-proposal for "Candide" was rejected
;; A makes an offer to split the price of "Candide", contributing 30.581250000000004
;; B is being asked to contribute 4.368749999999999 toward "Candide" at price 34.95
;; B hasn't enough funds (3.8899999999999997 remaining)
;; A learns that the split-proposal for "Candide" was rejected
;; A makes an offer to split the price of "Candide", contributing 32.765625
;; B is being asked to contribute 2.184375000000003 toward "Candide" at price 34.95
;; B accepts the offer, leaving them with 1.7056249999999968 remaining funds
;; A learns that the split-proposal for "Candide" was accepted, leaving them with 1.1243750000000006 remaining
;; The order for "Candide" has id 10001484, and will be delivered on March 9th
;; A learns that the price of "The Wind in the Willows" is 3.95
;; A makes an offer to split the price of "The Wind in the Willows", contributing 1.1243750000000006
;; B is being asked to contribute 2.8256249999999996 toward "The Wind in the Willows" at price 3.95
;; B hasn't enough funds (1.7056249999999968 remaining)
;; A learns that the split-proposal for "The Wind in the Willows" was rejected
;; A does not have enough money for "The Wind in the Willows".
;; A has bought everything they wanted!
;;---------------------------------------------------------------------------

;; Role: SELLER
;;  - when interest in (book-quote $title _) appears,
;;    asserts (book-quote title (Option Float)), #f meaning not available,
;;    and otherwise an asking-price.
;;  - when interest in (order $title $offer-price _ _) appears,
;;    asserts (order title offer-price #f #f) for "no sale", otherwise
;;    (order title offer-price PositiveInteger String), an accepted sale.

;; Role: BUYER
;;  - observes (book-quote title $price) to learn prices.
;;  - observes (order title offer-price $id $delivery-date) to make orders.

;; Role: SPLIT-PROPOSER
;;  - observes (split-proposal title asking-price contribution $accepted?)
;;    to make a split-proposal and learn whether it was accepted or not.

;; Role: SPLIT-DISPOSER
;;  - when interest in (split-proposal $title $asking-price $contribution _)
;;    appears, asserts (split-proposal title asking-price contribution #t)
;;    to indicate they are willing to go through with the deal, in which case
;;    they then perform the role of BUYER for title/asking-price, or asserts
;;    (split-proposal title asking-price contribution #f) to indicate they
;;    are unwilling to go through with the deal.

(struct book-quote (title price) #:prefab) ;; Assertion
(struct order (title price id delivery-date) #:prefab) ;; Assertion

(struct split-proposal (title price contribution accepted) #:prefab) ;; Assertion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Utility syntax. Under consideration for possible addition to actor.rkt.
;;
(define-syntax while-relevant-assert
  (syntax-rules ()
    [(_ P)
     (begin (stop-when (retracted (observe P)))
            (assert P))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SELLER
;;
(define (seller)
  (spawn (field [books (hash "The Wind in the Willows" 3.95
                             "Catch 22" 2.22
                             "Candide" 34.95)]
                [next-order-id 10001483])

         ;; Give quotes to interested parties.
         ;;
         (during (observe (book-quote $title _))
                 (assert (book-quote title (hash-ref (books) title #f))))

         ;; Respond to order requests.
         ;;
         (on (asserted (observe (order $title $offer-price _ _)))
             (define asking-price (hash-ref (books) title #f))
             (cond

               [(or (not asking-price) (< offer-price asking-price))
                ;; We cannot sell a book we do not have, and we will not sell for less
                ;; than our asking price.
                ;;
                (react (while-relevant-assert (order title offer-price #f #f)))]

               [else
                ;; Allocate an order ID.
                ;;
                (define order-id (next-order-id))
                (next-order-id (+ order-id 1))

                ;; Remove the book from our shelves.
                ;;
                (books (hash-remove (books) title))

                ;; Tell the ordering party their order ID and delivery date.
                ;;
                (spawn
                 (while-relevant-assert
                  (order title offer-price order-id "March 9th")))]))))

;; Serial SPLIT-PROPOSER
;;
(define (buyer-a)

  ;; Attempt to entice some SPLIT-DISPOSER to purchase each title in titles, one after another.
  ;;
  (define (try-to-buy titles budget)
    (match titles
      ['()
       (log-info "A has bought everything they wanted!")]
      [(cons title remaining-titles)

       ;; First, retrieve a quote for the title, and analyze the result.
       ;;
       (match (react/suspend (yield)
                (stop-when (asserted (book-quote title $price)) (yield price)))
         [#f
          (log-info "A learns that ~v is out-of-stock." title)
          (try-to-buy remaining-titles budget)]

         [price
          (log-info "A learns that the price of ~v is ~a" title price)

          ;; Next, repeatedly make split offers to a SPLIT-DISPOSER until either one is
          ;; accepted, or the contribution from the SPLIT-DISPOSER becomes pointlessly small.
          ;;
          (let try-to-split ((contribution (min budget (/ price 2))))
            (cond
              [(> contribution (- price 0.10))
               ;; Not worth bothering to split the price. May as well buy it ourselves.
               ;; TODO: could perform BUYER here
               ;;
               (log-info "A gives up on ~v." title)
               (try-to-buy remaining-titles budget)]
              [(> contribution budget)
               ;; Don't have enough money
               (log-info "A does not have enough money for ~v." title)
               (try-to-buy remaining-titles budget)]

              [else
               ;; Make our proposal, and wait for a response.
               ;;
               (log-info "A makes an offer to split the price of ~v, contributing ~a"
                      title
                      contribution)
               (react
                (stop-when (asserted (split-proposal title price contribution #t))
                           (define remaining-budget (- budget contribution))
                           (log-info "A learns that the split-proposal for ~v was accepted, leaving them with ~v remaining" title remaining-budget)
                           (try-to-buy remaining-titles remaining-budget))
                (stop-when (asserted (split-proposal title price contribution #f))
                           (log-info "A learns that the split-proposal for ~v was rejected" title)
                           (try-to-split (+ contribution (/ (- price contribution) 2)))))]))])]))

  (spawn* (try-to-buy (list "Catch 22"
                            "Encyclopaedia Brittannica"
                            "Candide"
                            "The Wind in the Willows")
                      35.00)))

;; Serial SPLIT-DISPOSER
;;
(define (buyer-b)
  (spawn ;; This actor maintains a record of the amount of money it has to spend.
         ;;
         (field [funds 5.00])

         (on (asserted (observe (split-proposal $title $price $their-contribution _)))

             (define my-contribution (- price their-contribution))
             (log-info "B is being asked to contribute ~a toward ~v at price ~a"
                       my-contribution
                       title
                       price)

             (cond
               [(> my-contribution (funds))
                (log-info "B hasn't enough funds (~a remaining)" (funds))
                (react (while-relevant-assert (split-proposal title price their-contribution #f)))]

               [else

                ;; Spawn a small actor (TODO: when we revise actor.rkt's implementation style,
                ;; this could perhaps be a facet rather than a full actor) to handle the
                ;; actual purchase now that we have agreed on a split.
                ;;
                (spawn* (define-values (order-id delivery-date)
                          (react/suspend (yield)
                                         ;; While we are in this state, waiting for order confirmation, take
                                         ;; the opportunity to signal to our SPLIT-PROPOSER that we accepted
                                         ;; their proposal.
                                         ;;
                                         (assert (split-proposal title price their-contribution #t))

                                         (stop-when (asserted (order title price $id $date))
                                                    ;; We have received order confirmation from the SELLER.
                                                    ;;
                                                    (yield id date))))
                       (log-info "The order for ~v has id ~a, and will be delivered on ~a"
                                 title
                                 order-id
                                 delivery-date))

                ;; Meanwhile, update our records of our available funds, and continue to wait
                ;; for more split-proposals to arrive.
                ;;
                (define remaining-funds (- (funds) my-contribution))
                (log-info "B accepts the offer, leaving them with ~a remaining funds"
                          remaining-funds)
                (funds remaining-funds)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Starting configuration:
;;
(seller)
(buyer-a)
(buyer-b)
