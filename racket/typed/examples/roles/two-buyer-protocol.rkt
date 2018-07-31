#lang typed/syndicate/roles

;; Expected Output
;; Completed Order:
;; Catch 22
;; 10001483
;; March 9th

(define-constructor (price v)
  #:type-constructor PriceT
  #:with Price (PriceT Int))

(define-constructor (out-of-stock)
  #:type-constructor OutOfStockT
  #:with OutOfStock (OutOfStockT))

(define-type-alias QuoteAnswer
  (U Price OutOfStock))

(define-constructor (quote title answer)
  #:type-constructor QuoteT
  #:with Quote (QuoteT String QuoteAnswer)
  #:with QuoteRequest (Observe (QuoteT String ★/t))
  #:with QuoteInterest (Observe (QuoteT ★/t ★/t)))

(define-constructor (split-proposal title price contribution accepted)
  #:type-constructor SplitProposalT
  #:with SplitProposal (SplitProposalT String Int Int Bool)
  #:with SplitRequest (Observe (SplitProposalT String Int Int ★/t))
  #:with SplitInterest (Observe (SplitProposalT ★/t ★/t ★/t ★/t)))

(define-constructor (order-id id)
  #:type-constructor OrderIdT
  #:with OrderId (OrderIdT Int))

(define-constructor (delivery-date date)
  #:type-constructor DeliveryDateT
  #:with DeliveryDate (DeliveryDateT String))

(define-type-alias (Maybe t)
  (U t Bool))

(define-constructor (order title price id delivery-date)
  #:type-constructor OrderT
  #:with Order (OrderT String Int (Maybe OrderId) (Maybe DeliveryDate))
  #:with OrderRequest (Observe (OrderT String Int ★/t ★/t))
  #:with OrderInterest (Observe (OrderT ★/t ★/t ★/t ★/t)))

(define-type-alias ds-type
  (U ;; quotes
   Quote
   QuoteRequest
   (Observe QuoteInterest)
   ;; splits
   SplitProposal
   SplitRequest
   (Observe SplitInterest)
   ;; orders
   Order
   OrderRequest
   (Observe OrderInterest)))

(dataspace ds-type

;; seller
(spawn ds-type
  (start-facet _
    (fields [book (Tuple String Int) (tuple "Catch 22" 22)]
            [next-order-id Int 10001483])
    (on (asserted (observe (quote (bind title String) discard)))
        (start-facet x
          (fields)
          (on (retracted (observe (quote title discard)))
              (stop x))
          (match title
            ["Catch 22"
             (assert (quote title (price 22)))]
            [discard
             (assert (quote title (out-of-stock)))])))
    (on (asserted (observe (order (bind title String) (bind offer Int) discard discard)))
        (start-facet x
          (fields)
          (on (retracted (observe (order title offer discard discard)))
              (stop x))
          (let ([asking-price 22])
            (if (and (equal? title "Catch 22") (>= offer asking-price))
                (let ([id (ref next-order-id)])
                  (set! next-order-id (+ 1 id))
                  (assert (order title offer (order-id id) (delivery-date "March 9th"))))
                (assert (order title offer #f #f))))))))

;; buyer A
(spawn ds-type
  (start-facet buyer
    (fields [title String "Catch 22"]
            [budget Int 1000])
    (on (asserted (quote (ref title) (bind answer QuoteAnswer)))
        (match answer
          [(out-of-stock)
           (stop buyer)]
          [(price (bind amount Int))
           (start-facet negotiation
             (fields [contribution Int (/ amount 2)])
             (on (asserted (split-proposal (ref title) amount (ref contribution) (bind accept? Bool)))
                 (if accept?
                     (stop buyer)
                     (if (> (ref contribution) (- amount 5))
                         (stop negotiation (displayln "negotiation failed"))
                         (set! contribution
                               (+ (ref contribution) (/ (- amount (ref contribution)) 2)))))))]))))

;; buyer B
(spawn ds-type
  (start-facet buyer-b
    (fields [funds Int 5])
    (on (asserted (observe (split-proposal (bind title String) (bind price Int) (bind their-contribution Int) discard)))
        (let ([my-contribution (- price their-contribution)])
          (cond
            [(> my-contribution (ref funds))
             (start-facet decline
               (fields)
               (assert (split-proposal title price their-contribution #f))
               (on (retracted (observe (split-proposal title price their-contribution discard)))
                   (stop decline)))]
            [#t
             (start-facet accept
               (fields)
               (assert (split-proposal title price their-contribution #t))
               (on (retracted (observe (split-proposal title price their-contribution discard)))
                   (stop accept))
               (on start
                   (spawn ds-type
                     (start-facet purchase
                       (fields)
                       (on (asserted (order title price (bind order-id? (Maybe OrderId)) (bind delivery-date? (Maybe DeliveryDate))))
                           (match (tuple order-id? delivery-date?)
                             [(tuple (order-id (bind id Int)) (delivery-date (bind date String)))
                              ;; complete!
                              (begin (displayln "Completed Order:")
                                     (displayln title)
                                     (displayln id)
                                     (displayln date)
                                     (stop purchase))]
                             [discard
                              (begin (displayln "Order Rejected")
                                     (stop purchase))]))))))])))))
)