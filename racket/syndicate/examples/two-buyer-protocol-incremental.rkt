#lang syndicate/core

(require racket/set)

(struct book-quote (title price) #:prefab)
(struct order (title price id delivery-date) #:prefab)

(struct split-proposal (title price contribution accepted) #:prefab)

(define (while-relevant-assert P #:noisy? [noisy #f])
  (define name (format "(while-relevant-assert ~a)" P))
  (actor/stateless
   #:name name
   (lambda (e)
     (match e
       [(patch _ removed)
        (when noisy
          (log-info "~v receives ~v" name (patch->pretty-string e)))
        (if (trie-empty? (trie-project removed (observe P)))
            #f
            (quit))]
       [_ #f]))
   (list (sub (observe P))
         (assert P))))

;; Event -> Inventory -> (Transition Inventory)
(define ((answer-quotes e) inv)
  (match e
    [(patch added removed)
     (define p-a (for-trie/patch ([(observe (book-quote $title _)) added])
                   (assert (book-quote title (hash-ref inv title #f)))))
     (define p-r (for-trie/patch ([(observe (book-quote $title _)) removed])
                   (retract (book-quote title ?))))
     (transition inv (list (patch-seq p-a p-r)))]
    [_ (transition inv '())]))

;; Event -> Inventory -> (Transition Inventory)
(define ((answer-orders e) inv)
  (match e
    [(patch added _)
     (define-values (actions new-inv)
       (for-trie/fold ([actions '()]
                       [inv inv])
                      ([(observe (order $title $offer-price _ _)) added])
         (define asking-price (hash-ref inv title #f))
         (cond
           [(or (not asking-price) (< offer-price asking-price))
            ;; We cannot sell a book we do not have, and we will not sell for less
            ;; than our asking price.
            ;; (make sure this is retracted somewhere)
            (values (cons (while-relevant-assert (order title offer-price #f #f)) actions)
                    inv)]
           [else
            ;; Allocate an order ID.
            ;;
            (define order-id next-order-id)
            (set! next-order-id (+ order-id 1))
            
            ;; Remove the book from our shelves.
            ;;
            (define new-inv (hash-remove inv title))
            
            ;; Tell the ordering party their order ID and delivery date.
            (values (cons (while-relevant-assert (order title offer-price order-id "March 9th"))
                          actions)
                    new-inv)])))
     (transition new-inv actions)]
    [_ (transition inv '())]))

;; Seller
;; Respond to requests for quotes
;; Respond to proposed offers
;; STATE: (Hashof string -> number)
(define next-order-id 10001483)
(define (seller-behavior e inv)
  (cond
    [(patch? e)
     (sequence-transitions (transition inv '())
                           (answer-quotes e)
                           (answer-orders e))]
    [else #f]))

(define (seller inv)
  (actor #:name 'seller
         seller-behavior
         inv
         (list (sub (observe (book-quote ? ?)))
               (sub (observe (order ? ? ? ?))))))

(define (buyer-a titles budget)
  (define (log-offer title contrib)
    (log-info "A makes an offer to split the price of ~v, contributing ~a"
              title
              contrib))
  (define (try-to-buy titles budget)
    (match titles
      ['() (log-info "A has bought everything they wanted!") patch-empty]
      [(cons title remaining-titles)
       (actor/stateless
        #:name (format "(try-to-buy ~a)" title)
        (lambda (e)
          (match e
            [(patch added _)
             (define maybe-price (set-first (trie-project/set/single added (book-quote title (?!)))))
             (cond
               [maybe-price
                (log-info "A learns that the price of ~v is ~a" title maybe-price)
                (quit (negotiate-split title maybe-price remaining-titles budget))]
               [else
                (log-info "A learns that ~v is out-of-stock." title)
                (quit (try-to-buy remaining-titles budget))])]
            [_ #f]))
        (list (sub (book-quote title ?))))]))
  (define (negotiate-split title price remaining-titles budget)
    (define initial-offer (min budget (/ price 2)))
    (cond
      [(> initial-offer budget)
       ;; Don't have enough money
       (log-info "A does not have enough money for ~v." title)
       (try-to-buy remaining-titles budget)]
      [else
       (log-offer title initial-offer)
       (actor
        #:name (format "(negotiate-split ~a ~a)" title price)
        (lambda (e my-contribution)
          (match e
            [(patch (and added (? (compose not trie-empty?))) _)
             (define accepted?
               (set-first (trie-project/set/single added (split-proposal title price ? (?!)))))
             (cond
               [accepted?
                (define remaining-budget (- budget my-contribution))
                (log-info "A learns that the split-proposal for ~v was accepted, leaving them with ~v remaining" title remaining-budget)
                (quit (try-to-buy remaining-titles remaining-budget))]
               [else
                (log-info "A learns that the split-proposal for ~v was rejected" title)
                (let ([my-contribution (+ my-contribution (/ (- price my-contribution) 2))])
                  (cond
                    [(> my-contribution (- price 0.10))
                     ;; Not worth bothering to split the price. May as well buy it ourselves.
                     ;; TODO: could perform BUYER here
                     ;;
                     (log-info "A gives up on ~v." title)
                     (quit (try-to-buy remaining-titles budget))]
                    [(> my-contribution budget)
                     ;; Don't have enough money
                     (log-info "A does not have enough money for ~v." title)
                     (quit (try-to-buy remaining-titles budget))]
                    [else
                     (log-offer title my-contribution)
                     (transition my-contribution
                                 (list (patch-seq (unsub (split-proposal title price ? ?))
                                                  (sub (split-proposal title price my-contribution ?)))))]))])]
            [_ #f]))
        initial-offer
        (list (sub (split-proposal title price initial-offer ?))))]))
  (try-to-buy titles budget))

(define (buyer-b funds)
  (define (complete-purchase title price contrib)
    (actor/stateless
     #:name (format "(complete-purchase ~a ~a ~a)" title price contrib)
     (lambda (e)
       (match e
         [(patch added _)
          (match-define (list order-id delivery-date)
            (set-first (trie-project/set #:take 2 added (order title price (?!) (?!)))))
          (log-info "The order for ~v has id ~a, and will be delivered on ~a"
                                  title
                                  order-id
                                  delivery-date)
          (quit)]
         [_ #f]))
     (list (assert (split-proposal title price contrib #t))
           (sub (order title price ? ?)))))
  (actor
   #:name 'buyer-b
   (lambda (e funds)
     (match e
       [(patch added _)
        (define-values (remaining-funds actions)
          (for-trie/fold ([funds funds]
                          [actions (list)])
                         ([(observe (split-proposal $title $price $contrib _)) added])
            (define my-contribution (- price contrib))
            (log-info "B is being asked to contribute ~a toward ~v at price ~a"
                      my-contribution
                      title
                      price)
            (cond
              [(> my-contribution funds)
               (log-info "B hasn't enough funds (~a remaining)" funds)
               (values funds
                       (cons (while-relevant-assert (split-proposal title price contrib #f))
                             actions))]
              [else
               (define remaining-funds (- funds my-contribution))
               (log-info "B accepts the offer, leaving them with ~a remaining funds"
                         remaining-funds)
               (values remaining-funds
                       (cons (complete-purchase title price contrib)
                             actions))])))
          (transition remaining-funds actions)]
       [_ #f]))
   funds
   (list (sub (observe (split-proposal ? ? ? ?))))))

(seller (hash "The Wind in the Willows" 3.95
              "Catch 22" 2.22
              "Candide" 34.95))

(buyer-a (list "Catch 22"
               "Encyclopaedia Brittannica"
               "Candide"
               "The Wind in the Willows")
         35.00)

(buyer-b 5.00)
