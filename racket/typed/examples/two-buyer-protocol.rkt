#lang typed/syndicate

(define-type-alias ds-type
  (U ;; quotes
   (Tuple String String Int)
   (Observe (Tuple String String ★))
   (Observe (Observe (Tuple String ★ ★)))
   ;; out of stock
   (Tuple String String)
   (Observe (Tuple String String))
   ;; splits
   (Tuple String String Int Int Bool)
   (Observe (Tuple String String Int Int ★))
   (Observe (Observe (Tuple String ★ ★ ★ ★)))
   ;; orders
   ;; work around generativity by putting it all inside a tuple
   (Tuple (Tuple String String Int Int String))
   (Observe (Tuple (Tuple String String Int ★ ★)))
   (Observe (Observe (Tuple (Tuple String ★ ★ ★ ★))))
   ;; denied order
   (Tuple (Tuple String String Int))
   (Observe (Tuple (Tuple String String Int)))))

(dataspace ds-type

;; seller
(spawn ds-type
       (facet _
              (fields [book (Tuple String Int) (tuple "Catch 22" 22)]
                      [next-order-id Int 10001483])
              (on (asserted (observe (tuple "book-quote" (bind title String) discard)))
                  (facet x
                         (fields)
                         (on (retracted (observe (tuple "book-quote" title discard)))
                             (stop x (begin)))
                         (match title
                           ["Catch 22"
                            (assert (tuple "book-quote" title 22))]
                           [discard
                            (assert (tuple "out-of-stock" title))])))
              (on (asserted (observe (tuple (tuple "order" (bind title String) (bind offer Int) discard discard))))
                  (facet x
                         (fields)
                         (on (retracted (observe (tuple (tuple "order" title offer discard discard))))
                             (stop x (begin)))
                         (let [asking-price 22]
                           (if (and (equal? title "Catch 22") (>= offer asking-price))
                               (let [order-id (ref next-order-id)]
                                 (begin (set! next-order-id (+ 1 order-id))
                                        (assert (tuple (tuple "order" title offer order-id "March 9th")))))
                               (assert (tuple (tuple "no-order" title offer)))))))))

;; buyer A
(spawn ds-type
       (facet buyer
              (fields [title String "Catch 22"]
                      [budget Int 1000])
              (on (asserted (tuple "out-of-stock" (ref title)))
                  (stop buyer (begin)))
              (on (asserted (tuple "book-quote" (ref title) (bind price Int)))
                  (facet negotiation
                         (fields [contribution Int (/ price 2)])
                         (on (asserted (tuple "split" (ref title) price (ref contribution) (bind accept? Bool)))
                             (if accept?
                                 (stop buyer (begin))
                                 (if (> (ref contribution) (- price 5))
                                     (stop negotiation (displayln "negotiation failed"))
                                     (set! contribution
                                           (+ (ref contribution) (/ (- price (ref contribution)) 2))))))))))

;; buyer B
(spawn ds-type
       (facet buyer-b
              (fields [funds Int 5])
              (on (asserted (observe (tuple "split" (bind title String) (bind price Int) (bind their-contribution Int) discard)))
                  (let [my-contribution (- price their-contribution)]
                    (cond
                      [(> my-contribution (ref funds))
                       (facet decline
                              (fields)
                              (assert (tuple "split" title price their-contribution #f))
                              (on (retracted (observe (tuple "split" title price their-contribution discard)))
                                  (stop decline (begin))))]
                      [#t
                       (facet accept
                              (fields)
                              (assert (tuple "split" title price their-contribution #t))
                              (on (retracted (observe (tuple "split" title price their-contribution discard)))
                                  (stop accept (begin)))
                              (on start
                                  (spawn ds-type
                                         (facet order
                                                (fields)
                                                (on (asserted (tuple (tuple "no-order" title price)))
                                                    (begin (displayln "Order Rejected")
                                                           (stop order (begin))))
                                                (on (asserted (tuple (tuple "order" title price (bind order-id Int) (bind delivery-date String))))
                                                    ;; complete!
                                                    (begin (displayln "Completed Order:")
                                                           (displayln order-id)
                                                           (displayln delivery-date)
                                                           (stop order (begin))))))))])))))
)