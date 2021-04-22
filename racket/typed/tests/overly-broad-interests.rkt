#lang typed/syndicate

(require rackunit/turnstile)

(typecheck-fail (spawn ⊥
                       (start-facet x
                                    (on (asserted $x:Int)
                                        #f)))
                #:with-msg "overly broad interest")

(typecheck-fail (spawn ⊥
                       (start-facet x
                                    (on (asserted (observe $x:Int))
                                        #f)))
                #:with-msg "overly broad interest")

;; TODO - but this one seems fine?
(typecheck-fail (spawn ⊥
                 (start-facet x
                              (on (asserted _)
                                  #f)))
                #:with-msg "overly broad interest")
