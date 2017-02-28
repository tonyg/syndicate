#lang syndicate
;; run-ground-vm is being modified to return the set of assertions
;; remaining at the end of its execution. This example demonstrates a
;; non-empty such set.

(actor (lambda (e u)
         (when (zero? u)
           (transition (+ u 1) (list (assert (outbound 'ok))
                                     (quit-dataspace)))))
       0
       '())

(module+ main
  (require rackunit)
  (require syndicate/trie)
  (require syndicate/tset)
  (define previous-ground-dataspace (current-ground-dataspace))
  (current-ground-dataspace (lambda boot-actions
                              (let ((result (apply previous-ground-dataspace boot-actions)))
                                (check-equal? result
                                              (pattern->trie (datum-tset 'root) 'ok))))))
