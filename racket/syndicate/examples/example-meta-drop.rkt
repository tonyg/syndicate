#lang syndicate
;; Analogous to nc-incremental-meta-drop.rkt in the Redex model.
;; Demonstrates (hopefully) correct processing of meta-interests when dropping a patch.

(dataspace-actor
 (actor (lambda (e u)
          (match u
            [0 (transition 1 '())]
            [1 (transition 2 (retract (outbound 'a)))]
            [_ #f]))
        0
        (patch-seq (assert (outbound 'a))
                   (assert (observe (inbound 'a))))))
