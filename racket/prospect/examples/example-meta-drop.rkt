#lang prospect
;; Analogous to nc-incremental-meta-drop.rkt in the Redex model.
;; Demonstrates (hopefully) correct processing of meta-interests when dropping a patch.

(spawn-network
 (spawn (lambda (e u)
          (match u
            [0 (transition 1 '())]
            [1 (transition 2 (retract 'a #:meta-level 1))]
            [_ #f]))
        0
        (patch-seq (assert 'a #:meta-level 1)
                   (assert (observe 'a) #:meta-level 1))))
