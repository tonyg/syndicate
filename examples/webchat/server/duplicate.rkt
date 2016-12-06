#lang syndicate/actor

(provide stop-when-duplicate)

(require syndicate/protocol/instance)
(require "util.rkt")

(define (stop-when-duplicate spec)
  (define id (random-hex-string 16))
  (field [duplicate? #f])
  (stop-when (rising-edge (duplicate?)))
  (assert (instance id spec))
  (on (asserted (instance $id2 spec))
      (when (string<? id id2)
        (log-info "Duplicate instance of ~v detected; terminating" spec)
        (duplicate? #t)))
  id)
