#lang racket/base
;; "Instance" protocol for discriminating among
;; otherwise-indistinguishable entities.

(provide (struct-out instance))

;; (instance Any Any), assertion or message
;;
;; In cases where `spec` can have multiple instantiations, serves to
;; distinguish between them. Each `id` should be unique within its
;; scope.
;;
(struct instance (id spec) #:prefab)
