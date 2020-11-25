#lang typed/syndicate/roles

(require-struct cow #:as Cow #:from "untyped.rkt")

(provide (struct-out cow))
