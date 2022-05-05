#lang racket

(struct donkey (weight stubborn?) #:transparent)

(provide (struct-out donkey))

(struct pot () #:transparent)

(provide (struct-out pot))
