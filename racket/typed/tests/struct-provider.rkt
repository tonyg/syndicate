#lang racket

(struct donkey (weight stubborn?) #:transparent)

(provide (struct-out donkey))
