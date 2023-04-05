#lang racket

(provide (struct-out chicken)
         (struct-out roost))

(struct chicken (eggs) #:transparent)
(struct roost (chickens) #:transparent)
