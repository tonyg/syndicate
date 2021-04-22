#lang racket

(provide (struct-out msg))

(struct msg (in out) #:transparent)