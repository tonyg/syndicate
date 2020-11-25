#lang racket

(provide (struct-out cow))

(struct cow (moos) #:transparent)
