#lang racket

(struct egg (size day) #:transparent)

(provide (except-out (struct-out egg)
                     egg-size
                     egg-day))


(struct chicken (eggs) #:transparent)

(provide chicken)
