#lang typed/syndicate

(module+ test
  (verify-actors (Always (Implies (Tuple True) (Tuple Bool)))
    #:IO (Tuple Bool)
    (spawn (react #f))))
