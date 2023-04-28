#lang typed/syndicate

(require rackunit/turnstile)

(module+ test
  ;; no interests, also only one output
  (check-deadlock-free* (Role (x) (Shares (Tuple))))
  ;; has an interest, but no IO
  (check-deadlock-free* (Role (x)
                              (Reacts (Asserted (Tuple)))
                              (Shares (Tuple)))))
