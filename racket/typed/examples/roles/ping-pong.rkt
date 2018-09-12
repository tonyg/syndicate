#lang typed/syndicate/roles

;; Expected Output
;; pong: 8339

(define-type-alias ds-type
  (U (Message (Tuple String Int))
     (Observe (Tuple String ★/t))))

(dataspace ds-type
  (spawn ds-type
    (start-facet echo
      (on (message (tuple "ping" (bind x Int)))
          (send! (tuple "pong" x)))))
  (spawn ds-type
    (start-facet serve
      (on start
          (send! (tuple "ping" 8339)))
      (on (message (tuple "pong" (bind x Int)))
          (printf "pong: ~v\n" x)))))