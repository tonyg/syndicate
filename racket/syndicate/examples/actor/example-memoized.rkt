#lang syndicate

(require/activate syndicate/drivers/timestate)

(assertion-struct memoized (req resp))
(assertion-struct underlying (req resp))

(spawn (during (observe (underlying $req _))
         (on-start (printf "**** Computing underlying ~a\n" req))
         (on-stop (printf "**** Releasing underlying ~a\n" req))
         (assert (underlying req (+ req 1)))))

(spawn (during (observe (memoized $req _))
         (on-start (printf "Outer memo entry for ~a created\n" req))
         (on-stop (printf "Outer memo entry for ~a released\n" req))
         (assert (observe (memoized req _))) ;; keep self alive
         (stop-when-timeout 3000)
         (on-start
          (react (stop-when (asserted (underlying req $resp))
                   (printf "Underlying response ~a for ~a received\n" resp req)
                   (react (on-start (printf "Memo entry for ~a => ~a created\n" req resp))
                          (on-stop (printf "Memo entry for ~a => ~a released\n" req resp))
                          (assert (memoized req resp))))))))

(spawn (stop-when (asserted (memoized 1 $n))
         (printf "First result for 1: ~a\n" n)
         (sleep 1)
         (react (stop-when (asserted (memoized 1 $m))
                  (printf "Second result for 1: ~a\n" m)
                  (sleep 3)
                  (react (stop-when (asserted (memoized 1 $k))
                           (printf "Third result for 1: ~a\n" k)))))))
