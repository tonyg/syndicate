#lang typed/syndicate

(lambda ()
  (spawn
    (on (asserted (tuple))
        #f)))

(lambda ()
  (spawn
    (field [v #t])
    (assert "hello")
    (on (asserted (tuple))
        #f)))

(lambda ()
  (spawn
    (field [v #t])
    (assert "hello")
    (on (asserted (tuple))
        #f)))

(lambda ()
  (spawn
    (on (asserted (tuple))
        (stop-current-facet))))

(lambda ()
  (spawn
    (stop-when (asserted (tuple)))))
