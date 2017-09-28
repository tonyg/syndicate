#lang syndicate

(assertion-struct greeting (text))

(spawn #:name "A" (assert (greeting "Hi from outer space!")))
(spawn #:name "B" (on (asserted (greeting $t))
                      (printf "Outer dataspace: ~a\n" t)))

(dataspace #:name "C"
  (spawn #:name "D" (assert (outbound (greeting "Hi from inner!"))))
  (spawn #:name "E" (on (asserted (inbound (greeting $t)))
                        (printf "Inner dataspace: ~a\n" t))))
