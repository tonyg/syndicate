#lang syndicate

(assertion-struct greeting (text))

(spawn (assert (greeting "Hello from an outer dataspace actor!")))
(spawn (on (asserted (greeting $t))
           (printf "Outer dataspace: ~a\n" t)))

(dataspace (spawn (assert (outbound (greeting "Hello from an inner dataspace actor!"))))
           (spawn (on (asserted (inbound (greeting $t)))
                      (printf "Inner dataspace: ~a\n" t))))
