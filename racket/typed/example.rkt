#lang typed/syndicate

#;(require racket/base)

;; really lame how many times I have to write the dataspace type

(dataspace (U (Observe ★) String)

           (spawn (U (Observe ★) String)
                  (facet _
                         (assert "hello")))

           (spawn (U (Observe ★) String)
                  (facet _
                         (on (asserted "hello")
                             (unsafe-do (printf "got hello\n"))))))