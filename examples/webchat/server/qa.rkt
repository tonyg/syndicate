#lang syndicate/actor

(provide ask-question!)

(require racket/port)
(require markdown)

(require/activate syndicate/reload)
(require/activate syndicate/supervise)

(require "protocol.rkt")
(require "util.rkt")

(supervise
 (actor #:name 'qa-relay
        (stop-when-reloaded)
        (during ($ q (question _ _ _ _ _ _ _))
          (define qid (question-id q))
          (define target (question-target q))
          (assert (api (session target _) q))
          (during (api (session target _) (answer qid $value))
            (assert (answer qid value))))))

(define (ask-question! #:title title
                       #:blurb blurb
                       #:class [q-class "q-generic"]
                       #:target target
                       question-type)
  (define qid (random-hex-string 32))
  (define q (question qid
                      (current-seconds)
                      q-class
                      target
                      title
                      (with-output-to-string
                        (lambda ()
                          (display-xexpr blurb)))
                      question-type))
  (assert q)
  qid)

