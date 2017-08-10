#lang syndicate
;; Explore use of sandboxes etc for being an "operating system"

(require racket/match)
(require racket/sandbox)

(define (program-boot-actions modspec)
  (parameterize ((sandbox-input (current-input-port))
                 (sandbox-output (current-output-port)))
    (define e (make-evaluator 'racket/base))
    (define ns (call-in-sandbox-context e current-namespace))
    (for [(syndicate-modspec (in-list (list 'syndicate/actor
                                            'syndicate/canonicalize
                                            'syndicate/core
                                            'syndicate/treap
                                            'syndicate/trie
                                            'syndicate/tset)))]
      (namespace-attach-module (current-namespace) syndicate-modspec ns))
    (e `(require (submod ,modspec syndicate-main)))
    (e `(activate!))))

(struct running-app (id) #:prefab) ;; assertion
(struct kill-app (id) #:prefab) ;; message

(struct named-account (name balance) #:prefab) ;; assertion

(define (run-bank-account name)
  (struct account (balance) #:prefab)
  (dataspace (define id (symbol->string (gensym 'app)))
             (printf "Starting app ~a\n" id)
             (schedule-action! (program-boot-actions 'syndicate/examples/actor/bank-account))
             (forever
              (assert (outbound (running-app id)))
              (stop-when (message (inbound (kill-app id)))
                         (printf "Received signal for app ~a\n" id))
              (during (account $balance)
                      (assert (outbound (named-account name balance)))))))

(run-bank-account 'a)
(run-bank-account 'b)
(run-bank-account 'c)

(spawn (on (asserted (named-account $name $balance))
           (printf "Named account balance ~a = ~a\n" name balance)))

(spawn (define/query-set running-apps (running-app $id) id)
       (begin/dataflow (printf "Running apps: ~v\n" (running-apps))))

(let ()
  (local-require racket/port)
  (define e (read-bytes-line-evt (current-input-port) 'any))
  (spawn (stop-when (message (inbound (external-event e (list (? eof-object? _))))))
         (on (message (inbound (external-event e (list (? bytes? $bs)))))
             (define app-id (bytes->string/utf-8 bs))
             (printf "Killing ~a\n" app-id)
             (send! (kill-app app-id)))))
