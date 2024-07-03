#lang syndicate

(provide spawn/persist)

(require syntax/parse/define)

;; a FieldStore is a (Hashof Symbol Any)

;; a Persisted is a (persisted Symbol FieldStore)
;; a Persist is a (persist Symbol FieldStore)

(assertion-struct persisted (id fields))
(assertion-struct persist (id fields))

(define (spawn-field-store id init-fields)
  (spawn
    (field [the-store init-fields])
    (assert (persisted id (the-store)))
    (on (asserted (persist id $new-fields))
        (the-store new-fields))))

(define-syntax-parse-rule (persistable #:fields ([field-name:id field-val-0:expr] ...) body ...+)
  (let ()
    (define persist-id (gensym))
    (define boot-store
      (lambda ()
        (spawn-field-store persist-id (hash (~@ 'field-name field-val-0) ...))))
    (define boot-actor
      (lambda ()
        (spawn
          (on-start
           (react
             (stop-when (asserted (persisted persist-id $fs))
                        (react
                          (field [field-name (hash-ref fs 'field-name)] ...)
                          (assert (persist persist-id (hash (~@ 'field-name (field-name)) ...)))
                          body ...)))))))
    (values boot-store boot-actor)))

(define-syntax-parse-rule (spawn/persist  #:fields ([field-name:id field-val-0:expr] ...) body ...+)
  (let ()
    (define-values (store-thunk actor-thunk)
      (persistable #:fields ([field-name field-val-0] ...)
                   body ...))
    (store-thunk)
    (actor-thunk)))
