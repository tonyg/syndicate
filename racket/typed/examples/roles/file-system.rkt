#lang typed/syndicate/roles

(define-constructor (file name content)
  #:type-constructor FileT
  #:with File (FileT String String))

(define-type-alias FileDemand
  (Observe (FileT String ★/t)))

(define-constructor (save name content)
  #:type-constructor SaveT
  #:with Save (SaveT String String))

(define-constructor (delete name)
  #:type-constructor DeleteT
  #:with Delete (DeleteT String))

;; unique role
(define-type-alias Server
  (Role (server)
        (Reacts (Know FileDemand)
                (Role (_)
                      (Shares File)))
        (Reacts (Message Save))
        (Reacts (Message Delete))))

(define-type-alias Reader
  (Role (reader)
        (Shares FileDemand)))

(define-type-alias Writer
  (Role (writer)
        (Sends Save)
        (Sends Delete)))