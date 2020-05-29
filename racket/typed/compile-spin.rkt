#lang racket

(require "proto.rkt")

(module+ test
  (require rackunit))

;; a SpinProcess is a
;;   (sproc SName [Listof SVar] [Hashof SName SValue] [Setof SpinState])
(struct sproc [name vars init states] #:transparent)

;; a SName is a Symbol that is a legal variable name in Spin

;; a SVar is a
;;   (svar SName SType)
(struct svar [name ty] #:transparent)

;; a SValue is one of
;;   - Int
;;   - Bool
;;   - SName
;; and must be a valid Spin literal

;; a SType is one of
;;  - 'SInt
;;  - 'SBool
;;  - 'mtype
(define SInt 'SInt)
(define SBool 'SBool)
(define mtype 'mtype)

;; a SpinState is a
;;   (sstate SName [Sequenceof SBranch])
(struct sstate [name branches] #:transparent)

;; a SBranch is a ........
