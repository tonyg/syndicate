#lang typed/syndicate/roles

(provide activate!
         tcp-connection
         tcp-accepted
         tcp-out
         tcp-in
         tcp-in-line
         tcp-address
         tcp-listener
         seal
         advertise
         Tcp2LineReaderFactory
         Tcp2Driver)

(require-struct tcp-connection
                #:as TcpConnection
                #:from syndicate/drivers/tcp2)

(require-struct tcp-accepted
                #:as TcpAccepted
                #:from syndicate/drivers/tcp2)

(require-struct tcp-out
                #:as TcpOut
                #:from syndicate/drivers/tcp2)

(require-struct tcp-in
                #:as TcpIn
                #:from syndicate/drivers/tcp2)

(require-struct tcp-in-line
                #:as TcpInLine
                #:from syndicate/drivers/tcp2)

(require-struct tcp-address
                #:as TcpAddress
                #:from syndicate/drivers/tcp2)

(require-struct tcp-listener
                #:as TcpListener
                #:from syndicate/drivers/tcp2)

(require-struct tcp-channel
                #:as TcpChannel
                #:from syndicate/drivers/tcp)

(require-struct tcp-handle
                #:as TcpHandle
                #:from syndicate/drivers/tcp)

(require-struct seal
                #:as Seal
                #:from syndicate/lang)

(require-struct advertise
                #:as Advertise
                #:from syndicate/protocol/advertise)

;; assertions and messages sent & received by the 'tcp2-listen-driver
(define-type-alias Tcp2ListenDriver
  (U (Observe (Observe (TcpConnection ★/t (TcpListener ★/t))))
     (Observe (TcpConnection ★/t (TcpListener Int)))
     (Observe (TcpConnection ★/t (TcpListener Int)))
     (Advertise (Observe (TcpChannel ★/t (TcpListener (TcpHandle (Seal ★/t))) ★/t)))
     (Observe (Advertise (TcpChannel (TcpAddress String Int) (TcpHandle (Seal ★/t)) ★/t)))
     (TcpAccepted ★/t)
     (Advertise (TcpChannel (TcpHandle (Seal ★/t)) (TcpAddress String Int) ★/t))
     (Observe (TcpChannel (TcpAddress String Int) (TcpHandle (Seal ★/t)) ★/t))
     (Message (TcpChannel (TcpAddress String Int) (TcpHandle (Seal ★/t)) ByteString))
     (Message (TcpIn (Seal ★/t) ByteString))
     (Observe (TcpOut (Seal ★/t) ★/t))
     (Message (TcpOut (Seal ★/t) ByteString))
     (Message (TcpChannel (TcpHandle (Seal ★/t)) (TcpAddress String Int) ByteString))))

;; assertions and messages sent & received by the 'tcp2-connect-driver
(define-type-alias Tcp2ConnectDriver
  (U (Observe (TcpConnection ★/t (TcpAddress ★/t ★/t)))
     (TcpConnection Symbol (TcpAddress String Int))
     (Observe (Advertise (TcpChannel (TcpAddress String Int) (TcpHandle (Seal ★/t)) ★/t)))
     (TcpAccepted ★/t)
     (Advertise (TcpChannel (TcpHandle (Seal ★/t)) (TcpAddress String Int) ★/t))
     (Observe (TcpChannel (TcpAddress String Int) (TcpHandle (Seal ★/t)) ★/t))
     (Message (TcpChannel (TcpAddress String Int) (TcpHandle (Seal ★/t)) ByteString))
     (Message (TcpIn ★/t ByteString))
     (Observe (TcpOut ★/t ★/t))
     (Message (TcpOut ★/t ByteString))
     (Message (TcpChannel (TcpHandle (Seal ★/t)) (TcpAddress String Int) ByteString))))

;; assertions and messages sent & received by the 'tcp2-line-reader-factory
(define-type-alias Tcp2LineReaderFactory
  (U (Observe (Observe (TcpInLine ★/t ★/t)))
     (Observe (TcpInLine ★/t ★/t))
     (Observe (TcpIn ★/t ★/t))
     (Message (TcpIn ★/t ByteString))
     (Message (TcpInLine ★/t ByteString))))

(define-type-alias Tcp2Driver
  (U Tcp2ListenDriver
     Tcp2ConnectDriver
     Tcp2LineReaderFactory))

(require/typed syndicate/drivers/tcp2)
(require/typed (submod syndicate/drivers/tcp2 syndicate-main)
  [activate! : (proc → (U) #:effects ((Actor Tcp2Driver))) #;(→ (Computation (Value (U))
                                            (Endpoints)
                                            (Roles)
                                            (Spawns (Actor Tcp2Driver))))])

;; TODO
;;
;; The tcp2 driver also "activates" the tcp driver, so in order to be sound the typed driver ought to
;; indicate through the types that whatever assertions and messages that driver does can also happen.
;;
;; The require/activate model doesn't lend itself to the current workings of the type system very
;; easily. Perhaps a require/activate/typed would be in order, where the provided type describes the
;; dataspace type of the actors that get spawned.
