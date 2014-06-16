#lang racket/base

(provide ones-complement-sum16 ip-checksum)

(require bitsyntax)
(require "dump-bytes.rkt")

(define (ones-complement-+16 a b)
  (define c (+ a b))
  (bitwise-and #xffff (+ (arithmetic-shift c -16) c)))

(define (ones-complement-sum16 bs)
  (bit-string-case bs
    ([ (n :: integer bytes 2) (rest :: binary) ]
     (ones-complement-+16 n (ones-complement-sum16 rest)))
    ([ odd-byte ]
     (arithmetic-shift odd-byte 8))
    ([ ]
     0)))

(define (ones-complement-negate16-safely x)
  (define r (bitwise-and #xffff (bitwise-not x)))
  (if (= r 0) #xffff r))

(define (ip-checksum offset blob #:pseudo-header [pseudo-header #""])
  (bit-string-case blob
    ([ (prefix :: binary bytes offset)
       (:: binary bytes 2)
       (suffix :: binary) ]
     (log-info "Packet pre checksum:\n~a" (dump-bytes->string blob))
     (define result (ones-complement-+16
		     (ones-complement-sum16 pseudo-header)
		     (ones-complement-+16 (ones-complement-sum16 prefix)
					  (ones-complement-sum16 suffix))))
     (log-info "result: ~a" (number->string result 16))
     (define checksum (ones-complement-negate16-safely result))
     (log-info "Checksum ~a" (number->string checksum 16))
     (define final-packet (bit-string (prefix :: binary)
				      (checksum :: integer bytes 2)
				      (suffix :: binary)))
     (log-info "Packet with checksum:\n~a" (dump-bytes->string final-packet))
     final-packet)))

(module+ test
  (require rackunit)
  (check-equal? (ones-complement-negate16-safely
		 (ones-complement-sum16 (bytes #x45 #x00 #x00 #x54
					       #x00 #x00 #x00 #x00
					       #x40 #x01 #x00 #x00
					       #xc0 #xa8 #x01 #xde
					       #xc0 #xa8 #x01 #x8f)))
		#xf5eb))
