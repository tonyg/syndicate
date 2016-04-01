#lang racket/base
;; Copyright (C) 2012 Tony Garnock-Jones <tonygarnockjones@gmail.com>
;;
;; dump-bytes.rkt is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; dump-bytes.rkt is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with dump-bytes.rkt. If not, see <http://www.gnu.org/licenses/>.

;; Pretty hex dump output of a Bytes.

(provide dump-bytes!
	 dump-bytes->string
	 pretty-bytes)

(require (only-in bitsyntax bit-string->bytes))
(require (only-in file/sha1 bytes->hex-string))

(define (pretty-bytes bs)
  (bytes->hex-string (bit-string->bytes bs)))

;; Exact Exact -> String
;; Returns the "0"-padded, width-digit hex representation of n
(define (hex width n)
  (define s (number->string n 16))
  (define slen (string-length s))
  (cond
   ((< slen width) (string-append (make-string (- width slen) #\0) s))
   ((= slen width) s)
   ((> slen width) (substring s 0 width))))

;; Bytes Exact -> Void
;; Prints a pretty hex/ASCII dump of bs on (current-output-port).
(define (dump-bytes! bs0 [requested-count #f] #:base [baseaddr 0])
  (define bs (bit-string->bytes bs0))
  (define count (if requested-count (min requested-count (bytes-length bs)) (bytes-length bs)))
  (define clipped (subbytes bs 0 count))
  (define (dump-hex i)
    (if (< i count)
	(display (hex 2 (bytes-ref clipped i)))
	(display "  "))
    (display #\space))
  (define (dump-char i)
    (if (< i count)
	(let ((ch (bytes-ref clipped i)))
	  (if (<= 32 ch 127)
	      (display (integer->char ch))
	      (display #\.)))
	(display #\space)))
  (define (for-each-between f low high)
    (do ((i low (+ i 1)))
	((= i high))
      (f i)))
  (define (dump-line i)
    (display (hex 8 (+ i baseaddr)))
    (display #\space)
    (for-each-between dump-hex i (+ i 8))
    (display ": ")
    (for-each-between dump-hex (+ i 8) (+ i 16))
    (display #\space)
    (for-each-between dump-char i (+ i 8))
    (display " : ")
    (for-each-between dump-char (+ i 8) (+ i 16))
    (newline))
  (do ((i 0 (+ i 16)))
      ((>= i count))
    (dump-line i)))

(define (dump-bytes->string bs [requested-count #f] #:base [baseaddr 0])
  (define s (open-output-string))
  (parameterize ((current-output-port s))
    (dump-bytes! bs requested-count #:base baseaddr))
  (get-output-string s))
