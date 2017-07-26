#lang racket/base
;; Simple "ping" responder. Nightmarishly oversimplified. We want to
;; look at overheads excluding Syndicate. See also
;; http://dunkels.com/adam/twip.html

(require packet-socket)

(require "dump-bytes.rkt")

(define device-name (or (getenv "PINGRESP_DEVICE") "eth0"))
(define self-ip (integer-bytes->integer (bytes 129 10 115 94) #f #t))

(define handle (raw-interface-open device-name))
(unless handle (error 'pingresp "Couldn't open ~a" device-name))

(let loop ()
  (define eth-buffer (raw-interface-read handle))
  (define buffer (subbytes eth-buffer 14))
  (when (>= (bytes-length buffer) 20) ;; enough space for local and remote IP addresses
    (define local-ip (integer-bytes->integer buffer #f #t 16 20))
    (define remote-ip (integer-bytes->integer buffer #f #t 12 16))
    (when (= local-ip self-ip)
      ;; (printf "Got ping from ~v\n" (bytes->list (subbytes buffer 12 16)))
      ;; (flush-output)
      ;; (dump-bytes! eth-buffer)
      ;; (newline)

      (when (and (>= (bytes-length buffer) 28) ;; IP + ICMP headers
                 (= (bytes-ref buffer 9) 1) ;; IP protocol
                 (= (bytes-ref buffer 20) 8) ;; ICMP ECHO
                 )

        (bytes-set! buffer 20 0) ;; ICMP ECHO_REPLY
        (integer->integer-bytes (bitwise-and #xffff
                                             (+ #x0800
                                                (integer-bytes->integer buffer #f #t 22 24)))
                                2 #f #t buffer 22) ;; "fix" checksum
        (integer->integer-bytes local-ip 4 #f #t buffer 12)
        (integer->integer-bytes remote-ip 4 #f #t buffer 16)

        (define reply
          (bytes-append (subbytes eth-buffer 6 12)
                        (subbytes eth-buffer 0 6)
                        (subbytes eth-buffer 12 14)
                        buffer))
        ;; (displayln "Reply:")
        ;; (dump-bytes! reply)
        ;; (newline)
        (raw-interface-write handle reply))))
  (loop))

(raw-interface-close handle)


;; short s[70];
;; int *l = s;
;; int t;
;;
;;    read(0, s, 140);
;;     if((s[4] & 65280) == 256 & s[10] == 8) {
;;       s[10] = 0;
;;       s[11] += 8;
;;       t = l[4];
;;       l[4] = l[3];
;;       l[3] = t;
;;       write(1, s, 140);
;;     }
