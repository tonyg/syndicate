#lang racket/base

(provide COOKIE
         clear-session-cookie
         format-cookie
         fresh-session-id
         session-id->cookie
         with-session)

(require racket/list)
(require racket/match)
(require racket/set)
(require web-server/http/request-structs)
(require web-server/http/cookie)

(require syndicate/actor)
(require syndicate/drivers/web)

(require "protocol.rkt")
(require "util.rkt")

(define COOKIE "syndicatewebchat")

(define clear-session-cookie (make-cookie COOKIE
                                          ""
                                          #:path "/"
                                          #:expires "Thu, 01 Jan 1970 00:00:00 GMT"))

(define (format-cookie c)
  (match-define (header field value) (cookie->header c))
  (cons (string->symbol (string-downcase (bytes->string/latin-1 field)))
        (bytes->string/utf-8 value)))

(define (fresh-session-id)
  (random-hex-string 32))

(define (session-id->cookie sid)
  (make-cookie COOKIE sid #:path "/"))

(define-syntax with-session
  (syntax-rules (else)
    [(_ id [(email sid) body ...])
     (with-session id [(email sid) body ...] [else (web-redirect! id "/")])]
    [(_ id [(email sid) body ...] [else no-session-body ...])
     (let ()
       (define (on-no-session)
         no-session-body ...)
       (match (immediate-query (query-value #f (web-request-cookie id COOKIE $v _ _) v))
         [#f (on-no-session)]
         [sid
          (match (immediate-query (query-value #f (session $e sid) e))
            [#f (on-no-session)]
            [email
             body ...])]))]))
