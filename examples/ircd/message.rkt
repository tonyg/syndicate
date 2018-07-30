#lang racket/base

(provide (struct-out irc-message)
         (struct-out irc-user)
         (struct-out irc-privmsg)

         (struct-out irc-source-servername)
         (struct-out irc-source-nick)

         parse-irc-message
         render-irc-message

         ;; TODO make these assertions in the dataspace:
         server-name
         server-prefix)

(require racket/string)
(require racket/match)
(require racket/format)

;; <message>  ::= [':' <prefix> <SPACE> ] <command> <params> <crlf>
;; <prefix>   ::= <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
;; <command>  ::= <letter> { <letter> } | <number> <number> <number>
;; <SPACE>    ::= ' ' { ' ' }
;; <params>   ::= <SPACE> [ ':' <trailing> | <middle> <params> ]
;;
;; <middle>   ::= <Any *non-empty* sequence of octets not including SPACE
;;                or NUL or CR or LF, the first of which may not be ':'>
;; <trailing> ::= <Any, possibly *empty*, sequence of octets not including
;;                  NUL or CR or LF>
;;
;; <crlf>     ::= CR LF

;; <target>     ::= <to> [ "," <target> ]
;; <to>         ::= <channel> | <user> '@' <servername> | <nick> | <mask>
;; <channel>    ::= ('#' | '&') <chstring>
;; <servername> ::= <host>
;; <host>       ::= see RFC 952 [DNS:4] for details on allowed hostnames
;; <nick>       ::= <letter> { <letter> | <number> | <special> }
;; <mask>       ::= ('#' | '$') <chstring>
;; <chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and
;;                   comma (',')>

;; <user>       ::= <nonwhite> { <nonwhite> }
;; <letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
;; <number>     ::= '0' ... '9'
;; <special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'

;; <nonwhite>   ::= <any 8bit code except SPACE (0x20), NUL (0x0), CR
;;                   (0xd), and LF (0xa)>

(struct irc-message (prefix command params trailing) #:prefab)
(struct irc-user (username hostname realname) #:prefab)
(struct irc-privmsg (source target text) #:prefab)

(struct irc-source-servername (servername) #:prefab)
(struct irc-source-nick (nick user) #:prefab)

(define (parse-irc-message line0)
  (match (string-trim #:left? #f line0 #px"[\r\n]")
    [(pregexp #px"^:([^ ]+) +(.*)$" (list _ prefix rest)) (parse-command prefix rest)]
    [line (parse-command #f line)]))

(define (parse-command prefix line)
  (match-define (pregexp #px"^([^ ]+)( +([^:]+)?(:(.*))?)?$" (list _ command _ params _ rest)) line)
  (irc-message prefix
               (string-upcase command)
               (string-split (or params ""))
               rest))

;; libpurple's irc protocol support crashes (!) (SIGSEGV) if you send
;; a prefix on a JOIN event from the server as just "nick" rather than
;; "nick!user@host" - specifically, it will crash if "!" doesn't
;; appear in the prefix.
;;
(define (render-irc-message m)
  (match-define (irc-message prefix command params trailing) m)
  (string-append (render-prefix prefix)
                 (~a command)
                 (if (pair? params) (string-append " " (string-join (map ~a params))) "")
                 (if trailing (string-append " :" trailing) "")))

(define (render-prefix p)
  (match p
    [#f
     ""]
    [(irc-source-servername servername)
     (format ":~a " servername)]
    [(irc-source-nick nick (irc-user username hostname _))
     (format ":~a!~a@~a " nick username hostname)]))

(define server-name "syndicate-ircd")
(define server-prefix (irc-source-servername "syndicate-ircd.example"))
