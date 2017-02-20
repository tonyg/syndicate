#lang syndicate/actor
;; Dreadfully simplified IRC client driver.

(provide (struct-out irc-connection)
         (struct-out irc-presence)
         (struct-out irc-inbound)
         (struct-out irc-outbound))

(define-logger syndicate/drivers/irc)

(require racket/format)
(require racket/set)
(require racket/string)

(require syndicate/protocol/advertise)

(require/activate syndicate/supervise)
(require/activate "tcp.rkt")
(require/activate "line-reader.rkt")

(struct irc-connection (host port nick) #:prefab) ;; ASSERTION

(struct irc-presence (conn nick channel) #:prefab) ;; ASSERTION
(struct irc-inbound (conn nick target body) #:prefab) ;; MESSAGE
(struct irc-outbound (conn target body) #:prefab) ;; MESSAGE

(spawn #:name 'irc-connection-factory

       (during (observe (irc-inbound $C _ _ _))
         (assert C))

       (during (observe (observe (irc-outbound $C _ _)))
         (assert C))

       (during/spawn (irc-connection $host $port $nick)
         #:spawn supervise/spawn
         #:name (list 'irc-connection host port nick)
         (define C (irc-connection host port nick))
         (define LH (tcp-handle (gensym 'irc)))
         (define RH (tcp-address host port))

         (define (irc-send! . pieces)
           (send! (tcp-channel LH RH (string->bytes/utf-8
                                      (string-append
                                       (apply string-append (map ~a pieces))
                                       "\r\n")))))

         (on (asserted (advertise (tcp-channel RH LH _)))
             (irc-send! "NICK "nick)
             (irc-send! "USER "nick" 0 * :"nick)
             (react
              (during (observe (irc-inbound C _ $target _))
                (when (string-prefix? target "#")
                  (on-start (irc-send! "JOIN :"target))
                  (on-stop (irc-send! "PART :"target))))
              (on (message (irc-outbound C $target $body))
                  (irc-send! "PRIVMSG "target" :"body)
                  (send! (irc-inbound C nick target body)))))

         (stop-when (retracted (advertise (tcp-channel RH LH _))))
         (assert (advertise (tcp-channel LH RH _)))

         (field [names-tgt #f]
                [names-acc (set)])

         (on (message (tcp-channel-line RH LH $line-bytes))
             (log-syndicate/drivers/irc-debug "irc got ~v" line-bytes)
             (match line-bytes

               [(regexp #px#"^PING(.*)\r$" (list _ line-tail))
                (irc-send! "PONG"line-tail)]

               [(regexp #px#"^:([^!]+)![^ ]* PRIVMSG ([^ ]+) :(.*)\r$" (list _ src tgt body))
                (send! (irc-inbound C
                                    (bytes->string/utf-8 src)
                                    (bytes->string/utf-8 tgt)
                                    (bytes->string/utf-8 body)))]

               [(regexp #px#"^:[^ ]* 353 [^:]+ ([^ ]+) :(.*)\r$" (list _ tgt names))
                (names-tgt (bytes->string/utf-8 tgt))
                (define new-names
                  (for/set [(n (string-split (bytes->string/utf-8 names)))]
                    (match n
                      [(regexp #px"@(.*)" (list _ n1)) n1]
                      [(regexp #px"\\+(.*)" (list _ n1)) n1]
                      [n1 n1])))
                (names-acc (set-union (names-acc) new-names))]

               [(regexp #px#"^:[^ ]* 366 " (list _))
                (retract! (irc-presence C ? (names-tgt)))
                (for ((n (names-acc)))
                  (assert! (irc-presence C n (names-tgt))))
                (assert! (irc-presence C nick (names-tgt)))
                ;; ^ (*) Here we note our own presence. We don't do it
                ;; in response to our own JOIN because we want to make
                ;; sure that our presence is marked *last* to help
                ;; preserve some semblance of causal ordering... This
                ;; is pretty hacky!
                (names-tgt #f)
                (names-acc (set))]

               [(regexp #px#"^:([^!]+)![^ ]* PART ([^ ]+)\r$" (list _ src tgt))
                (retract! (irc-presence C (bytes->string/utf-8 src) (bytes->string/utf-8 tgt)))]

               [(regexp #px#"^:([^!]+)![^ ]* QUIT :(.*)\r$" (list _ src _quitmsg))
                (retract! (irc-presence C (bytes->string/utf-8 src) ?))]

               [(regexp #px#"^:([^!]+)![^ ]* JOIN ([^ ]+)\r$" (list _ src-bs tgt))
                (define src (bytes->string/utf-8 src-bs))
                (when (not (equal? src nick)) ;; See above marked (*)
                  (assert! (irc-presence C src (bytes->string/utf-8 tgt))))]

               [_ (void)]))))
