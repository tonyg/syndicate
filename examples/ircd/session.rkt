#lang syndicate/actor

(require racket/set)
(require racket/string)

(require "protocol.rkt")
(require "message.rkt")

(require/activate syndicate/reload)
(require/activate syndicate/drivers/tcp)
(require/activate syndicate/drivers/line-reader)
(require syndicate/protocol/advertise)
(require syndicate/support/hash)

(define (ircd-connection-facet this-conn server-handle)
  (define (send-to-remote #:newline [with-newline #t] fmt . vs)
    (define bs (string->bytes/utf-8 (apply format fmt vs)))
    (log-info "~a <- ~v" this-conn bs)
    (send! (tcp-channel server-handle this-conn (if with-newline (bytes-append bs #"\r\n") bs))))

  (define (send-irc-message m)
    (send-to-remote "~a" (render-irc-message m)))

  (define (send* #:source [prefix server-prefix] #:trailing [trailing #f] command . params)
    (send-irc-message (irc-message prefix command params trailing)))

  (on-start (log-info "Connecting ~a" this-conn))
  (on-stop (log-info "Disconnecting ~a" this-conn))

  (field [nick #f]
         [user #f])
  (define/dataflow conn-info (ircd-connection-info this-conn (nick) (user)))
  (assert (conn-info))

  (during (ircd-motd $motd-lines)
    (field [motd-sent? #f])
    (begin/dataflow
      (unless (motd-sent?)
        (when (and (nick) (user))
          (motd-sent? #t)
          (send* 375 (nick) #:trailing (format "- ~a Message of the day - " server-name))
          (for [(line motd-lines)] (send* 372 (nick) #:trailing (format "- ~a" line)))
          (send* 376 (nick) #:trailing (format "End of /MOTD command"))))))

  (field [peer-common-channels (hash)]
         [peer-names (hash)])

  (during (ircd-channel-member $Ch this-conn)
    (field [initial-names-sent? #f]
           [initial-member-nicks (set)])

    (on-start (send* #:source (irc-source-nick (nick) (user)) "JOIN" Ch)
              (flush!)
              (flush!)
              (define nicks (initial-member-nicks))
              (initial-names-sent? #t)
              (initial-member-nicks 'no-longer-valid)
              (send* 353 (nick) "@" Ch #:trailing (string-join (set->list nicks)))
              (send* 366 (nick) Ch #:trailing "End of /NAMES list"))

    (during (ircd-channel-member Ch $other-conn)
      (on-start (peer-common-channels (hashset-add (peer-common-channels) other-conn Ch)))
      (on-stop (peer-common-channels (hashset-remove (peer-common-channels) other-conn Ch)))
      (field [current-other-source #f])
      (define/query-value next-other-source #f
        (ircd-connection-info other-conn $N $U)
        (irc-source-nick N U))
      (on (retracted (ircd-channel-member Ch other-conn))
          (when (current-other-source) (send* #:source (current-other-source) "PART" Ch)))
      (on-stop (when (not (hash-has-key? (peer-common-channels) other-conn))
                 (peer-names (hash-remove (peer-names) other-conn))))
      (begin/dataflow
        (when (not (equal? (current-other-source) (next-other-source)))
          (if (not (next-other-source)) ;; other-conn is disconnecting
              (when (hash-ref (peer-names) other-conn #f)
                (send* #:source (current-other-source) "QUIT")
                (peer-names (hash-remove (peer-names) other-conn)))
              (begin
                (cond
                  [(not (initial-names-sent?)) ;; still gathering data for 353/366 below
                   (initial-member-nicks (set-add (initial-member-nicks)
                                                  (irc-source-nick-nick (next-other-source))))]
                  [(not (current-other-source)) ;; other-conn is joining
                   (send* #:source (next-other-source) "JOIN" Ch)]
                  [else ;; it's a nick change
                   (when (not (equal? this-conn other-conn)) ;; avoid dups for our own connection
                     (when (not (equal? (next-other-source) (hash-ref (peer-names) other-conn #f)))
                       (send* #:source (current-other-source) "NICK"
                              (irc-source-nick-nick (next-other-source)))))])
                (peer-names (hash-set (peer-names) other-conn (next-other-source)))))
          (current-other-source (next-other-source)))))

    (on (asserted (ircd-channel-topic Ch $topic))
        (if topic
            (send* 332 (nick) Ch #:trailing topic)
            (send* 331 (nick) Ch #:trailing "No topic is set")))

    (on (message (ircd-action this-conn (irc-message _ "WHO" (list Ch) _)))
        (flush!) ;; Wait for responses to come in. GROSS and not in
                 ;; general correct (e.g. in the presence of
                 ;; pipelining)
        (send! (ircd-event this-conn
                           (irc-message server-prefix 315 (list (nick) Ch) "End of WHO list."))))
    (on (message (ircd-action $who (irc-message _ "WHO" (list Ch) _)))
        (match-define (irc-user U H R) (user))
        (send! (ircd-event who (irc-message server-prefix 352
                                            (list (nick) Ch U H server-name (nick) "H")
                                            (format "0 ~a" R)))))

    (on (message (ircd-action $other-conn (irc-privmsg $source Ch $text)))
        (when (not (equal? other-conn this-conn))
          (send* #:source source "PRIVMSG" Ch #:trailing text))))

  (on (message (ircd-event this-conn $m))
      (send-irc-message m))

  (on (message (ircd-action $other-conn (irc-privmsg $source (nick) $text)))
      (when (not (equal? other-conn this-conn))
        (send* #:source source "PRIVMSG" (nick) #:trailing text)))

  (on (message (tcp-channel-line this-conn server-handle $bs))
      (define m (parse-irc-message (bytes->string/utf-8 bs)))
      (log-info "~a -> ~v" this-conn m)
      (send! (ircd-action this-conn m))
      (match m
        [(irc-message _ "PING" _ _) (void)] ;; RFC says servers don't reply to PINGs
        [(or (irc-message _ "NICK" (list N) _)
             (irc-message _ "NICK" '() N)) ;; libpurple does this (!)
         ;; TODO: enforce syntactic restrictions on nick
         (if (immediate-query [query-value #f (ircd-connection-info _ N _) #t])
             (send* 433 N #:trailing "Nickname is already in use")
             (begin (when (nick) (send* #:source (irc-source-nick (nick) (user)) "NICK" N))
                    (nick N)))]
        [(irc-message _ "USER" (list U _Hostname _Servername) R)
         ;; TODO: enforce syntactic restrictions on parameters to USER
         (define H (tcp-address-host this-conn))
         (user (irc-user U H R))]
        [(irc-message _ "QUIT" _ _) (stop-current-facet)]
        [_
         (when (and (nick) (user))
           (match m
             [(irc-message _ "JOIN" (cons Channels _MaybeKeys) _)
              (for [(Ch (string-split Channels #px",+"))]
                (assert! (ircd-channel-member Ch this-conn)))]
             [(irc-message _ "PART" (list Channels) _)
              (for [(Ch (string-split Channels #px",+"))]
                (retract! (ircd-channel-member Ch this-conn)))]
             [(irc-message _ "WHOIS" _ _)
              (send* 318 (nick) #:trailing "End of /WHOIS list")] ;; TODO
             [(irc-message _ "PRIVMSG" (list Targets) Text)
              (for [(T (string-split Targets #px",+"))]
                (send! (ircd-action this-conn
                                    (irc-privmsg (irc-source-nick (nick) (user)) T Text))))]
             [_ (void)]))])))

(spawn #:name 'session-listener-factory
       (stop-when-reloaded)
       (during/spawn (ircd-listener $port)
         #:name (ircd-listener port)
         (on-start (log-info "Listening on port ~a." port))
         (on-stop (log-info "No longer listening on port ~a." port))
         (define server-handle (tcp-listener port))
         (assert (advertise (observe (tcp-channel _ server-handle _))))
         (during/spawn (advertise (tcp-channel $this-conn server-handle _))
           #:name `(ircd-connection ,this-conn ,server-handle)
           (assert (advertise (tcp-channel server-handle this-conn _)))
           (ircd-connection-facet this-conn server-handle))))
