#lang syndicate
;; Terminal-based IRC client.

(require/activate syndicate/drivers/irc)
(require (only-in racket/port read-bytes-line-evt))

(define NICK "syndicateuser")
(define C (irc-connection "irc.freenode.net" 6667 NICK))

(define stdin-evt (read-bytes-line-evt (current-input-port) 'any))

(assertion-struct join (channel))

(spawn #:name 'input-handler
       (field [active-channel #f])
       (begin/dataflow
         (match (active-channel)
           [#f (printf "No destination for typed text is selected.\n")]
           [dest (printf "Typed text will go to ~a.\n" dest)]))
       (assert C)
       (stop-when (message (inbound (external-event stdin-evt (list (? eof-object? _)))))
         (printf "Goodbye!\n"))
       (on (message (inbound (external-event stdin-evt (list (? bytes? $line)))))
           (match (bytes->string/utf-8 line)
             [(pregexp #px"^/join (#.*)$" (list _ chan))
              (assert! (join chan))
              (active-channel chan)]
             [(pregexp #px"^/part( (#.*))?$" (list _ _ chan))
              (let ((chan (or chan (active-channel))))
                (retract! (join chan))
                (when (equal? (active-channel) chan)
                  (active-channel #f)))]
             [(pregexp #px"^/select (.*)$" (list _ chan))
              (active-channel chan)]
             [(pregexp #px"^/" (list _))
              (printf "Bad command.\n")]
             [line
              (if (active-channel)
                  (send! (irc-outbound C (active-channel) line))
                  (printf "No destination for typed text is selected. Use /join or /select.\n"))]))
       (on (message (irc-inbound C $who NICK $body))
           (printf "DIRECT: ~a: ~a\n" who body)))

(spawn #:name 'room-monitor-factory
       (during/spawn (join $chan)
         (on-start (printf "Joining channel ~a.\n" chan))
         (on-stop (printf "Left channel ~a.\n" chan))
         (on (message (irc-inbound C $who chan $body))
             (printf "~a: ~a: ~a\n" chan who body))
         (during (irc-presence C $who chan)
           (on-start (printf "~a: ~a joined\n" chan who))
           (on-stop (printf "~a: ~a left\n" chan who)))))
