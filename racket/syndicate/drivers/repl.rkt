#lang syndicate

(provide do-quit
         do-assert
         do-retract
         do-send
         do-spawn
         do-query
         do-receive
         do-together
         do-query/set
         do-query/value

         do-quit/async
         do-assert/async
         do-retract/async
         do-send/async
         do-spawn/async
         do-query/async
         do-receive/async
         do-together/async
         do-query/set/async
         do-query/value/async

         repl-assert
         repl-spawn

         instr:quit
         instr:assert
         instr:retract
         instr:send
         instr:spawn
         instr:query
         instr:together

         boot-repl
         )

(require (prefix-in core: syndicate/core)
         syndicate/trie
         syndicate/supervisor
         syndicate/pattern)
(require racket/async-channel
         syntax/parse/define
         (for-syntax racket/syntax)
         racket/set)

#|
An Instruction is one of:
- instr:quit
- (instr:assert Pattern)
- (instr:retract Pattern)
- (instr:send Any)
- (instr:spawn (Sealof (-> Spawn)))
- (instr:query Pattern)
- (instr:receive Pattern)
- (instr:together (Listof Instruction))
|#

#|
A Command is (command ID Instruction)
where ID is any value that uniquely identifies this command

|#

(message-struct command (resp-channel instr))

(message-struct delegated-instruction (resp-channel instr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL Instructions

(define (do instr)
  (async-channel-get (do/async instr)))

(define (do/async instr)
  (define resp-channel (make-async-channel))
  (send-ground-message (command resp-channel instr))
  resp-channel)

(define-syntax-parser define-instruction
  [(_ name)
   #:with instr-name (format-id #'name "instr:~a" #'name)
   #:with do-name (format-id #'name "do-~a" #'name)
   #:with do/async-name (format-id #'name "do-~a/async" #'name)
   #'(begin
       (define instr-name 'name)
       (define (do-name) (do instr-name))
       (define (do/async-name) (do/async instr-name)))]
  [(_ name (args ...))
   #:with instr-name (format-id #'name "instr:~a" #'name)
   #:with do-name (format-id #'name "do-~a" #'name)
   #:with do/async-name (format-id #'name "do-~a/async" #'name)
   #'(begin
       (message-struct instr-name (args ...))
       (define (do-name args ...) (do (instr-name args ...)))
       (define (do/async-name args ...) (do/async (instr-name args ...))))])

(define-instruction quit)
(define-instruction assert (pat))
(define-instruction retract (pat))
(define-instruction send (v))
(define-instruction spawn (boot))
(define-instruction query (pat))
(define-instruction receive (pat))
(define-instruction together (instrs))

(define (do-query/set proj)
  (sync (do-query/set/async proj)))

(define (do-query/set/async proj)
  (define pat (projection->pattern proj))
  (define query-chan (do-query/async pat))
  (handle-evt query-chan
              (lambda (query-result) (trie-project/set/single query-result proj))))

(define (do-query/value proj [default #f])
  (sync (do-query/value/async proj default)))

(define (do-query/value/async proj [default #f])
  (define query-chan (do-query/set/async proj))
  (handle-evt query-chan
              (lambda (query-result)
                (if (set-empty? query-result)
                    default
                    (set-first query-result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience Wrappers

(define (repl-assert v)
  (if (pair? (current-facet-id))
      (assert v)
      (do-assert v)))

(define-syntax-parse-rule (repl-spawn e ...+)
  #:with boot-expr #'(spawn e ...)
  (if (syndicate-effects-available?)
      boot-expr
      (do-spawn (lambda () boot-expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver Actors

(define (spawn-command-handler ready-chan)
  (define instance (gensym 'instance))
  (log-repl-info "repl ~a created" instance)
  (spawn
    (on-start (async-channel-put ready-chan #t))
    (on (message (inbound (command $resp-channel $instr)))
        (log-repl-info "repl ~a received instruction: ~a" instance instr)
        (perform! resp-channel instr))))

(define (perform! resp-channel instr)
  (match instr
    [(== instr:quit)
     (async-channel-put resp-channel 'ok)
     (quit-dataspace!)]
    [(instr:send v)
     (send! v)
     (async-channel-put resp-channel 'ok)]
    [(instr:spawn boot)
     (boot)
     (async-channel-put resp-channel 'ok)]
    [(instr:assert pat)
     (assert! pat)
     (async-channel-put resp-channel 'ok)]
    [(instr:retract pat)
     (retract! pat)
     (async-channel-put resp-channel 'ok)]
    [(instr:query pat)
     (spawn-querier resp-channel pat)]
    [(instr:receive pat)
     (spawn-querier resp-channel pat #:message? #t)]
    [(instr:together instrs)
     (define dummy-channel (make-async-channel))
     (for ([instr instrs])
       (perform! dummy-channel instr))
     (async-channel-put resp-channel 'ok)]))

(define (delegate! resp-channel instr)
  (log-repl-info "delegating instruction: ~a" instr)
  (send! (delegated-instruction resp-channel instr)))

(define (spawn-querier resp-channel pat #:message? [m? #f])
  (perform-actions! (list (make-querier resp-channel pat m?))))

(define (make-querier resp-channel pat m?)
  (define token (gensym 'query-token))
  (define (query-behavior e boot?)
    (log-repl-info "querier receives event: ~a" e)
    (cond
      [(and (patch? e) (not m?))
       (define added (patch-added e))
       (define token-trie (trie-project added (?! token)))
       (define without-token (trie-subtract added token-trie))
       (async-channel-put resp-channel without-token)
       (core:quit)]
      [(and (message? e) m?)
       (async-channel-put resp-channel (message-body e))
       (core:quit)]
      [else
       #f]))
  (core:actor query-behavior #t (list (core:sub pat)
                                      (core:assert token)
                                      (core:sub token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver Instantiation

(define (boot-repl #:when-ready [ready-chan (make-async-channel)])
  (supervisor (list (child-spec 'command-handler (lambda () (spawn-command-handler ready-chan))))
              #:strategy ONE-FOR-ONE
              #:name 'repl-supervisor))

(boot-repl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging

(define (log-repl-info fmt . args)
  (apply printf fmt args)
  (newline))
