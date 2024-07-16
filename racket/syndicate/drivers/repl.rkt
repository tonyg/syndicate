#lang syndicate

(provide do-quit
         do-assert
         do-retract
         do-send
         do-spawn
         do-query
         do-together
         do-quit/async

         do-assert/async
         do-retract/async
         do-send/async
         do-spawn/async
         do-query/async
         do-together/async


         QUIT
         instr:assert
         instr:retract
         instr:send
         instr:spawn
         instr:query
         instr:together

         boot-repl
         )

(require (prefix-in core: syndicate/core)
         syndicate/supervisor)
(require racket/async-channel)

#|
An Instruction is one of:
- 'quit
- (instr:assert Pattern)
- (instr:retract Pattern)
- (instr:send Any)
- (instr:spawn (Sealof (-> Spawn)))
- (instr:query Pattern)
- (instr:together (Listof Instruction))
|#

(define QUIT 'quit)
(message-struct instr:assert (pat))
(message-struct instr:retract (pat))
(message-struct instr:send (v))
(message-struct instr:spawn (boot))
(message-struct instr:query (p))
(message-struct instr:together (instrs))

#|
A Command is (command ID Instruction)
where ID is any value that uniquely identifies this command

|#

(message-struct command (resp-channel instr))

(message-struct delegated-instruction (resp-channel instr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client Interface

(define (do instr)
  (async-channel-get (do/async instr)))

(define (do/async instr)
  (define resp-channel (make-async-channel))
  (send-ground-message (command resp-channel instr))
  resp-channel)

(define (do-quit) (do QUIT))
(define (do-assert pat) (do (instr:assert pat)))
(define (do-retract pat) (do (instr:retract pat)))
(define (do-send pat) (do (instr:send pat)))
(define (do-spawn boot) (do (instr:spawn boot)))
(define (do-query pat) (do (instr:query pat)))
(define (do-together . instrs) (do (instr:together instrs)))

(define (do-quit/async) (do/async QUIT))
(define (do-assert/async pat) (do/async (instr:assert pat)))
(define (do-retract/async pat) (do/async (instr:retract pat)))
(define (do-send/async pat) (do/async (instr:send pat)))
(define (do-spawn/async boot) (do/async (instr:spawn boot)))
(define (do-query/async pat) (do/async (instr:query pat)))
(define (do-together/async . instrs) (do/async (instr:together instrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver Actors

(define (spawn-command-handler)
  (spawn
    (on (message (inbound (command $resp-channel $instr)))
        (log-repl-info "repl received instruction: ~a" instr)
        (perform! resp-channel instr))))

(define (perform! resp-channel instr)
  (match instr
    [(== QUIT)
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
    [(instr:together instrs)
     (define dummy-channel (make-async-channel))
     (for ([instr instrs])
       (perform! dummy-channel instr))
     (async-channel-put resp-channel 'ok)]))

(define (delegate! resp-channel instr)
  (log-repl-info "delegating instruction: ~a" instr)
  (send! (delegated-instruction resp-channel instr)))

(define (spawn-querier resp-channel pat)
  (perform-actions! (list (make-querier resp-channel pat))))

(define (make-querier resp-channel pat)
  (define (query-behavior e boot?)
    (log-repl-info "querier receives event: ~a" e)
    (cond
      [(patch? e)
       (async-channel-put resp-channel (patch-added e))
       (core:quit)]
      [(and (not e) boot?)
       (transition #f '())]
      [(not boot?)
       (async-channel-put resp-channel trie-empty)
       (core:quit)]
      [else
       #f]))
  (core:actor query-behavior #t (sub pat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver Instantiation

(define (boot-repl)
  (supervisor (list (child-spec 'command-handler spawn-command-handler))
              #:strategy ONE-FOR-ONE
              #:name 'repl-supervisor))

(boot-repl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging

(define (log-repl-info fmt . args)
  (apply printf fmt args)
  (newline))
