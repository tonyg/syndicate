#lang scribble/manual

@(require (for-label (except-in racket process field send)
            racket/async-channel
            (only-in syndicate/actor)
            ))

@(require (for-label (only-in syndicate/interactive-lang
                                         quit
                                         assert
                                         retract
                                         spawn
                                         send
                                         receive)))


@title{Interactive Programming}

@defmodule[syndicate/interactive-lang]

The @hash-lang[] @racket[syndicate/interactive] language provides an interface
to running dataspace programs. The interface allows events and actors to be
introduced externally and for querying the contents of the dataspace.

@section{Synchronous Interactions}

The following procedures communicate synchronously with a driver actor running
in the dataspace.

@defproc[(quit) void?]{
Terminates the dataspace.
}

@deftogether[(@defproc[(assert [pat pattern]) void?]
              @defproc[(retract [pat pattern]) void?])]{
Introduce or withdraw an assertion in the dataspace based on the supplied
pattern. Note that only assertions made through the REPL interface may be
retracted in this manner.
}

@defproc[(send [val any/c]) void?]{
Broadcasts a message in the dataspace.
}

@defform[(spawn EI...+)]{
Spawn an actor in the dataspace.
}

@deftogether[(@defproc[(do-query [proj projection]) trie?]
              @defproc[(do-query/value [proj projection] [default any/c #f]) any/c]
              @defproc[(do-query/set [proj projection]) set?])]{
Query the contents of the dataspace via a projection.
}

@racketgrammar[#:literals (? ?!) projection
               (code:line ?)
               (code:line (?!))
               (code:line (ctor projection ...))]
              
@defproc[(receive [pat pattern]) any/c]{
Wait for a message broadcast in the dataspace matching the supplied pattern.
Returns the body of the first such matching message.
}

@section{Asynchronous Interactions}
The following forms provide an asynchronous interface to running dataspace
programs. Each procedure returns an asynchronous channel that is ready when the
request has been performed and yields the result, if any.

@deftogether[(@defproc[(do-quit/async) async-channel?]
              @defproc[(do-assert/async [pat pattern]) async-channel?]
              @defproc[(do-retract/async [pat pattern]) async-channel?]
              @defproc[(do-send/async [val any/c]) async-channel?]
              @defproc[(do-spawn/async [boot (-> any/c)]) async-channel?]
              @defproc[(do-query/async [proj projection]) async-channel?]
              @defproc[(do-query/value/async [proj projection] [default any/c #f]) async-channel?]
              @defproc[(do-query/set/async [proj projection]) async-channel?]
              @defproc[(do-receive/async [pat pattern]) async-channel?]
              )]{
Asynchronous commands.
}
