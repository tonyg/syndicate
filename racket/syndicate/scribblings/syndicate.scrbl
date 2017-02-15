#lang scribble/manual

@(require (for-label (except-in racket process field)
            syndicate/actor))

@title{High Level Syntax for Syndicate}


@defmodule[syndicate/actor]

@section{Instantaneous Actions (I)}

@defform[(spawn I ...)]{
Spawns an actor that executes each instantaneous action @racket[I] in
sequence.}

@defform[(dataspace I ...)]{
Spawns a dataspace as a child of the dataspace enclosing the executing actor. The
new dataspace executes each instantaneous action @racket[I].}

@defproc[(send! [v any/c]
                [#:meta-level level natural-number/c 0])
         void?]{
Sends a message with body @racket[v]. The message is sent @racket[level]
dataspaces removed from the dataspace containing the actor performing the
@racket[send!].}

@defproc[(assert! [v any/c]
                  [#:meta-level level natural-number/c 0])
         void?]{
Asserts the value of @racket[v] until either explicitly retracted via
@racket[retract!] or the immediately enclosing actor exits. @racket[level]
specifies which dataspace the assertion should be made, in terms of relative
distance from the dataspace containing the enclosing actor.}

@defproc[(retract! [v any/c]
                   [#:meta-level level natural-number/c 0])
         void?]{
Retracts any assertions made by the immediately enclosing actor at
@racket[level] dataspaces above the enclosing dataspace of the form @racket[v].}

@section{Ongoing Behaviors (O)}

@defform[(state maybe-init (maybe-bindings O ...) ([E I ...] ...))
         #:grammar
         [(maybe-init (code:line)
                      (code:line #:init [I ...]))
          (maybe-bindings (code:line)
                          (code:line #:collect ([id init] ...)))]
         #:contracts ([id identifier?])]{
Spawns a new actor with ongoing behaviors @racket[O ...] that runs until a
termination event is detected.

The optional @racket[#:init [I ...]] provides a sequence of initialization
actions. The initial actions are executed before the ongoing behaviors begin but
after the interests of the state actor are established.

The optional @racket[#:collect [(id init) ...]] clause introduces bindings that
are visible within the body of the state actor. Each binding @racket[id] is
initialized to the corresponding @racket[init] expression. The bindings are
updated when an ongoing behavior executes an instantaneous event, such as the
result of an @racket[on] behavior. The new bindings are in the form of a
@racket[values] form, with the new values in the same order and number as in the
@racket[#:collect].

The ongoing behaviors @racket[O ...] are run simultaneously until the state
actor exits.

Each @racket[[E I ...]] specifies a termination event @racket[E] of the actor.
When a termination event @racket[E] activates, the corresponding @racket[I]s are
executed. The state actor then exits, with the same result of the final
@racket[I] action.}

@defform[(until E
                maybe-init
                maybe-bindings
                maybe-done
                O ...)
         #:grammar
         [(maybe-init (code:line)
                      (code:line #:init [I ...]))
          (maybe-bindings (code:line)
                          (code:line #:collect ([id init] ...)))
          (maybe-done (code:line)
                      (code:line #:done [I ...]))]
         #:contracts ([id identifier?])]{
An @racket[until] behavior corresponds to a @racket[state] behavior with only
one termination event, given by @racket[E]. The final result of the
@racket[until] behavior is the values of the @racket[#:collect] bindings in
scope from any parent actors followed by the final values of the @racket[until]
actor's bindings. The actions in a @racket[#:done] clause are executed after the
termination event but before the @racket[until] actor exits.}

@defform[(forever maybe-init
           maybe-bindings
           O ...)
         #:grammar
         [(maybe-init (code:line)
                      (code:line #:init [I ...]))
          (maybe-bindings (code:line)
                          (code:line #:collect ([id init] ...)))]
         #:contracts ([id identifier?])]{
The @racket[forever] behavior is analogous to a @racket[state] form with no
termination events.}

@defform[(during pat O ...)]{
Runs the behaviors @racket[O ...] for the duration of each assertion matching
@racket[pat].

Roughly equivalent to
@racket[(on (asserted pat)
            (until (retracted pat)
                   O ...))]
where the @racket[pat] in the @racket[until] clause is specialized to the actual
value matched by @racket[pat] in the @racket[asserted] clause.
}

@defform[(assert maybe-pred exp maybe-level)
         #:grammar
         [(maybe-pred (code:line)
                      (code:line #:when pred))
          (maybe-level (code:line)
                       (code:line #:meta-level level))]
         #:contracts ([pred boolean?]
                      [level natural-number/c])]{
Makes the assertion @racket[exp] while the enclosing actor is running. If a
@racket[#:when] predicate is given, the assertion is made conditionally on the
predicate expression evaluating to true.}

@defform[(on E
             I ...)]{
When the event @racket[E] becomes active, executes the instantaneous actions
@racket[I ...] in the body. The result of the final action is the result of the
entire behavior.}

@section{Events (E)}

@defform[(message pat)]{
Activates when a message is received with a body matching @racket[pat].
The message event establishes the enclosing actor's interest in @racket[pat].}

@defform[(asserted pat)]{
Activates when a patch is received with an added assertion matching
@racket[pat]. Establishes the enclosing actor's interest in @racket[pat].}

@defform[(retracted pat)]{
Similar to @racket[asserted], except for assertions removed in a patch.}

@defform[(rising-edge expr)]{
Activates when @racket[expr] evaluates to anything besides @racket[#f] (having
previously evaluated to @racket[#f]). The condition is checked after each
received event, corresponding to after each instantaneous action is executed.}

@section{Patterns}

@(racketgrammar
  pat
  (code:line)
  (code:line _)
  (code:line $id)
  (code:line ($ id pat))
  (code:line (? pred pat))
  (code:line (ctor pat ...))
  (code:line expr))

@racket[_] matches anything.

@racket[$id] matches anything and binds the value to @racket[id].

@racket[($ id pat)] matches values that match @racket[pat] and binds the value
to @racket[id].

@racket[(? pred pat)] matches values where @racket[(pred val)] is not
@racket[#f] and that match @racket[pat].

@racket[(ctor pat ...)] matches values built by applying the constructor
@racket[ctor] to values matching @racket[pat ...].

@racket[expr] patterns match values that are exactly equal to @racket[expr].

