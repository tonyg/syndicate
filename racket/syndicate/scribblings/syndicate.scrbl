#lang scribble/manual

@(require (for-label (except-in racket process field)
            syndicate/actor))

@title{Dataspace Programming with Syndicate}


@defmodule[syndicate/actor]

@section{Overview}

Syndicate is an actor language where all communication occurs through a tightly
controlled shared memory, dubbed the @emph{dataspace}. The values in the
dataspace are called @emph{assertions}, representing the information that the
actors in the system are currently sharing with each other. Assertions are
@emph{read-only} and @emph{owned} by the actor that entered them into the
dataspace. Only the originating actor has permission to withdraw an assertion.
Assertions are linked to the lifetime of their actor, and are withdrawn from the
dataspace when that actor exits, either normally or via exception.

To respond to assertions in the dataspace, an actor expresses an @emph{interest}
in the shape of assertions it wishes to receive. An interest is an assertion
constructed with @racket[observe] and wildcards where the actor wishes to
receive any matching assertion. When an actor makes an assertion of interest,
the dataspace dispatches the set of all matching assertions to that actor.
Moreover, the dataspace keeps the actor @emph{up-to-date}, informing it when a
new assertion appears matching its interest, as well as when a matching
assertion disappears from the dataspace. Thus, dataspaces implement a form of
publish/subscribe communication.

@;{would be nice to link pub/sub}

In addition to assertions, dataspaces support instantaneous @racket[message]
broadcast. At the time a message is sent, all actors with a matching interest
receive notification.

In response to an event, that is, a message broadcast or assertion
appearance/disappearance matching an expressed interest, a Syndicate actor may
take any of the following actions:
@itemlist[
  @item{Updating its internal state;}
  @item{Making or withdrawing assertions;}
  @item{Sending broadcast messages;}
  @item{Spawning additional actors;}
  @item{Exiting;}
  @item{Or any combination of these.}
]

Thus, each individual Syndicate actor has three fudamental concerns:

@itemlist[
  @item{Defining local state and updating it in response to events;}
  @item{Publishing aspects of local state/knowledge as assertions; and}
  @item{Reacting to relevant assertions and messages.}
]

Each concern is addressed by a separate language construct, which are
collectively dubbed @emph{endpoints}:

@itemlist[
  @item{The @racket[field]s of an actor hold its state;}
  @item{An actor publishes information using @racket[assert]; and}
  @item{An event-handler endpoint uses @racket[on] to define reactions to
  particular messages and assertions.}
]

Endpoints are tied together via @emph{dataflow}. Thus, the assertions of an
actor automatically reflect the current value of its fields.

Implementing an actor's role in a particular conversation typically involves
some combination of these behaviors; a @emph{facet} is a collection of related
endpoints constituting the actor's participation in a particular conversation.

Each actor starts with a single facet, and may add new facets or terminate
current ones in response to events. The facets of an actor form a tree, where
the parent of a particular facet is the facet in which it was created. The tree
structure affects facet shutdown; terminating a facet also terminates all of its
descendants.

To recap: an actor is a tree of facets, each of which comprises of a collection
of endpoints.

@section{Programming Syndicate Actors with Facets}

Code within Syndicate actors executes in one of two contexts:
@itemlist[
  @item{The @emph{endpoint-installation} context occurs during the creation of a
  new facet, when all of its endpoints are created.}
  @item{The @emph{script} context occurs during the execution of event handlers,
  and permits creating/terminating facets, sending messages, and spawning
  actors.}
]

The actions permitted by the two contexts are mutually exclusive, and trying to
perform an action in the wrong context will give rise to a run-time
@racket[error].

Within the following descriptions, we use @emph{EI} as a shorthand for
expressions that execute in an endpoint-installation context and @emph{S} for
expressions in a script context.

@defform[(spawn EI ...)]{
Spawn an actor with a single inital facet whose endpoints are installed by
@racket[EI]. That is, there is an implicit @racket[react] around @racket[EI ...].}


@defproc[(send! [v any/c]
                [#:meta-level level natural-number/c 0])
         void?]{
Sends a message with body @racket[v]. The message is sent @racket[level]
dataspaces removed from the dataspace containing the actor performing the
@racket[send!].}

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

@section{Actors with an Agenda}

Here we talk about @racket[spawn*] and @racket[react/suspend].

@section{Odds and Ends}

@defform[(dataspace I ...)]{
Spawns a dataspace as a child of the dataspace enclosing the executing actor. The
new dataspace executes each instantaneous action @racket[I].}

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
