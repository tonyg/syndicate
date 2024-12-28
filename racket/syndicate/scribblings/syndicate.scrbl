#lang scribble/manual

@(require (for-label (except-in racket process field)
            racket/async-channel
            syndicate/actor
            ))

@title{Dataspace Programming with Syndicate}

@author["Sam Caldwell"]

@table-of-contents[]

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

@subsection{Script Actions: Starting and Stopping Actors and Facets}

@defform[(spawn maybe-name
                maybe-assertions
                maybe-linkage
                EI ...+)
         #:grammar
         [(maybe-name (code:line)
                      (code:line #:name name-expr))
          (maybe-assertions (code:line)
                            (code:line #:assertions assertion-expr)
                            (code:line #:assertions* assertions-expr))
          (maybe-linkage (code:line)
                         (code:line #:linkage [linkage-expr ...]))]
         #:contracts
         ([assertion-expr any/c]
          [assertions-expr trie?])]{
Spawn an actor with a single inital facet whose endpoints are installed by
@racket[EI]. That is, there is an implicit @racket[react] around @racket[EI
...]. Allowed within a script and module-top-level.

An optionally provided @racket[name-expr] is associated with the created actor.
The name is only used for error and log messages, thus is mainly useful for
debugging.

The actor may optionally be given some initial assertions, which come into being
at the same time as the actor. (Otherwise, the actor spawns, then boots its
initial facet(s), then establishes any ensuing assertions.) When
@racket[assertion-expr] is provided, the actors initial assertions are the
result of interpreting the expression as a @racket[trie] pattern, with
@racket[?] giving rise to infinte sets. On the other hand,
@racket[assertions-expr] may be used to specify an entire set of initial
assertions as an arbitrary @racket[trie].

The optional @racket[linkage-expr]s are executed during facet startup; your
simple documentation author is not sure why they are useful, as opposed to just
putting them in the body of the @racket[spawn].
}

@defform[(react EI ...+)]{
Create a new facet in the current actor whose endpoints are the result of
executing @racket[EI ...]. Allowed within a script.
}

@defform[(stop-facet fid S ...)
         #:contracts ([fid facet-id?])]{
Terminate the facet with ID @racket[fid], as well as all of its children.
Allowed within a script.

The optional script actions @racket[S ...] function like a continuation. They
run @emph{after} the facet and all of its children finish shutting down, i.e.
after all @racket[stop] handlers have executed. Moreover, @racket[S ...] runs in
the context of the @emph{parent} of @racket[fid]. Thus, any facet created by the
script survives termination and will have @racket[fid]'s parent as its own
parent.

Note that @racket[fid] must be an ancestor of the current facet.
}

@defform[(stop-current-facet S ...)]{
Stop the currently running facet; equivalent to
@racketblock[(stop-facet (current-facet-id) S ...)].

Allowed within a script.
}

@defproc[(current-facet-id) facet-id?]{
Retrieves the ID of the currently running facet.
}

@defproc[(send! [v any/c])
         void?]{
Sends a @racket[message] with body @racket[v].

Allowed within a script.
}

@subsection{Installing Endpoints}

@defform[(field [x init-expr maybe-contract] ...+)
         #:grammar
         [(maybe-contract (code:line)
                          (code:line #:contract in)
                          (code:line #:contract in out))]]{
Define fields for the current facet. Each @racket[x] is bound to a handle
function: calling @racket[(x)] retrieves the current value, while @racket[(x v)]
sets the field to @racket[v].

Fields may optionally have a contract; the @racket[in] contract is applied when
writing to a field, while the (optional) @racket[out] contract applies when
reading a value from a field.

Allowed within an endpoint installation context.
}

@defform[(assert maybe-pred exp)
         #:grammar
         [(maybe-pred (code:line)
                      (code:line #:when pred))]
         #:contracts ([pred boolean?])]{
Make the assertion @racket[exp] while the enclosing facet is active. Publishing
the assertion can be made conditional on a boolean expression by supplying a
@racket[#:when] predicate, in which case the assertion is made only when
@racket[pred] evaluates to a truthy value.

If the expression @racket[exp] refers to any fields, then the assertion created
by the endpoint is automatically kept up-to-date each time any of those fields
is updated. More specifically, the will issue a patch retracting the assertion
of the previous value, replacing it with the results of reevaluating
@racket[exp] with the current values of each field.

Allowed within an endpoint installation context.
}

@defform[#:literals (message asserted retracted _ $ ?)
         (on maybe-pred event-description
             S ...+)
         
         #:grammar
         [(maybe-pred (code:line)
                      (code:line #:when pred))
          (event-description (code:line (message pattern))
                             (code:line (asserted pattern))
                             (code:line (retracted pattern)))
          (pattern (code:line _)
                   (code:line $id)
                   (code:line ($ id pattern))
                   (code:line (? pred pattern))
                   (code:line (ctor pattern ...))
                   (code:line expr))]
         #:contracts ([pred boolean?])]{
Creates an event handler endpoint that responds to the event specified by
@racket[event-description]. Executes the body @racket[S ...] for each matching
event, with any pattern variables bound to their matched value.

The actor will make an assertion of interest in events that could match
@racket[event-description]. Like with @racket[assert], the interest will be
refreshed any time a field referenced within the @racket[event-description]
pattern changes.

The event handler can optionally be made conditional on a boolean expression by
supplying a @racket[#:when] predicate, in which case the endpoint only reacts to
events, and only expresses the corresponding assertion of interest, when
@racket[pred] evaluates to a truthy value.

Allowed within an endpoint installation context.

Event descriptions have one of the following forms:
@itemlist[
  @item{@racket[(message pattern)] activates when a message is received with a
    body matching @racket[pat].}

  @item{@racket[(asserted pattern)] activates when a patch is received with an
    added assertion matching @racket[pattern]. Additionally, if the actor has
    @emph{already} received a patch with matching assertions, which can occur if
    multiple facets in a single actor have overlapping interests, then the
    endpoint will match those assertions upon facet start up.}

  @item{@racket[(retracted pat)] is similar to @racket[asserted], but for
    assertions withdrawn in a patch.}

  @;{@item{@racket[(rising-edge expr)] activates when @racket[expr] evaluates to
    anything besides @racket[#f] (having previously evaluated to @racket[#f]). The
    condition is checked after each received event.}}
]

While patterns have the following meanings:
@itemlist[
  @item{@racket[_] matches anything.}

  @item{@racket[$id] matches anything and binds the value to @racket[id].}

  @item{@racket[($ id pattern)] matches values that match @racket[pattern] and
    binds the value to @racket[id].}

  @item{@racket[(? pred pattern)] matches values where @racket[(pred val)] is not
     @racket[#f] and that match @racket[pattern].}

  @item{@racket[(ctor pat ...)] matches values built by applying the constructor
    @racket[ctor] to values matching @racket[pat ...]. @racket[ctor] is usually
    a @racket[struct] name.}

  @item{@racket[expr] patterns match values that are @racket[equal?] to
    @racket[expr].}
]
}

@defform[(during pattern EI ...+)]{
Engage in behavior for the duration of a matching assertion. Roughly equivalent
to:

@racketblock[
(on (asserted pattern)
  (react
    EI ...
    (on (retracted inst-pattern)
        (stop-current-facet))))]

where @racket[inst-pattern] is the @racket[pattern] with variables instantiated
to their matching values.

Allowed within an endpoint installation context.
}

@defform[(during/spawn pattern
                        maybe-actor-wrapper
                        maybe-name
                        maybe-assertions
                        maybe-parent-let
                        maybe-on-crash
                        EI ...)
          #:grammar
          [(maybe-actor-wrapper (code:line)
                                (code:line #:spawn wrapper-stx))
           (maybe-parent-let (code:line)
                             (code:line #:let [x expr] ...))
           (maybe-on-crash (code:line)
                           (code:line #:on-crash on-crash-expr))]]{
Like @racket[during], but in addition to creating a new facet for each matching
assertion, @racket[spawn]s a new actor. The difference is primarily relevant for
error propagation; an exception inside @racket[during] causes the entire actor
to crash, while an exception inside @racket[during/spawn] crashes only the newly
spawned actor.

The assertion triggering the @racket[during/spawn] may disappear @emph{before}
the spawned actor boots, in which case it fails to see the retraction event. To
avoid potential glitches, the @emph{spawning} actor maintains an assertion that
lets the @racket[spawned] actor know whether the originial assertion still
exists.

The @racket[maybe-name] and @racket[maybe-assertions] have the same meaning they
do for @racket[spawn], applied to the newly spawned actor.

The @racket[wrapper-stx] serves as an interposition point; it may be provided to
change the meaning of "spawning an actor" in response to an assertion. By
default, it is @racket[#'spawn].

The optional @racket[#:let] clauses can be used to read the values of fields in
the @emph{spawning} actor so that they can be used in the @emph{spawned} actor.
Otherwise, the spawned actor has no access to the parent's fields, and trying to
read or write to such a field will cause a runtime @racket[error].

The @racket[on-crash-expr] provides a hook for script actions that can be
performed in the @emph{spawning} actor if the @emph{spawned} actor crashes.

Allowed within an endpoint installation context.
}

@defform[(stop-when maybe-pred event-description S ...)
         #:grammar
         [(maybe-pred (code:line)
                      (code:line #:when pred))]
         #:contracts ([pred boolean?])]{
Stop the current facet when an event matching @racket[event-description] occurs.
Roughly equivalent to
@racketblock[
(on event-description
    (stop-current-facet S ...))]

Allowed within an endpoint installation context.
}

@subsection{Handling Facet Startup and Shutdown}

In addition to external events, such as assertion (dis)appearance and message
broadcast, facets can react to their own startup and shutdown. This provides a
handy way to perform initialization, cleanup, as well as setting up and tearing
down resources.

@defform[(on-start S ...)]{
Perform the script actions @racket[S ...] upon facet startup.

Allowed within an endpoint installation context.
}

@defform[(on-stop S ...)]{
Perform the script actions @racket[S ...] upon facet shutdown.

The script @racket[S ...] differs from that of @racket[stop-facet] in that it
executes in the context of the terminating facet, not its parent. Thus, any
facets created in @racket[S ...] will start up and then immediately shut down.

Allowed within an endpoint installation context.
}

Note that a single facet may have any number of @racket[on-start] and
@racket[on-stop] handlers, which do not compete with each other. That is, each
@racket[on-start] handler runs during facet startup and, likewise, each
@racket[on-stop] during facet shutdown.

@subsection{Streaming Query Fields}

Syndicate actors often aggregate information about current assertions as part of
their local state, that is, in a @racket[field]. Since these patterns are
exceedingly common, Syndicate provides a number of forms for defining fields
that behave as streaming queries over the assertions in the dataspace.

@defform[(define/query-set name pattern expr maybe-on-add maybe-on-remove)
         #:grammar
         [(maybe-on-add (code:line)
                        (code:line #:on-add on-add-expr))
          (maybe-on-remove (code:line)
                           (code:line #:on-remove on-remove-expr))]]{
Define a @racket[field] called @racket[name] that is the @racket[set] of values
extracted from assertions matching @racket[pattern]. Each value is extracted
from a matching assertion by evaluating @racket[expr], which may refer to
variables bound by @racket[pattern].

The query set expands to roughly the following code:
@racketblock[
(begin
  (field [name (set)])
  (on (asserted pattern)
      (name (set-add (name) expr)))
  (on (retracted pattern)
      (name (set-remove (name) expr))))]
    
The optional @racket[on-add-expr] is performed inside the @racket[on asserted]
handler, while @racket[on-remove-expr] runs in the @racket[on retracted]
handler.

Allowed within an endpoint installation context.
}

@defform[(define/query-hash name pattern key-expr value-expr
                              maybe-on-add
                              maybe-on-remove)
         #:grammar
         [(maybe-on-add (code:line)
                        (code:line #:on-add on-add-expr))
          (maybe-on-remove (code:line)
                           (code:line #:on-remove on-remove-expr))]]{
Define a @racket[field] called @racket[name] that is a @racket[hash] based on
assertions matching @racket[pattern]. Each matching assertion establishes a key
in the hash based on @racket[key-expr] whose value is the result of
@racket[value-expr], with each expression referring to variables bound by
@racket[pattern]. When a matching assertion disappears from the dataspace, the
associated key is removed from the hash.

The optional @racket[maybe-on-add] and @racket[maybe-on-expr] behave the same
way they do for @racket[define/query-set].

Allowed within an endpoint installation context.
}

@defform[(define/query-value name absent-expr pattern expr
                               maybe-on-add
                               maybe-on-remove)
         #:grammar
         [(maybe-on-add (code:line)
                        (code:line #:on-add on-add-expr))
          (maybe-on-remove (code:line)
                           (code:line #:on-remove on-remove-expr))]]{
Define a @racket[field] called @racket[name] whose value is based on the
presence of an assertion matching @racket[pattern] in the dataspace. When such
an assertion is present, the value of the @racket[name] field is the result of
evaluating @racket[expr], which may refer to @racket[pattern]. When no such
assertion exists, including initially, the value of @racket[name] is the result
of @racket[absent-expr].

The optional @racket[maybe-on-add] and @racket[maybe-on-expr] behave the same
way they do for @racket[define/query-set].

Allowed within an endpoint installation context.
}

@defform[(define/query-count name pattern key-expr
                               maybe-on-add
                               maybe-on-remove)
         #:grammar
         [(maybe-on-add (code:line)
                        (code:line #:on-add on-add-expr))
          (maybe-on-remove (code:line)
                           (code:line #:on-remove on-remove-expr))]]{
Define a @racket[field] called @racket[name] whose value is a @racket[hash]
counting occurrences of matching assertions in the dataspace. More precisely,
for each assertion @racket[pattern], evaluating @racket[key-expr] determines a
key in the hash; the value for that key is incremented when the assertion
appears and decremented when it disappears. When the count associated with a
particular key falls to @racket[0], that key is removed from the hash.

The optional @racket[maybe-on-add] and @racket[maybe-on-expr] behave the same
way they do for @racket[define/query-set].

Allowed within an endpoint installation context.
}

@subsection{Generalizing Dataflow}

The dataflow mechanism that automatically refreshes @racket[assert] endpoints
when a referenced field changes may be used to react to local state updates in
arbitrary ways using @racket[begin/dataflow].

@defform[(begin/dataflow S ...+)]{
Evaluate and perform the script actions @racket[S ...] during facet startup, and
then again each time a field referenced by the script updates.

Conceptually, @racket[begin/dataflow] may be thought of as an event handler
endpoint in the vein of @racket[on], where the event of interest is @emph{update
of local state}.

Allowed within an endpoint installation context.
}

@defform[(define/dataflow name expr maybe-default)
         #:grammar
         [(maybe-default (code:line)
                         (code:line #:default default-expr))]]{
Define a @racket[field] named @racket[name], whose value is reevaluated to the
result of @racket[expr] each time any referenced field changes.

The value of @racket[name] is either @racket[#f] or, if provided,
@racket[default-expr]. This initial value is observable for a short time during
facet startup.

Note that when a field referenced by @racket[expr] changes, there may be some
time before @racket[name] refreshes, during which "stale" values may be read
from the field.

Allowed within an endpoint installation context.
}


@subsection{Generalizing Actor-Internal Communication}

Talk about internal assertions and messages.

@subsection{Nesting Dataspaces}

Nested dataspaces, inbound and outbound assertions, quit-datapace.

@defform[(dataspace S ...)]{
Spawns a dataspace as a child of the dataspace enclosing the executing actor.
The new dataspace executes each action @racket[S].

Allowed within a script.
}


@section{@hash-lang[] @racket[syndicate] Programs}

In a @hash-lang[] @racket[syndicate] program, the results of top-level
expressions define the initial group of actors in the dataspace. That is,
evaluating @racket[spawn] or @racket[dataspace] in the context of the module
top-level adds that actor specification to the initial dataspace of the program.
For example, a module such as:

@codeblock[#:line-numbers 0]|{
#lang syndicate

(define (spawn-fun)
  (spawn ...))

(spawn ...)

(spawn-fun)
}|

launches a syndicate program with two initial actors, one the result of the
@racket[spawn] expression on line 5 and one the result of evaluating the
@racket[spawn] expresion on line 3 during the course of calling
@racket[spawn-fun] on line 7.

The initial dataspace is referred to as the @emph{ground} dataspace, and it
plays a special role in Syndicate programming; see below.

@; -----------------------------------------------------------------------

@include-section["interactive.scrbl"]
