#lang scribble/manual

@(require (for-label (only-in racket struct #;equal?)
                     typed/syndicate/roles)
           (prefix-in untyped: (for-label syndicate/actor)))

@title{Typed Syndicate}


@defmodule[typed/syndicate/roles]

@section{Overview}

@section{Types}

@defform[(U type ...)]{
The type representing the union of @racket[type ...].
}

@defidform[⊥]{
An alias for @racket[(U)].
}

@defidform[★/t]{
The type representing any possible assertion, and, in an @racket[AssertionSet],
the possibility for an infinite set of assertions.
}

@defidform[Discard]{
The type of @racket[_] patterns.
}

@defform[(Bind type)]{
The type of @racket[$] patterns.
}

@defidform[FacetName]{
The type associated with identifiers bound by @racket[start-facet].
}

@defform[(Role (x) type ...)]{
The type of a facet named @racket[x] and endpoints described by @racket[type
...].
}

@defform[(Stop X type ...)]{
The type of a @racket[stop] action.
}

@defform[(Field type)]{
The type of a field containing values of @racket[type].
}


@defform[(Shares type)]{
The type of an @racket[assert] endpoint.
}

@defform[#:literals (OnStart OnStop Asserted Retracted)
         (Reacts EventDesc type ...)
         #:grammar
         [(EventDesc (code:line OnStart)
                     (code:line OnStart)
                     (code:line (Asserted event-type))
                     (code:line (Retracted event-type)))]]{
The type of a @racket[on] endpoint that reacts to events described by
@racket[EventDesc] with the behavior given by @racket[type ...].
}

@deftogether[(@defidform[OnStart]
              @defidform[OnStop]
              @defform[(Asserted type)]
              @defform[(Retracted type)])]{
See @racket[Reacts].
}

@defform[(Actor type)]{
The type of an actor that operates in a dataspace with a certain communication
@racket[type].
}

@defform[(ActorWithRole comm-type behavior-type)]{
An @racket[Actor] type with the additional @racket[behavior-type] describing the
actor's behavior in terms of a @racket[Role].
}

@defform[(Sends type)]{
The type of a @racket[send!] action.
}

@defform[(Realize type)]{
The type of a @racket[realize!] action.
}

@deftogether[(@defform[(Branch type ...)]
              @defform[(Effs type ...)])]{
Types that may arise in descriptions in @racket[Role] types due to branching and
sequencing.
}

@defform[(Tuple type ...)]{
The type of @racket[tuple] expressions.
}

@defidform[Unit]{
An alias for @racket[(Tuple)].
}

@defform[(AssertionSet type)]{
The type for a set of assertions of a certain @racket[type]. Note that these are
not interoperable with the general purpose @racket[set] data structures.
}

@defform[(∀ (X ...) type)]{
Universal quantification over types.
}

@defform[#:literals (Computation Value Endpoints Roles Spawns)
         (→ type ... (Computation (Value result-type)
                                  (Endpoints ep-type ...)
                                  (Roles role-type ...)
                                  (Spawns spawn-type ...)))]{
The type of a function with parameters @racket[type ...] that returns @racket[result-type]. The type includes the effects of any actions performed by the function:
@itemlist[
  @item{@racket[Endpoints]: includes any endpoint installation effects, such as from @racket[assert] and @racket[on].}
  @item{@racket[Roles]: includes any script action effects, such as from @racket[start-facet], @racket[stop], and @racket[send!].}
  @item{@racket[Spawns]: includes descriptions of any @racket[spawn] actions.}
]
}

@defform[(→fn type-in ... type-out)]{
Shorthand for a @racket[→] type with no effects.
}

@defform[(proc maybe-quantifiers type-in ... maybe-arrow type-out
               maybe-endpoints
               maybe-roles
               maybe-spawns)
         #:grammar
         [(maybe-quantifiers (code:line)
                             (code:line #:forall (X ...)))
          (maybe-arrow (code:line)
                       (code:line →)
                       (code:line ->))
          (maybe-endpoints (code:line)
                           (code:line #:endpoints (e ...)))
          (maybe-roles (code:line)
                       (code:line #:roles (r ...)))
          (maybe-spawns (code:line)
                        (code:line #:spawns (s ...)))]]{
A more convenient notation for writing (potentially polymorphic) function types
with effects. Shorthand for @racket[(∀ (X ...) (→ type-in ... (Computation
(Value type-out) (Endpoints e ...) (Roles r ...) (Spawns s ...))))].
}

@deftogether[(@defform[(Computation type ...)]
              @defform[(Value type)]
              @defform[(Endpoints type)]
              @defform[(Roles type)]
              @defform[(Spawns type)])]{
See @racket[→].
}

@section{User Defined Types}

@defform[(define-constructor (ctor-id slot-id ...)
               maybe-type-ctor
               maybe-alias ...)
         #:grammar
         [(maybe-type-ctor (code:line)
                           (code:line #:type-constructor type-ctor-id))
          (maybe-alias (code:line)
                       (code:line #:with alias alias-body))]]{
Defines a container analagous to a prefab @racket[struct]. Includes accessor
functions for each @racket[slot-id]. (But not, presently, a predicate function).

When a @racket[type-ctor-id] is provided, the type of such structures is
@racket[(type-ctor-id type ...)], where each @racket[type] describes the value
of the corresponding slot. When not provided, the type constructor is named by
appending @racket["/t"] to @racket[ctor-id].

Each @racket[alias] and @racket[alias-body] creates an instance of
@racket[define-type-alias].
}

@defform[#:literals (:)
         (define-constructor* (ctor-id : type-ctor-id slot-id ...)
               maybe-alias ...)]{
An abbreviated form of @racket[define-constructor].
}

@defform[#:literals (:)
         (assertion-struct ctor-id : type-ctor-id (slot-id ...))]{
An abbreviated form of @racket[define-constructor].
}

@defform[#:literals (:)
         (message-struct ctor-id : type-ctor-id (slot-id ...))]{
An abbreviated form of @racket[define-constructor].
}

@section{Actor Forms}

@defform[(run-ground-dataspace type expr ...)]{
Starts a ground, i.e. main, dataspace of the program, with the given
communication @racket[type] and initial actors spawned by @racket[expr ...].
}

@defform[(spawn maybe-type s)
         #:grammar
         [(maybe-type (code:line)
                      (code:line type))]]{
Spawns an actor with behavior given by @racket[s]. The @racket[type] gives the
communication type of the enclosing dataspace. When absent, @racket[type] is
supplied by the nearest lexically enclosing @racket[spawn] or @racket[dataspace]
form, if any exist.
}

@defform[(dataspace type expr ...)]{
Spawns a dataspace with communication type @racket[type] as a child of the
dataspace enclosing the executing actor. The script @racket[expr ...] spawns the
initial actors of the new dataspace.
}

@defform[(start-facet id maybe-spec expr ...+)
         #:grammar
         [(maybe-spec (code:line)
                      (code:line #:implements type)
                      (code:line #:includes-behavior type))]]{
Start a facet with name @racket[id] and endpoints installed through the
evaluation of @racket[expr ...].
}

@defform[(stop id expr ...)]{
Terminate the facet @racket[id] with continuation script @racket[expr ...]. Any
facets started by the continuation script survive the termination of facet
@racket[id].
}

@defform[#:literals (start stop message asserted retracted _ $)
         (on event-description body ...+)
         #:grammar
         [(event-description (code:line start)
                             (code:line stop)
                             (code:line (message pattern))
                             (code:line (asserted pattern))
                             (code:line (retracted pattern)))
          (pattern (code:line _)
                   (code:line ($ id type))
                   (code:line ($ id))
                   (code:line $id)
                   (code:line $id:type)
                   (code:line (ctor pattern ...))
                   (code:line expr))]]{
Creates an event handler endpoint that responds to the event specified by
@racket[event-description]. Executes the @racket[body ...] for each matching
event, with any pattern variables bound to their matched value.

Patterns have the following meanings:
@itemlist[
  @item{@racket[_] matches anything.}

  @item{@racket[($ id type)] matches any value and binds it to @racket[id] with
  assumed type @racket[type].}

  @item{@racket[($ id)] is like @racket[($ id type)], but attempts to use the
  current communication type to fill in the @racket[type] of potential matches.
  May raise an error if no suitable communication type is in scope.}

  @item{@racket[(? pred pattern)] matches values where @racket[(pred val)] is not
     @racket[#f] and that match @racket[pattern].}

  @item{@racket[$id:type] is shorthand for @racket[($ id type)].}

  @item{@racket[$id] is shorthand for @racket[($ id)].}

  @item{@racket[(ctor pat ...)] matches values built by applying the constructor
    @racket[ctor] to values matching @racket[pat ...]. @racket[ctor] is usually
    a @racket[struct] name.}

  @item{@racket[expr] patterns match values that are @racket[equal?] to
    @racket[expr].}
]
}

@defform[(on-start expr ...+)]{
Shorthand for @racket[(on start expr ...)].
}

@defform[(on-stop expr ...+)]{
Shorthand for @racket[(on stop expr ...)].
}

@defform[(assert expr)]{
Creates an assertion endpoint with the value of @racket[expr].
}

@defform[(know expr)]{
Creates an internal assertion endpoint with the value of @racket[expr].
}

@defform[(send! expr)]{
Broadcast a dataspace message with the value of @racket[expr].
}

@defform[(realize! expr)]{
Broadcast an actor-internal message with the value of @racket[expr].
}

@defform[#:literals (:)
         (field [id maybe-type expr] ...)
         #:grammar
         [(maybe-type (code:line)
                      (code:line type)
                      (code:line : type))]]{
Defines fields of type @racket[type] with names @racket[id] and initial values
@racket[expr]. If @racket[type] is not provided, the type of the initial
expression is used as the type of the field.
}

@defform[(ref id)]{
Reference the @racket[field] named @racket[id].
}

@defform[(set! id expr)]{
Update the value  the @racket[field] named @racket[id].
}

@defform[(begin/dataflow expr ...+)]{
Evaluate and perform the script @racket[expr ...], and then again each time a
field referenced by the script updates.
}

@defform[(during pattern expr ...+)]{
Engage in behavior for the duration of a matching assertion. The syntax of
@racket[pattern] is the same as described by @racket[on].
}

@defform[(during/spawn pattern expr ...+)]{
Like @racket[during], but spawns an actor for the behavior @racket[expr ...].
}

@defform[(define/query-value name absent-expr pattern expr
                             maybe-on-add
                             maybe-on-remove)
         #:grammar
         [(maybe-on-add (code:line)
                        (code:line #:on-add on-add-expr))
          (maybe-on-remove (code:line)
                           (code:line #:on-remove on-remove-expr))]]{
Equivalent to the untyped @racket[untyped:define/query-value].
}

@defform[(define/query-set name pattern expr
                           maybe-on-add
                           maybe-on-remove)
         #:grammar
         [(maybe-on-add (code:line)
                        (code:line #:on-add on-add-expr))
          (maybe-on-remove (code:line)
                           (code:line #:on-remove on-remove-expr))]]{
Equivalent to the untyped @racket[untyped:define/query-set].
}

@defform[(define/query-hash name pattern key-expr value-expr
                            maybe-on-add
                            maybe-on-remove)
         #:grammar
         [(maybe-on-add (code:line)
                        (code:line #:on-add on-add-expr))
          (maybe-on-remove (code:line)
                           (code:line #:on-remove on-remove-expr))]]{
Equivalent to the untyped @racket[untyped:define/query-hash].
}

@defform[(define/dataflow name maybe-type expr)
         #:grammar
         [(maybe-type (code:line)
                      (code:line type))]]{
Define a @racket[field] named @racket[name], whose value is reevaluated to the
result of @racket[expr] each time any referenced field changes. When
@racket[type] is not supplied, the field has the type of the given
@racket[expr].
}

@section{Expressions}

@defform[(tuple expr ...)]{
Constructs a tuple of arbitrary arity.
}

@section{Requiring From Outside Typed Syndicate}

@section{Behavioral Checking}
