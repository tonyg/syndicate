#lang scribble/manual

@(require (for-label (only-in racket struct)
                     typed/syndicate/roles)
          (prefix-in racket: (for-label racket))
          (prefix-in untyped: (for-label syndicate/actor)))

@title{Typed Syndicate}


@defmodule[typed/syndicate/roles]

@section{Overview}

@section{Types}

@deftogether[(@defidform[Int]
              @defidform[Bool]
              @defidform[String]
              @defidform[ByteString]
              @defidform[Symbol])]{
Base types.
}

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

@defform*[[(define-type-alias id type)
           (define-type-alias (ty-cons-id arg-id ...) type)]]{
Define @racket[id] to be the same as @racket[type], or create a type constructor
@racket[(ty-cons-id ty ...)] whose meaning is @racket[type] with references to
@racket[arg-id ...] replaced by @racket[ty ...].
}

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

@defform*[#:literals (:)
          [(ann expr : type)
           (ann expr type)]]{
Ensure that @racket[expr] has the given @racket[type].
}

@defform[(if test-expr then-expr else-expr)]{
The same as Racket's @racket[racket:if].
}

@deftogether[(@defform[(cond [test-expr body-expr ...+] ...+)]
              @defthing[else Bool #:value #t])]{
Like Racket's @racket[racket:cond].
}

@defform[(when test-expr expr)]{
Like Racket's @racket[racket:when], but results in @racket[#f] when
@racket[test-expr] is @racket[#f].
}

@defform[(unless test-expr expr)]{
Like Racket's @racket[racket:unless], but results in @racket[#f] when
@racket[test-expr] is @racket[#f].
}

@defform[(let ([id expr] ...) body ...+)]{
The same as Racket's @racket[racket:let].
}

@defform[(let* ([id expr] ...) body ...+)]{
The same as Racket's @racket[racket:let*].
}

@defform[#:literals (:)
         (lambda ([x opt-: type] ...) expr ...+)
         #:grammar
         [(opt-: (code:line)
                 (code:line :))]]{
Constructsa an anonymous function.
}

@defidform[λ]{Synonym for @racket[lambda].}

@defform[(Λ (X ...) expr)]{
Parametric abstraction over type variables @racket[X ...].
}

@defform[(inst expr type ...)]{
Instantiates the type variables @racket[X ...] with @racket[type ...], where
@racket[expr] has type @racket[(∀ (X ...) t)].
}

@defform*[#:literals (: → -> ∀)
          [(define id : type expr)
           (define id expr)
           (define (id [arg-id opt-: arg-type] ... opt-res-ty) expr ...+)
           (define (∀ (X ...) (id [arg-id opt-: arg-type] ... opt-res-ty)) expr ...+)]
          #:grammar
          [(opt-: (code:line) (code:line :))
           (opt-res-ty (code:line)
                       (code:line arr res-type))
           (arr (code:line →) (code:line ->))]]{
Define a constant or a (potentially polymorphic) function. Note that the
function name @racket[id] is @emph{not} bound in the body.
}

@defform[(define-tuple (id ...) expr)]{
Define @racket[id ...] to each of the slots of the tuple produced by
@racket[expr].
}

@defform[(match-define pattern expr)]{
Define the binders of @racket[pattern] to the matching values of @racket[expr].
}

@defform[(begin expr ...+)]{
Sequencing form whose value and type is that of the final @racket[expr].
}

@defform[(block expr ...+)]{
Like @racket[begin], but also introduces a definition context for its body.
}

@defform[(match expr [pattern body-expr ...+] ...+)]{
Like Racket's @racket[racket:match] but with the pattern syntax described by
@racket[on].
}

@defform[(tuple expr ...)]{
Constructs a tuple of arbitrary arity.
}

@defform[(select i expr)]{
Extract the @racket[i]th element of a @racket[tuple].
}

@defthing[unit Unit #:value (tuple)]

@defform[(error format-expr arg-expr ...)]{
Raises an exception using @racket[format-expr] as a format string together with
@racket[arg-expr ...].
}

@deftogether[(
@defthing[+ (→fn Int Int Int)]
@defthing[- (→fn Int Int Int)]
@defthing[* (→fn Int Int Int)]
@defthing[< (→fn Int Int Bool)]
@defthing[> (→fn Int Int Bool)]
@defthing[<= (→fn Int Int Bool)]
@defthing[>= (→fn Int Int Bool)]
@defthing[= (→fn Int Int Bool)]
@defthing[even? (→fn Int Bool)]
@defthing[odd? (→fn Int Bool)]
@defthing[add1 (→fn Int Int)]
@defthing[sub1 (→fn Int Int)]
@defthing[max (→fn Int Int Int)]
@defthing[min (→fn Int Int Int)]
@defthing[zero? (→fn Int Bool)]
@defthing[positive? (→fn Int Bool)]
@defthing[negative? (→fn Int Bool)]
@defthing[current-inexact-milleseconds? (→fn Int)]
@defthing[string=? (→fn String String Bool)]
@defthing[bytes->string/utf-8 (→fn ByteString String)]
@defthing[string->bytes/utf-8 (→fn String ByteString)]
@defthing[gensym (→fn Symbol Symbol)]
@defthing[symbol->string (→fn Symbol String)]
@defthing[string->symbol (→fn String Symbol)]
@defthing[not (→fn Bool Bool)]
@defform[(/ e1 e2)]
@defform[(and e ...)]
@defform[(or e ...)]
@defform[(equal? e1 e2)]
@defform[(displayln e)]
@defform[(printf fmt-expr val-expr ...)]
@defform[(~a e ...)]
)]{
Primitive operations imported from Racket.
}

@defform[#:literals (:)
         (for/fold ([acc-id maybe-:ty acc-expr] ...+)
                   (for-clause ...)
           body-expr ...+)
         #:grammar
         [(maybe-:ty (code:line)
                     (code:line : acc-type))
          (for-clause (code:line [id seq-expr])
                      (code:line [id : type seq-expr])
                      (code:line [(k-id v-id) hash-expr])
                      (code:line #:when test-expr)
                      (code:line #:unless test-expr)
                      (code:line #:break test-expr))]]{
Similar to Racket's @racket[racket:for/fold].

When more than one @racket[acc-id] is used, the body of the loop must evaluate
to a @racket[tuple] with one value for each accumulator, with the final tuple
also being the result of the entire expression.

Each @racket[seq-expr] should be of type @racket[Sequence], though expressions
of type @racket[List] and @racket[Set] are automatically converted.
}

@deftogether[(
@defform[(for/list (for-clause ...) body ...+)]
@defform[(for/set (for-clause ...) body ...+)]
@defform[(for/sum (for-clause ...) body ...+)]
@defform[(for (for-clause ...) body ...+)]
@defform[(for/first (for-clause ...) body ...+)]
)]{
Like their Racket counterparts. See @racket[for/fold] for the description of
@racket[for-clause].

Unlike @racket[racket:for/first], @racket[for/first] returns a @racket[Maybe]
value to indicate success/failure.
}

@section{Require & Provide}

@defform[(struct-out ctor-id)]{
}

@subsection{Requiring From Outside Typed Syndicate}

@defform[#:literals (:)
         (require/typed lib clause ...)
         #:grammar
         [(clause (code:line [id : type])
                  (code:line opaque))
          (opaque (code:line [#:opaque type-name])
                  (code:line [#:opaque type-name #:arity op arity-nat]))
          (opaque (code:line =) (code:line >) (code:line >=))]]{
Import and assign types to bindings from outside Typed Syndicate.

Note that @emph{unlike} Typed Racket, Typed Syndicate does not attach contracts
to imported bindings.

An @racket[#:opaque] declaration defines a type @racket[type-name] (or, in the
@racket[#:arity] case, a type constructor) that may be used to describe imports
but otherwise has no other operations.
}

@defform[(require-struct ctor-id #:as ty-ctor-id #:from lib maybe-omit-accs)
         #:grammar
         [(maybe-omit-accs (code:line)
                           (code:line #:omit-accs))]]{
Import a Racket @racket[struct] named @racket[ctor-id] and create a type
constructor @racket[ty-ctor-id] for its instances.

Unless @racket[#:omit-accs] is specified, defines the accessor functions for the
struct.
}


@section{Builtin Data Structures}

@deftogether[(@defstruct[observe ([claim any?]) #:omit-constructor]
              @defform[(Observe type)])]{
Constructs an assertion of interest.
}

@deftogether[(@defstruct[inbound ([assertion any?]) #:omit-constructor]
              @defform[(Inbound type)])]{
Constructor for an assertion inbound from an outer dataspace.
}

@deftogether[(@defstruct[outbound ([assertion any?]) #:omit-constructor]
              @defform[(Outbound type)])]{
Constructor for an assertion outbound to an outer dataspace.
}

@deftogether[(@defstruct[message ([body any?]) #:omit-constructor]
              @defform[(Message type)])]{
Constructor for a broadcast message.
}

@subsection{Lists}

@defform[(List type)]{
The type for @racket[cons] lists whose elements are of type @racket[type].
}

@deftogether[(
@defthing[empty (List ⊥)]
@defthing[empty? (∀ (X) (→fn (List X) Bool))]
@defthing[cons (∀ (X) (→fn X (List X) (List X)))]
@defthing[cons? (∀ (X) (→fn X (List X) Bool))]
@defthing[first (∀ (X) (→fn (List X) X))]
@defthing[second (∀ (X) (→fn (List X) X))]
@defthing[rest (∀ (X) (→fn (List X) (List X)))]
@defthing[member? (∀ (X) (→fn X (List X) Bool))]
@defthing[reverse (∀ (X) (→fn (List X) (List X)))]
@defthing[partition (∀ (X) (→fn (List X) (→fn X Bool) (List X)))]
@defthing[map (∀ (X Y) (→fn (→fn X Y) (List X) (List Y)))]
@defthing[argmax (∀ (X) (→fn (→fn X Int) (List X) X))]
@defthing[argmin (∀ (X) (→fn (→fn X Int) (List X) X))]
@defthing[remove (∀ (X) (→fn X (List X) (List X)))]
@defthing[length (∀ (X) (→fn (List X) Int))]
@defform[(list e ...)]
)]{
Like their Racket counterparts.
}

@subsection{Sets}

@defform[(Set type)]{
The type for sets whose elements are of type @racket[type].
}

@deftogether[(
@defform[(set e ...)]
@defform[(set-union st ...+)]
@defform[(set-intersect st ...+)]
@defform[(set-subtract st ...+)]
@defthing[set-first (∀ (X) (→fn (Set X) X))]
@defthing[set-empty? (∀ (X) (→fn (Set X) Bool))]
@defthing[set-count (∀ (X) (→fn (Set X) Int))]
@defthing[set-add (∀ (X) (→fn (Set X) X (Set X)))]
@defthing[set-remove (∀ (X) (→fn (Set X) X (Set X)))]
@defthing[set-member? (∀ (X) (→fn (Set X) X Bool))]
@defthing[list->set (∀ (X) (→fn (List X) (Set X)))]
@defthing[set->list (∀ (X) (→fn (Set X) (List X)))]
)]{
Like their Racket counterparts.
}

@subsection{Hashes}

@defform[(Hash key-type value-type)]{
The type for key/value hash tables.
}

@deftogether[(
@defform[(hash key val ... ...)]
@defthing[hash-set (∀ (K V) (→fn (Hash K V) K V (Hash K V)))]
@defthing[hash-ref (∀ (K V) (→fn (Hash K V) K V))]
@defthing[hash-ref/failure (∀ (K V) (→fn (Hash K V) K V V))]
@defthing[hash-empty? (∀ (K V) (→fn (Hash K V) Bool))]
@defthing[hash-has-key? (∀ (K V) (→fn (Hash K V) K Bool))]
@defthing[hash-count (∀ (K V) (→fn (Hash K V) Int))]
@defthing[hash-update (∀ (K V) (→fn (Hash K V) K (→fn V V) (Hash K V)))]
@defthing[hash-update/failure (∀ (K V) (→fn (Hash K V) K (→fn V V) V (Hash K V)))]
@defthing[hash-remove (∀ (K V) (→fn (Hash K V) K (Hash K V)))]
@defthing[hash-map (∀ (K V R) (→fn (Hash K V) (→fn K V R) (List R)))]
@defthing[hash-keys (∀ (K V) (→fn (Hash K V) (List K)))]
@defthing[hash-values (∀ (K V) (→fn (Hash K V) (List V)))]
@defthing[hash-union (∀ (K1 V1 K2 V2) (→fn (Hash K1 V1) (Hash K2 V2) (Hash (U K1 K2) (U V1 V2))))]
@defthing[hash-union/combine (∀ (K V) (→fn (Hash K V) (Hash K V) (→fn V V V) (Hash K V)))]
@defthing[hash-keys-subset? (∀ (K1 V1 K2 V2) (→fn (Hash K1 V1) (Hash K2 V2) Bool))]
)]{
Like their Racket counterparts. The /failure and /combine suffixes are in place
of keyword arguments, which Typed Syndicate does not presently support.
}

@subsection{Sequences}

@defform[(Sequence type)]{
The type for a sequence of @racket[type] values.
}

@deftogether[(
@defthing[empty-sequence (Sequence ⊥)]
@defthing[sequence->list (∀ (X) (→fn (Sequence X) (List X)))]
@defthing[sequence-length (∀ (X) (→fn (Sequence X) Int))]
@defthing[sequence-ref (∀ (X) (→fn (Sequence X) Int Int))]
@defthing[sequence-tail (∀ (X) (→fn (Sequence X) Int (Sequence X)))]
@defthing[sequence-append (∀ (X) (→fn (Sequence X) (Sequence X) (Sequence X)))]
@defthing[sequence-map (∀ (A B) (→fn (→fn A B) (Sequence A) (Sequence B)))]
@defthing[sequence-andmap (∀ (X) (→fn (→fn X Bool) (Sequence X) Bool))]
@defthing[sequence-ormap (∀ (X) (→fn (→fn X Bool) (Sequence X) Bool))]
@defthing[sequence-fold (∀ (A B) (→fn (→fn A B A) (Sequence B) A))]
@defthing[sequence-count (∀ (X) (→fn (→fn X Bool) (Sequence X) Int))]
@defthing[sequence-filter (∀ (X) (→fn (→fn X Bool) (Sequence X) (Sequence X)))]
@defthing[sequence-add-between (∀ (X) (→fn (Sequence X) X (Sequence X)))]
@defthing[in-list (∀ (X) (→fn (List X) (Sequence X)))]
@defthing[in-hash-keys (∀ (K V) (→fn (Hash K V) (Sequence K)))]
@defthing[in-hash-values (∀ (K V) (→fn (Hash K V) (Sequence V)))]
@defthing[in-range (→fn Int (Sequence Int))]
@defthing[in-set (∀ (X) (→fn (Set X) (Sequence X)))]
)]{
Like their Racket counterparts.
}

@subsection{Maybe}

@deftogether[(
@defidform[None]
@defthing[none None]
@defstruct[some ([v any?]) #:omit-constructor]
@defform[(Some type)]
@defform[(Maybe type)]
)]{
@racket[(Maybe type)] is an alias for @racket[(U None (Some type))].
}

@subsection{Either}

@deftogether[(
@defstruct[left ([v any?]) #:omit-constructor]
@defform[(Left type)]
@defstruct[right ([v any?]) #:omit-constructor]
@defform[(Right type)]
@defform[(Either left-type right-type)]
)]{
@racket[(Either left-type right-type)] is an alias for @racket[(U (Left
left-type) (Right right-type))].
}

@defthing[partition/either (∀ (X Y Z) (→fn (List X) (→fn X (Either Y Z)) (Tuple (List Y) (List Z))))]{
Partition a list based on a function that returns an @racket[Either] value.
}

@section{Behavioral Checking}
