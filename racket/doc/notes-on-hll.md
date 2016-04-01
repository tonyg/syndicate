## Syntax

Just a sketch, at the moment.

    Instantaneous actions, I := (actor I ...)
                                (network I ...)
                                (state [B O ...] [E I ...] ...)
                                (background I ...)
                                (assert! P)
                                (retract! P)
                                (send! P)
                                (flush!)              ;; ???
                                expr
        Optional Bindings, B := ;; nothing, or
                                #:collect [(id I) ...]
          Ongoing actions, O := (on E I ...)
                                (during P O ...)
                                (once E I ...)        ;; ???
                                (assert P)
                                (assert #:when Pred P)
                                (track [x Agg] I ...)
                                (begin O ...)         ;; ??? begin isn't quite right
            Predicates, Pred := (not Pred)        ;; -- NOT YET IMPLEMENTED
                                (exists P Pred)   ;; -- NOT YET IMPLEMENTED
                                (forall P Pred)   ;; -- NOT YET IMPLEMENTED
                                expr
                   Events, E := (asserted P)
                                (retracted P)
                                (message P)
                                (rising-edge Pred)
                                (falling-edge Pred)
             Aggregates, Agg := (count expr P)
                                (set expr P)
                                (hash k_expr v_expr P)
                                (project P)
                                (single-value expr P)
                                (single-value expr P #:default def_expr)
                                Pred
                 Patterns, P := ... ;; uses $var as binder

    (define-syntax-rule (until B E O ...)
      (state [B O ...] [E (values)]))

    (define-syntax-rule (forever B O ...)
      (state [B O ...]))

Note also that `falling-edge` is encodable using `rising-edge` and
`not`, and that `forall` is encodable using `exists` and `not`.

`state` has the `B` bindings visible in the `I`s, and returns the
value(s) of the final `I` from the `E` exit branch that was chosen
*prepended* to the values of the calling actor's variables at the time
of `state` termination.

There are four kinds of actor-spawning `I`s: `actor`, `network`,
`state` and `background`. Neither `actor`, `network` nor `background`
yield a value; only `state` does so. However, both `state` and
`background` link to their invoking contexts, because `state` may
return values or crash, and `background` may crash. Actors using
`state` and `background` must therefore have a special continuation
table in their private state to link them to their `state` and
`background` logical-children-but-physical-siblings. The link
communicates values (on success) and crashes (on failure).

Q: Should exception values be transmitted on `state` failure? I think
no, but I am not sure there's a good reason why not.

Of the events, `asserted`, `retracted` and `message` require no
private-state, since the network does the book-keeping for us.
`rising-edge`, however, needs to track the state of its predicate. If
the predicate happens to involve an `exists`, then an assertion set
must be maintained, like for a `track`.

This is leading me to believe that the order of operations is:

 - Given a patch,
   - update `track`s and assertion-sets related to `rising-edge`.
   - handle `on` for `asserted`, `retracted` and `rising-edge`, in order of appearance
   - check termination conditions
   - maintain `assert`s and subscriptions for `on`s
 - Given a message,
   - handle `on` for `message` and `rising-edge`, in order of appearance
   - check termination conditions
   - maintain `assert`s and subscriptions for `on`s

Actually, I'm not sure `falling-edge` is encodable using
`rising-edge`, since the initial state might be different. Do we
assume that the level is high when the level is unknown for a
falling-edge? I think it likely, given I think it likely that we
assume the level is low when the level is unknown for a rising-edge.

`when` is a bit tricky, because it should clearly apply to `on` and
`assert`, but it is not clear that it should apply to `track`.
Furthermore, what should happen if, mid-way through some interaction
involving assertions, the `when` predicate goes false? Should the
subscription be retracted? On balance, I'm starting to think that
`when` is a bad idea. In my sketches so far, there's only one place
it's used, to conditionally `assert` a set, so I've added `#:when` to
`assert` instead.

Note that `exists` (and so `forall`) are tricky because of the nested
`Pred`. For now, I'm not implementing them -- we'll see how painful it
is to use `track` and plain-old `expr` `Pred`s instead.

`during` is a quasi-macro, with defining equation

    (during P O ...) === (on (asserted P)
                             (until (retracted P')
                                    O ...))

where `P'` is like `P` but with all the binders in `P` instantiated
with their values at the time the `until` is started.

## Examples

```racket
#lang syndicate/actor
;; Simple mutable box and count-to-infinity box client.

(struct set-box (new-value) #:transparent)
(struct box-state (value) #:transparent)

(actor (forever #:collect [(value 0)]
         (assert (box-state value))
         (on (message (set-box $new-value))
           new-value)))

(actor (forever
         (on (asserted (box-state $value))
           (send! (set-box (+ value 1))))))
```
