# Syndicate: A Networked, Concurrent, Functional Programming Language

Syndicate is an actor-based concurrent language able to express
communication, enforce isolation, and manage resources.
Network-inspired extensions to a functional core represent imperative
actions as values, giving side-effects locality and enabling
composition of communicating processes.

Collaborating actors are grouped within task-specific *networks* (a.k.a.
virtual machines) to scope their interactions. Conversations between
actors are multi-party (using a publish/subscribe medium), and actors
can easily participate in many such conversations at once.

Syndicate makes *presence* notifications an integral part of pub/sub
through its *shared dataspaces*, akin to
[tuplespaces](https://en.wikipedia.org/wiki/Tuple_space). Each shared
dataspace doubles as the pub/sub subscription table for its network.
Actors react to *state change notifications* reporting changes in a
dataspace, including new subscriptions created by peers and removal of
subscriptions when a peer exits or crashes. State change notifications
serve to communicate changes in demand for and supply of services,
both within a single network and across nested layers of
networks-within-networks. Programs can give up responsibility for
maintaining shared state and for scoping group communications, letting
their containing network take on those burdens.

## Contents

This repository contains

 - a [Racket](http://racket-lang.org/) implementation of Syndicate
   (plus auxiliary modules) in `racket/syndicate/`

 - an
   [ECMAScript 5](http://www.ecma-international.org/publications/standards/Ecma-262.htm)
   implementation of Syndicate in `js/`

 - larger example programs:

    - `examples/platformer`, a 2D Platform game written in Syndicate
      for Racket.

    - `examples/netstack`, a TCP/IP stack written in Syndicate for
      Racket. It reads and writes raw Ethernet packets from the kernel
      using Linux- and OSX-specific APIs.

 - a sketch of a Haskell implementation of the core routing structures
   of Syndicate in `hs/`

## Copyright

Copyright &copy; Tony Garnock-Jones 2010, 2011, 2012, 2013, 2014, 2015, 2016.
