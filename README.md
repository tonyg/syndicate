# Prospect: A Networked, Concurrent, Functional Programming Language

Prospect is an actor-based concurrent language able to express
communication, enforce isolation, and manage resources.
Network-inspired extensions to a functional core represent imperative
actions as values, giving side-effects locality and enabling
composition of communicating processes.

Collaborating actors are grouped within task-specific *networks* (a.k.a.
virtual machines) to scope their interactions. Conversations between
actors are multi-party (using a publish/subscribe medium), and actors
can easily participate in many such conversations at once.

Prospect makes *presence* notifications an integral part of pub/sub
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

## The code

This repository contains a [Racket](http://racket-lang.org/) package,
`prospect`, which includes

 - the implementation of the `#lang prospect` language, in the
   [`prospect` directory](https://github.com/tonyg/prospect/tree/master/prospect/).

 - a TCP echo server example, which listens for connections on port
   5999 by default, in
   [`prospect/examples/echo.rkt`](https://github.com/tonyg/prospect/tree/master/prospect/examples/echo.rkt).
   Connect to it using, for example, `telnet localhost 5999`.

 - a handful of other examples, in
   [`prospect/examples/`](https://github.com/tonyg/prospect/tree/master/prospect/examples/).

## Compiling and running the code

You will need Racket version 6.3 or later.

Once you have Racket installed, run

    raco pkg install prospect

to install the package from the Racket package repository, or

    raco pkg install

from the root directory of the Git checkout to install the package
from a local snapshot. (Alternatively, `make link` does the same thing.)
This will make `#lang prospect` available to programs.

At this point, you may load and run any of the example `*.rkt` files
in the
[`prospect/examples/`](https://github.com/tonyg/prospect/tree/master/prospect/examples/)
directory.

## Copyright

Copyright &copy; Tony Garnock-Jones 2010, 2011, 2012, 2013, 2014, 2015, 2016.
