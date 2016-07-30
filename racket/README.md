# Racket implementation of Syndicate

## The language itself

This repository contains a [Racket](http://racket-lang.org/)
implementation of Syndicate in `syndicate`, which includes

 - the implementation of the `#lang syndicate` language, in the
   [`syndicate` directory](https://github.com/tonyg/syndicate/tree/master/racket/syndicate/).

 - a TCP echo server example, which listens for connections on port
   5999 by default, in
   [`syndicate/examples/echo.rkt`](https://github.com/tonyg/syndicate/tree/master/racket/syndicate/examples/echo.rkt).
   Connect to it using, for example, `telnet localhost 5999`.

 - a handful of other examples, in
   [`syndicate/examples/`](https://github.com/tonyg/syndicate/tree/master/racket/syndicate/examples/).

## Auxiliary collects

 - `syndicate-gl` is a Syndicate interface to 2D OpenGL based graphics

## Compiling and running the code

You will need Racket version 6.4.0.14 or later.

Once you have Racket installed, run

    raco pkg install syndicate

to install the package from the Racket package repository, or

    raco pkg install

from the `racket/` directory of the Git checkout to install the package
from a local snapshot. (Alternatively, `make link` does the same thing.)
This will make `#lang syndicate` available to programs.

At this point, you may load and run any of the example `*.rkt` files
in the
[`syndicate/examples/`](https://github.com/tonyg/syndicate/tree/master/syndicate/examples/)
directory.

## Copyright

Copyright &copy; Tony Garnock-Jones 2010, 2011, 2012, 2013, 2014, 2015, 2016.
