#lang scribble/manual

@(require (for-label (except-in racket process)
                     (except-in racket/gui process)
                     syndicate
                     syndicate-gl/2d
                     (only-in 2htdp/image image?)
                     (only-in pict pict?)))

@title{2d Graphics with syndicate-gl}

@defmodule[syndicate-gl/2d]

@section{Creating Dataspaces}

@defproc[((2d-dataspace [#:width width number? 640]
                        [#:height height number? 480]
                        [#:exit? exit? boolean? #t])
          [boot-actions action?] ...)
         any/c]{
Create and run a @racket[frame%] with dimensions @racket[width] and
@racket[height] controlled by a Syndicate dataspace. The dataspace is
initialized with the given @racket[boot-actions]. Runs (blocking)
while the @racket[frame%] is active. If @racket[exit?] is true, calls
@racket[(exit 0)] once the dataspace exits.
}

@defproc[(spawn-keyboard-integrator
          [#:meta-level meta-level natural-number/c 1])
         actor?]{
KeyboardIntegrator. Integrates key-events into key-pressed assertions. The
@racket[meta-level] must point to the root of the 2d dataspace, which defaults
to assuming is one level removed.
}

@section{Changing the State}

@defproc[(update-scene [prelude (listof (sealof (listof instruction)))]
                       [postlude (listof (sealof (listof instruction)))]
                       [#:meta-level meta-level natural-number/c 1])
         patch?]{
Create a patch retracting any previous scene assertion and asserting a new scene
from the @racket[prelude] and @racket[postlude] instructions. The
@racket[meta-level] must point to the root of the 2d dataspace, which defaults
to assuming is one level removed.
}

@defproc[(update-sprites [#:meta-level meta-level natural-number/c 1]
                         [sprites sprite?] ...)
         patch?]{
Create a patch retracting any previous sprite assertion(s) and asserting each of
@racket[sprites]. The @racket[meta-level] must point to the root of the 2d
dataspace, which defaults to assuming is one level removed.
}

@section{Data Structures}

@defstruct*[window ([width number?] [height number?])]{
Shared state maintained by dataspace. Describes current window dimensions.
}

@defstruct*[frame-event ([counter any/c]
                         [timestamp any/c]
                         [elapsed-ms any/c]
                         [target-frame-rate any/c])]{
Message sent by dataspace. Describes frame about to be rendered.
}

@defstruct*[key-event ([code (or/c char? key-code-symbol?)]
                       [press? boolean?]
                       [key (sealof (instanceof/c key-event%))])]{
Message sent by dataspace. Describes a key event. @racket[key] is a sealed
@racket[key-event%]. @racket[press?] is @racket[#t] when the key is pressed (or
autorepeated!), and @racket[#f] when it is released.
}

@defstruct*[key-pressed ([code (or/c char? key-code-symbol?)])]{
Assertion. Indicates that the named key is held down. See role
KeyboardIntegrator and @racket[spawn-keyboard-integrator].
}

@defstruct*[scene ([prelude (sealof (listof instruction))]
                   [postlude (sealof (listof instruction))])]{
Shared state maintained by program. @racket[prelude] and @racket[postlude] are
to be sealed instruction lists. It is an error to have more than exactly
one active such record at a given time.
}

@defstruct*[sprite ([z number?]
                    [instructions (sealof (listof instruction))])
                   #:omit-constructor]{
Shared state maintained by program. @racket[z] is to be a number, negative
toward camera. @racket[instructions] to be a sealed instruction list.
}

@defproc[(make-sprite [z number?]
                      [instructions (listof any/c)])
         sprite?]{
Create a sprite from a z number and a list of instructions.
}

@defstruct*[request-gc ()]{
Message. Requests that the OpenGL loop perform a major
garbage-collection while *pausing the simulation's real-time
correspondence*. This lets a GC take place without such severe
simulation glitches as happen when doing it in-world.
}

@defproc[(simple-sprite [z number?]
                        [x number?]
                        [y number?]
                        [w number?]
                        [h number?]
                        [i (or/c (instanceof/c bitmap%)
                                 pict?
                                 image?)])
         sprite?]{
Create a sprite from image @racket[i] at position (@racket[x], @racket[y]) with
width @racket[w], height @racket[h], and z-index @racket[z].
}

@section{Instructions}

@racket['(rotate degrees)] where @racket[degrees] is a @racket[number?]

@racket['(scale x y)] where @racket[x] and @racket[y] are both
@racket[number?]-s

@racket['(translate x y)] where @racket[x] and @racket[y] are both
@racket[number?]-s

@racket['(color r g b a)] where @racket[r], @racket[g], @racket[b], and
@racket[a] are @racket[color-number?]-s

@racket['(texture i)] where @racket[i] is a
@racket[(or/c (instanceof/c bitmap%) pict? image?)]

@racket['(push-matrix instr ...)] where each @racket[instr] is an instruction

@racket['(begin instr ...)] where each @racket[instr] is an instruction
