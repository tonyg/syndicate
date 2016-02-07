# Syndicate-JS: Syndicate for Javascript environments

## A walk through the code

Source files in `src/`, from most general to most specific:

 - `reflect.js`: Reflection on function formal parameter lists.
 - `util.js`: Functions `extend` and `kwApply`.
 - `randomid.js`: Generation of (cryptographically) random base64 strings.

 - `route.js`: Implementation of dataspace trie structure.
 - `patch.js`: Implementation of patches over dataspace tries.
 - `mux.js`: Use of tries plus patches to build a (de)multiplexing routing structure.
 - `network.js`: Implementation of core leaf actors and networks.
 - `ground.js`: Pseudo-network acting as the global outermost context for Syndicate actors.

 - `ack.js`: Utility for detecting when a previous state change has taken effect.
 - `seal.js`: Immutable container for data, used to hide structure from dataspace tries.

 - `demand-matcher.js`: Tracking and responding to demand and supply expressed as assertions.
 - `dom-driver.js`: Syndicate driver for displaying DOM fragments on a webpage.
 - `jquery-driver.js`: Syndicate driver for soliciting jQuery-based DOM events.

 - `main.js`: Main package entry point.
