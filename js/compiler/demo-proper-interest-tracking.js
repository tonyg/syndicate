// Illustrates the response of asserted / retracted / during to
// observation of assertions discarding some of their dimensions.
//
// Should eventually be turned into some kind of test case.

var Syndicate = require('./src/main.js');

var Dataspace = Syndicate.Dataspace;
var Patch = Syndicate.Patch;

var __ = Syndicate.__;

assertion type ready(what);
assertion type entry(key, val);

ground dataspace {
  actor named 'listener' {
    assert ready('listener');
    on asserted entry($key, _) {
      console.log('key asserted', key);
      react {
        on asserted  entry(key, $value) { console.log('binding', key, '--->', value); }
        on retracted entry(key, $value) { console.log('binding', key, '-/->', value); }
        stop on retracted entry(key, _) {
          console.log('key retracted', key);
        }
      }
    }
  }

  actor named 'other-listener' {
    assert ready('other-listener');
    during entry($key, _) {
      on start { console.log('(other-listener) key asserted', key); }
      during entry(key, $value) {
        on start { console.log('(other-listener) binding', key, '--->', value); }
        on stop  { console.log('(other-listener) binding', key, '-/->', value); }
      }
      on stop { console.log('(other-listener) key retracted', key); }
    }
  }

  function pause(k) {
    console.log('pause');
    react {
      assert ready('pause');
      on asserted ready('pause') {
        return k();
      }
    }
  }

  actor named 'driver' {
    stop on asserted ready('listener') {
      react {
        stop on asserted ready('other-listener') {
          Dataspace.stateChange(Patch.assert(entry('a', 1)));
          Dataspace.stateChange(Patch.assert(entry('a', 2)));
          Dataspace.stateChange(Patch.assert(entry('b', 3)));
          Dataspace.stateChange(Patch.assert(entry('c', 33)));
          Dataspace.stateChange(Patch.assert(entry('a', 4)));
          Dataspace.stateChange(Patch.assert(entry('a', 5)));
          pause(function () {
            Dataspace.stateChange(Patch.retract(entry('a', 2)));
            Dataspace.stateChange(Patch.retract(entry('c', 33)));
            Dataspace.stateChange(Patch.assert(entry('a', 9)));
            pause(function () {
              Dataspace.stateChange(Patch.retract(entry('a', __)));
              pause(function () {
                console.log('done');
              });
            });
          });
        }
      }
    }
  }
}
