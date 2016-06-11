// Illustrates a (now fixed) bug where mutation altering a
// subscription caused the `retracted` half of a during instance to be
// lost.
//
// Symptomatic output:
// x= 123 v= 999
// x= 124 v= 999
//
// Correct output:
// x= 123 v= 999
// finally for x= 124 v= 999
// x= 124 v= 999
//
// Should eventually be turned into some kind of test case.

var Syndicate = require('./src/main.js');
var Dataspace = Syndicate.Dataspace;

assertion type foo(x, y);

ground dataspace {
  actor {
    var x = 123;
    react {
      assert foo(x, 999);

      during foo(x, $v) {
        do {
          console.log('x=', x, 'v=', v);
          if (x === 123) {
            x = 124;
          }
        }
        finally {
          console.log('finally for x=', x, 'v=', v);
        }
      }
    }
  }
}
