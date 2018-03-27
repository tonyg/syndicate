// bin/syndicatec compiler/demo-synthetic-patch-2.js | node
//
// Analogous example to syndicate/racket/syndicate/examples/actor/example-synthetic-patch-2.rkt.
//
// Symptomatic output:
//
// Outer value 4 = 4
// Value 0 = 0
// Value 1 = 1
// Value 2 = 2
// Value 3 = 3
//
// Correct output:
//
// Outer value 4 = 4
// Value 0 = 0
// Value 1 = 1
// Value 2 = 2
// Value 3 = 3
// Value 4 = 4
// Value 5 = 5

var Syndicate = require('./src/main.js');

assertion type mapping(key, value);
assertion type ready();

ground dataspace {
  spawn {
    field this.ofInterest = 0;
    during ready() {
      on asserted mapping(this.ofInterest, $v) {
        console.log("Value", this.ofInterest, "=", v);
        this.ofInterest += 1;
      }
    }
    on asserted mapping(4, $v) {
      console.log("Outer value", 4, "=", v);
    }
  }

  spawn {
    assert mapping(0, 0);
    assert mapping(1, 1);
    assert mapping(2, 2);
    assert mapping(3, 3);
    assert mapping(4, 4);
    assert mapping(5, 5);
    on start {
      react {
        assert ready();
      }
    }
  }
}
