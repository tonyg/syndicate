// bin/syndicatec compiler/demo-bad-this.js | node
//
// Bug with this-ness. Symptomatic output:
//
// + render one false
// + render two false
// present one
// - render one false
// - render two false
// + render one one
// + render two one
//
// Good output:
//
// + render one false
// + render two false
// present one
// - render one false
// + render one one

var Syndicate = require('./src/main.js');

assertion type user(who);
assertion type present(who);
assertion type rendered(who, isPresent);

ground dataspace {
  actor {
    assert user('one');
    assert present('one');
  }

  actor {
    assert user('two');
    // assert present('two');
  }

  actor {
    during user($who) {
      field this.isPresent = false;
      on asserted present(who) {
        console.log('present', who);
        this.isPresent = who;
      }
      on retracted present(who) {
        console.log('absent', who);
        this.isPresent = false;
      }
      assert rendered(who, this.isPresent);
    }
  }

  actor {
    during rendered($who, $isPresent) {
      on start { console.log('+ render', who, isPresent); }
      on stop { console.log('- render', who, isPresent); }
    }
  }
}
