// Illustrates a bug where dataflow damage was being repaired even for
// subjects pertaining to previously-terminated facets.
//
// Should eventually be turned into some kind of test case.

//---------------------------------------------------------------------------
// Bad output:
//
// OUTER++ first
// ++ first
// VIEW++ VIEW first
// VIEW-- VIEW first
// VIEW++ EDIT first
// VIEW-- EDIT first
// VIEW++ VIEW first
// -- first
// OUTER-- first
// OUTER++ second
// ++ second
// VIEW-- VIEW first
// VIEW++ VIEW second
// Kicking off second edit cycle
// VIEW-- VIEW second
// VIEW++ EDIT second
// VIEW++ EDIT first
// VIEW-- EDIT second
// VIEW-- EDIT first
// VIEW++ VIEW second
// VIEW++ VIEW first
//
// Notice the appearance of "first" even after the "second edit cycle" has been kicked off!
//---------------------------------------------------------------------------
// Good output:
//
// OUTER++ first
// ++ first
// VIEW++ VIEW first
// VIEW-- VIEW first
// VIEW++ EDIT first
// VIEW-- EDIT first
// VIEW++ VIEW first
// -- first
// OUTER-- first
// OUTER++ second
// ++ second
// VIEW-- VIEW first
// VIEW++ VIEW second
// Kicking off second edit cycle
// VIEW-- VIEW second
// VIEW++ EDIT second
// VIEW-- EDIT second
// VIEW++ VIEW second
//---------------------------------------------------------------------------

var Syndicate = require('./src/main.js');

assertion type todo(title);
assertion type show();
assertion type view(str);

ground dataspace {
  actor {
    react {
      field this.title = "first";
      assert todo(this.title);
      on message 3 {
        this.title = "second";
      }
    }
  }

  actor {
    react {
      assert show();
    }
  }

  actor {
    react {
      field this.editing = false;

      during todo($title) {
        do { console.log('OUTER++', title); }
        during show() {
          do { console.log('++', title); }
          assert view((this.editing ? 'EDIT ' : 'VIEW ') + title);
          finally { console.log('--', title); }
        }
        finally { console.log('OUTER--', title); }
      }

      on message 1 {
        this.editing = true;
        :: 2;
      }

      on message 2 {
        :: 3;
        this.editing = false;
      }
    }
  }

  actor {
    react {
      do { :: 0; }
    } until {
      case message 0 {
        :: 1;
      }
    }
  }

  actor {
    react {
      field this.count = 0;
      on retracted view($x) { console.log('VIEW--', x); }
      on asserted view($x) {
        console.log('VIEW++', x);
        if (x === 'VIEW second') {
          this.count++;
          if (this.count === 1) {
            console.log("Kicking off second edit cycle");
            :: 1;
          }
        }
      }
    }
  }
}
