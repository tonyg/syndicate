// bin/syndicatec compiler/demo-bankaccount.js | node

var Syndicate = require('./src/main.js');

assertion type account(balance);
message type deposit(amount);

ground dataspace {
  actor {
    field this.balance = 0;

    react {
      assert account(this.balance);
      dataflow {
        console.log("Balance inside account is", this.balance);
      }
      on message deposit($amount) {
        this.balance += amount;
      }
    }
  }

  actor {
    react {
      on asserted account($balance) {
        console.log("Balance is now", balance);
      }
    }
  }

  actor {
    react {
      do {
        console.log("Waiting for account.");
      }
      finally {
        console.log("Account became ready.");
      }
    } until {
      case asserted Syndicate.observe(deposit(_)) {
        :: deposit(+100);
        :: deposit(-30);
      }
    }
  }
}
