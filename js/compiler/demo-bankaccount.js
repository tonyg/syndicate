// bin/syndicatec compiler/demo-bankaccount.js | node

var Syndicate = require('./src/main.js');

assertion type account(balance);
message type deposit(amount);

ground dataspace {
  spawn {
    field this.balance = 0;
    assert account(this.balance);
    dataflow {
      console.log("Balance inside account is", this.balance);
    }
    on message deposit($amount) {
      this.balance += amount;
    }
  }

  spawn {
    on asserted account($balance) {
      console.log("Balance is now", balance);
    }
  }

  spawn {
    on start {
      console.log("Waiting for account.");
    }
    stop on asserted Syndicate.observe(deposit(_)) {
      console.log("Account became ready.");
      :: deposit(+100);
      :: deposit(-30);
    }
  }
}
