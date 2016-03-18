// node src/compiler.js src/demo-bankaccount.js | node

var Syndicate = require('./src/main.js');

assertion type account(balance);
assertion type deposit(amount);

ground network {
  actor {
    this.balance = 0;

    forever {
      assert account(this.balance);
      on message deposit($amount) {
        this.balance += amount;
      }
    }
  }

  actor {
    forever {
      on asserted account($balance) {
        console.log("Balance is now", balance);
      }
    }
  }

  actor {
    until {
      case asserted Syndicate.observe(deposit(_)) {
        :: deposit(+100);
        :: deposit(-30);
      }
    }
  }
}
