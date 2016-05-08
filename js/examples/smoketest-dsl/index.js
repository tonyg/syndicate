assertion type beep(counter);

ground dataspace {
  console.log('starting ground boot');

  actor {
    until {
      case asserted Syndicate.observe(beep(_)) {
        var counter = 0;
        state {
          init {
            :: beep(counter++);
          }
          on message beep(_) {
            :: beep(counter++);
          }
        } until {
          case (counter >= 10);
        }
      }
    }
  }

  actor {
    forever {
      on message beep($counter) {
        console.log("beep!", counter);
      }
    }
  }
}
