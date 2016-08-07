assertion type beep(counter);

ground dataspace {
  console.log('starting ground boot');

  actor {
    react until {
      case asserted Syndicate.observe(beep(_)) {
        field this.counter = 0;
        react {
          do {
            :: beep(this.counter++);
          }
          on message beep(_) {
            :: beep(this.counter++);
          }
        } until {
          case (this.counter > 10);
        }
      }
    }
  }

  actor {
    react {
      on message beep($counter) {
        console.log("beep!", counter);
      }
    }
  }
}
