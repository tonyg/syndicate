assertion type beep(counter);

ground dataspace {
  console.log('starting ground boot');

  actor {
    react until {
      case asserted Syndicate.observe(beep(_)) {
        var counter = 0;
        react {
          do {
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
    react {
      on message beep($counter) {
        console.log("beep!", counter);
      }
    }
  }
}
