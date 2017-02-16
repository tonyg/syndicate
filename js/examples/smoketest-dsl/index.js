assertion type beep(counter);

ground dataspace {
  console.log('starting ground boot');

  spawn {
    stop on asserted Syndicate.observe(beep(_)) {
      field this.counter = 0;
      react {
        on start {
          :: beep(this.counter++);
        }
        on message beep(_) {
          :: beep(this.counter++);
        }
        stop on (this.counter > 10);
      }
    }
  }

  spawn {
    on message beep($counter) {
      console.log("beep!", counter);
    }
  }
}
