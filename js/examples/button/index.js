ground dataspace {
  Syndicate.UI.spawnUIDriver();

  actor {
    field this.counter = 0;
    var ui = new Syndicate.UI.Anchor();
    react {
      assert ui.html('#button-label', '' + this.counter);
      on message Syndicate.UI.globalEvent('#counter', 'click', _) {
        this.counter++;
      }
    }
  }
}
