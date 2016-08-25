ground dataspace {
  Syndicate.UI.spawnUIDriver();

  actor {
    var ui = new Syndicate.UI.Anchor();
    field this.counter = 0;
    assert ui.html('#button-label', '' + this.counter);
    on message Syndicate.UI.globalEvent('#counter', 'click', _) {
      this.counter++;
    }
  }
}
