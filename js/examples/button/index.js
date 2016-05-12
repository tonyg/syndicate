ground dataspace {
  Syndicate.UI.spawnUIDriver();

  actor {
    var counter = 0;
    var ui = new Syndicate.UI.Anchor();
    react {
      assert ui.html('#button-label', '' + counter);
      on message Syndicate.UI.globalEvent('#counter', 'click', _) {
        counter++;
      }
    }
  }
}
