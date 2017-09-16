importScripts("../../dist/syndicate.js");

var G = new Syndicate.WorkerGround(function () {
  spawn {
    var ui = new Syndicate.UI.Anchor();
    field this.counter = 0;

    assert ui.html('#counter-holder', '<div><p>The current count is: '+this.counter+'</p></div>')
           metalevel 1;

    on message 'bump_count'
       metalevel 1
    {
      this.counter++;
    }
  }
});

G.startStepping();
