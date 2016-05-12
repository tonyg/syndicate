"use strict";
new Syndicate.Ground(function () {
  Syndicate.UI.spawnUIDriver();

  Syndicate.Actor.spawnActor(new Object(), function() {
    var counter = 0;
    var ui = new Syndicate.UI.Anchor();
    Syndicate.Actor.createFacet()
.addAssertion((function() { var _ = Syndicate.__; return Syndicate.Patch.assert(ui.html('#button-label',''+counter), 0); }))
.onEvent(false, "message", (function() { var _ = Syndicate.__; return Syndicate.Patch.sub(Syndicate.UI.globalEvent('#counter','click',_), 0); }), (function() { var _ = Syndicate.__; return { assertion: Syndicate.UI.globalEvent('#counter','click',_), metalevel: 0 }; }), (function() {
        counter++;
      })).completeBuild();
  });
}).startStepping();
