"use strict";
var DOM = Syndicate.DOM.DOM;
var jQueryEvent = Syndicate.JQuery.jQueryEvent;

$(document).ready(function() {
  new Syndicate.Ground(function () {
    Syndicate.DOM.spawnDOMDriver();
    Syndicate.JQuery.spawnJQueryDriver();

    Syndicate.Actor.spawnActor(new Object(), function() {
      this.counter = 0;
      Syndicate.Actor.createFacet()
.addAssertion((function() { var _ = Syndicate.__; return Syndicate.Patch.assert(DOM('#button-label','',Syndicate.seal(this.counter)), 0); }))
.onEvent(false, "message", (function() { var _ = Syndicate.__; return Syndicate.Patch.sub(jQueryEvent('#counter','click',_), 0); }), (function() { var _ = Syndicate.__; return { assertion: jQueryEvent('#counter','click',_), metalevel: 0 }; }), (function() {
          this.counter++;
        })).completeBuild();
    });
  }).startStepping();
});
