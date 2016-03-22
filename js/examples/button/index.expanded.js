"use strict";
var DOM = (function() {
  var $SyndicateMeta$ = {
    label: "DOM",
    arguments: ["containerSelector","fragmentClass","spec"]
  };
  return function DOM(containerSelector, fragmentClass, spec) {
    return {
      "containerSelector": containerSelector,
      "fragmentClass": fragmentClass,
      "spec": spec,
      "$SyndicateMeta$": $SyndicateMeta$
    };
  };
})();
var jQuery = (function() {
  var $SyndicateMeta$ = {
    label: "jQuery",
    arguments: ["selector","eventType","event"]
  };
  return function jQuery(selector, eventType, event) {
    return {
      "selector": selector,
      "eventType": eventType,
      "event": event,
      "$SyndicateMeta$": $SyndicateMeta$
    };
  };
})();

$(document).ready(function() {
  new Syndicate.Ground(function () {
    Syndicate.DOM.spawnDOMDriver();
    Syndicate.JQuery.spawnJQueryDriver();

    Syndicate.Actor.spawnActor(new Object(), function() {
      this.counter = 0;
      Syndicate.Actor.createFacet()
.addAssertion((function() { var _ = Syndicate.__; return Syndicate.Patch.assert(DOM('#button-label','',Syndicate.seal(this.counter)), 0); }))
.onEvent(false, "message", (function() { var _ = Syndicate.__; return Syndicate.Patch.sub(jQuery('#counter','click',_), 0); }), (function() { var _ = Syndicate.__; return { assertion: jQuery('#counter','click',_), metalevel: 0 }; }), (function() {
          this.counter++;
        })).completeBuild();
    });
  }).startStepping();
});
