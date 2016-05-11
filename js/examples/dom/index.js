var G;
$(document).ready(function () {
  var Dataspace = Syndicate.Dataspace;
  var sub = Syndicate.sub;
  var assert = Syndicate.assert;
  var retract = Syndicate.retract;
  var __ = Syndicate.__;
  var _$ = Syndicate._$;

  G = new Syndicate.Ground(function () {
    console.log('starting ground boot');

    Syndicate.DOM.spawnDOMDriver();
    var DOM = Syndicate.DOM.DOM;
    var jQueryEvent = Syndicate.JQuery.jQueryEvent;

    Dataspace.spawn({
      boot: function () {
	return assert(DOM("#clicker-holder", "clicker",
                          '<button><span style="font-style: italic">Click me!</span></button>'))
	  .andThen(sub(jQueryEvent("button.clicker", "click", __)));
      },
      handleEvent: function (e) {
	if (e.type === "message" && jQueryEvent.isClassOf(e.message)) {
	  Dataspace.send("bump_count");
	}
      }
    });

    Dataspace.spawn({
      counter: 0,
      boot: function () {
	this.updateState();
	return sub("bump_count");
      },
      updateState: function () {
	Dataspace.stateChange(retract(DOM.pattern)
			      .andThen(assert(DOM("#counter-holder", "counter",
                                                  '<div><p>The current count is: '+this.counter+
                                                  '</p></div>'))));
      },
      handleEvent: function (e) {
	if (e.type === "message" && e.message === "bump_count") {
	  this.counter++;
	  this.updateState();
	}
      }
    });
  });

  G.dataspace.setOnStateChange(function (mux, patch) {
    $("#spy-holder").text(Syndicate.prettyTrie(mux.routingTable));
  });

  G.startStepping();
});
