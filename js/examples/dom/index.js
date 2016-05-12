var G;
document.addEventListener('DOMContentLoaded', function () {
  var Dataspace = Syndicate.Dataspace;
  var sub = Syndicate.sub;
  var assert = Syndicate.assert;
  var retract = Syndicate.retract;
  var __ = Syndicate.__;
  var _$ = Syndicate._$;

  G = new Syndicate.Ground(function () {
    console.log('starting ground boot');

    Syndicate.UI.spawnUIDriver();

    Dataspace.spawn({
      boot: function () {
        var ui = new Syndicate.UI.Anchor();
	return assert(ui.html("#clicker-holder",
                              '<button><span style="font-style: italic">Click me!</span></button>'))
	  .andThen(sub(Syndicate.UI.globalEvent("button", "click", __)));
      },
      handleEvent: function (e) {
	if (e.type === "message" && Syndicate.UI.globalEvent.isClassOf(e.message)) {
	  Dataspace.send("bump_count");
	}
      }
    });

    Dataspace.spawn({
      counter: 0,
      ui: new Syndicate.UI.Anchor(),
      boot: function () {
	this.updateState();
	return sub("bump_count");
      },
      updateState: function () {
	Dataspace.stateChange(retract(this.ui.htmlPattern)
			      .andThen(assert(this.ui.html(
                                "#counter-holder",
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
    document.getElementById('spy-holder').innerText = Syndicate.prettyTrie(mux.routingTable);
  });

  G.startStepping();
});
