"use strict";

var G;
$(document).ready(function () {
    var Dataspace = Syndicate.Dataspace;
    var sub = Syndicate.sub;
    var __ = Syndicate.__;
    var _$ = Syndicate._$;

    G = new Syndicate.Ground(function () {
      console.log('starting ground boot');

      Syndicate.JQuery.spawnJQueryDriver();

      Dataspace.spawn({
	boot: function () {
	  return sub(Syndicate.JQuery.jQueryEvent('#clicker', 'click', __));
	},
	handleEvent: function (e) {
	  if (e.type === 'message'
              && Syndicate.JQuery.jQueryEvent.isClassOf(e.message)
              && e.message.selector === '#clicker')
          {
	    var r = $('#result');
	    r.html(Number(r.html()) + 1);
	  }
	}
      });
    });
    G.dataspace.setOnStateChange(function (mux, patch) {
      $("#spy-holder").text(Syndicate.prettyTrie(mux.routingTable));
    });
    G.startStepping();
});
