"use strict";

var G;
$(document).ready(function () {
    var Network = Syndicate.Network;
    var sub = Syndicate.sub;
    var __ = Syndicate.__;
    var _$ = Syndicate._$;

    G = new Syndicate.Ground(function () {
      console.log('starting ground boot');

      Network.spawn({
	counter: 0,
	boot: function () {},
	handleEvent: function (e) {},
	step: function () {
	  Network.send(["beep", this.counter++]);
	  return this.counter <= 10;
	}
      });

      Network.spawn({
	boot: function () { return sub(["beep", __]); },
	handleEvent: function (e) {
	  if (e.type === 'message') {
	    console.log("beep!", e.message[1]);
	  }
	}
      });
    });
    G.startStepping();
});
