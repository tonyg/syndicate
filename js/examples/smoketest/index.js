"use strict";

var beep = Syndicate.Struct.makeConstructor('beep', ['counter']);

var G;
document.addEventListener('DOMContentLoaded', function () {
  var Dataspace = Syndicate.Dataspace;
  var sub = Syndicate.sub;
  var __ = Syndicate.__;
  var _$ = Syndicate._$;

  G = new Syndicate.Ground(function () {
    console.log('starting ground boot');

    Dataspace.spawn({
      counter: 0,
      boot: function () {},
      handleEvent: function (e) {},
      step: function () {
	Dataspace.send(beep(this.counter++));
	return this.counter <= 10;
      }
    });

    Dataspace.spawn({
      boot: function () { return sub(beep.pattern); },
      handleEvent: function (e) {
	if (e.type === 'message') {
	  console.log("beep!", e.message[0]);
	}
      }
    });
  });
  G.startStepping();
});
