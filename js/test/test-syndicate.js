"use strict";

var expect = require('expect.js');
var Syndicate = require('../src/main.js');

var Network = Syndicate.Network;

// var sub = Syndicate.sub;
// var pub = Syndicate.pub;
// var __ = Syndicate.__;
// var _$ = Syndicate._$;

// function configurationTrace(bootConfiguration) {
//   var eventLog = [];
//   function trace(item) {
//     eventLog.push(item);
//   }

//   var G = new Syndicate.Ground(function () {
//     bootConfiguration(trace);
//   });

//   while (G.step()) {
//     // do nothing until G becomes inert
//   }

//   return eventLog;
// }

// function checkTrace(bootConfiguration, expected) {
//   expect(configurationTrace(bootConfiguration)).to.eql(expected);
// }

// describe("configurationTrace", function() {
//   describe("with an inert configuration", function () {
//     it("should yield an empty trace", function () {
//       checkTrace(function (trace) {}, []);
//     });
//   });

//   describe("with a single trace in an inert configuration", function () {
//     it("should yield that trace", function () {
//       checkTrace(function (trace) { trace(1) }, [1]);
//     });
//   });

//   describe("with some traced communication", function () {
//     it("should yield an appropriate trace", function () {
//       checkTrace(function (trace) {
// 	Network.spawn({
// 	  boot: function () { return [sub(__)] },
// 	  handleEvent: function (e) {
// 	    trace(e);
// 	  }
// 	});
// 	Network.send(123);
// 	Network.send(234);
//       }, [Syndicate.updateRoutes([]),
// 	  Syndicate.sendMessage(123),
// 	  Syndicate.sendMessage(234)]);
//     });
//   });
// });

// describe("nonempty initial routes", function () {
//   it("should be immediately signalled to the process", function () {
//     // Specifically, no Syndicate.updateRoutes([]) first.
//     checkTrace(function (trace) {
//       Network.spawn({
// 	boot: function () { return [pub(["A", __])] },
// 	handleEvent: function (e) {
// 	  Network.spawn({
// 	    boot: function () { return [sub(["A", __], 0, 1)] },
// 	    handleEvent: trace
// 	  });
// 	}
//       });
//     }, [Syndicate.updateRoutes([pub(["A", __]).label(1)])]);
//   });
// });

// describe("actor with nonempty initial routes", function () {
//   it("shouldn't see initial empty conversational context", function () {
//     checkTrace(function (trace) {
//       Network.spawn({
// 	boot: function () { return [pub(["A", __])] },
// 	handleEvent: function (e) {
// 	  Network.spawn(new Actor(function () {
// 	    Actor.observeAdvertisers(
// 	      function () { return ["A", __] },
// 	      { presence: "isPresent" },
// 	      function () {
// 		trace(["isPresent", this.isPresent]);
// 	      });
// 	  }));
// 	}
//       });
//     }, [["isPresent", true]]);
//   });
// });
