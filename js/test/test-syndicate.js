"use strict";

var expect = require('expect.js');
var Immutable = require('immutable');

var Syndicate = require('../src/main.js');
var Dataspace = Syndicate.Dataspace;
var Patch = Syndicate.Patch;

var __ = Syndicate.__;
var _$ = Syndicate._$;

function configurationTrace(bootConfiguration) {
  var eventLog = [];
  function trace(item) {
    eventLog.push(item);
  }

  var G = new Syndicate.Ground(function () {
    bootConfiguration(trace);
  });

  while (G.step()) {
    // do nothing until G becomes inert
  }

  return eventLog;
}

function traceEvent(trace) {
  return function(item) {
    trace((item.type === "stateChange") ? item.patch.pretty() : item);
  }
}

function checkTrace(bootConfiguration, expected) {
  expect(configurationTrace(bootConfiguration)).to.eql(expected);
}

describe("configurationTrace", function() {
  describe("with an inert configuration", function () {
    it("should yield an empty trace", function () {
      checkTrace(function (trace) {}, []);
    });
  });

  describe("with a single trace in an inert configuration", function () {
    it("should yield that trace", function () {
      checkTrace(function (trace) { trace(1) }, [1]);
    });
  });

  describe("with some traced communication", function () {
    it("should yield an appropriate trace", function () {
      checkTrace(function (trace) {
	Dataspace.spawn({
	  boot: function () { return Syndicate.sub(__); },
	  handleEvent: traceEvent(trace)
	});
	Dataspace.send(123);
	Dataspace.send(234);
      }, ['<<<<<<<< Removed:\n'+
	  ' ::: nothing\n'+
	  '======== Added:\n'+
	  ' observe<1> ★ {[0]}\n'+
	  '>>>>>>>>',
	  Syndicate.message(123),
	  Syndicate.message(234)]);
    });
  });
});

describe("nonempty initial routes", function () {
  it("should be immediately signalled to the process", function () {
    // Specifically, no Syndicate.updateRoutes([]) first.
    checkTrace(function (trace) {
      Dataspace.spawn({
	boot: function () { return Patch.assert(["A", __]); },
	handleEvent: function (e) {}
      });
      Dataspace.spawn({
	boot: function () { return Patch.sub(["A", __]); },
	handleEvent: traceEvent(trace)
      });
    }, ['<<<<<<<< Removed:\n'+
	' ::: nothing\n'+
	'======== Added:\n'+
	' <2> "A" ★ {[0]}\n'+
	'>>>>>>>>']);
  });
});

describe("nested actor with an echoey protocol", function () {
  it("shouldn't see an echoed assertion", function () {
    checkTrace(function (trace) {
      Dataspace.spawn(new Dataspace(function () {
	Dataspace.spawn({
	  boot: function () {
	    Dataspace.stateChange(Patch.retract("X", 1)); // happens after subs on next line!
	    return Patch.sub("X", 1).andThen(Patch.assert("X", 1));
	  },
	  handleEvent: traceEvent(trace)
	});
      }));
    }, ['<<<<<<<< Removed:\n'+
	' ::: nothing\n'+
	'======== Added:\n'+
	' atMeta<1> "X" {["meta"]}\n'+
	'>>>>>>>>',
	'<<<<<<<< Removed:\n'+
	' atMeta<1> "X" {["meta"]}\n'+
	'======== Added:\n'+
	' ::: nothing\n'+
	'>>>>>>>>']);
  })
  it("shouldn't see an echoed message", function () {
    checkTrace(function (trace) {
      Dataspace.spawn(new Dataspace(function () {
	Dataspace.spawn({
	  boot: function () {
	    Dataspace.send("X", 1); // happens after subs on next line!
            return Patch.sub("X", 1);
	  },
	  handleEvent: traceEvent(trace)
	});
      }));
    }, [Syndicate.message(Patch.atMeta("X"))]);
  });
  it("shouldn't see an echoed assertion", function () {
  });
});

// describe("actor with nonempty initial routes", function () {
//   it("shouldn't see initial empty conversational context", function () {
//     checkTrace(function (trace) {
//       Dataspace.spawn({
// 	boot: function () { return [pub(["A", __])] },
// 	handleEvent: function (e) {
// 	  Dataspace.spawn(new Actor(function () {
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
