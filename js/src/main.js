"use strict";

function copyKeys(keys, to, from) {
  for (var i = 0; i < keys.length; i++) {
    to[keys[i]] = from[keys[i]];
  }
}

module.exports = require("./syndicate.js");

module.exports.Route = require("./route.js");
copyKeys(['__', '_$', '$Capture', '$Special',
	  'is_emptyTrie', 'emptyTrie',
	  'embeddedTrie', 'compilePattern',
	  'compileProjection', 'project', 'projectObjects',
	  'prettyTrie'],
	 module.exports,
	 module.exports.Route);

module.exports.DemandMatcher = require('./demand-matcher.js').DemandMatcher;
module.exports.Seal = require('./seal.js').Seal;

module.exports.DOM = require("./dom-driver.js");
module.exports.JQuery = require("./jquery-driver.js");
// module.exports.RoutingTableWidget = require("./routing-table-widget.js");
// module.exports.WebSocket = require("./websocket-driver.js");
module.exports.Reflect = require("./reflect.js");

module.exports.Patch = require("./patch.js");
copyKeys(['emptyPatch',
	  'observe', 'atMeta', 'advertise',
	  'isObserve', 'isAtMeta', 'isAdvertise',
	  'assert', 'retract', 'sub', 'unsub', 'pub', 'unpub',
	  'patchSeq'],
	 module.exports,
	 module.exports.Patch);

module.exports.Ground = require("./ground.js").Ground;
// module.exports.Actor = require("./actor.js").Actor;
// module.exports.Spy = require("./spy.js").Spy;
// module.exports.WakeDetector = require("./wake-detector.js").WakeDetector;

// var Worker = require("./worker.js");
// module.exports.Worker = Worker.Worker;
// module.exports.WorkerGround = Worker.WorkerGround;
