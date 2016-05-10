"use strict";

function copyKeys(keys, to, from) {
  for (var i = 0; i < keys.length; i++) {
    to[keys[i]] = from[keys[i]];
  }
}

module.exports = require("./dataspace.js");

module.exports.Trie = require("./trie.js");
copyKeys(['__', '_$', '$Capture', '$Special',
	  'is_emptyTrie', 'emptyTrie',
	  'embeddedTrie', 'compilePattern',
	  'project', 'projectObjects',
	  'prettyTrie'],
	 module.exports,
	 module.exports.Trie);
module.exports.Struct = require('./struct.js');

var Seal = require('./seal.js')
copyKeys(['Seal', 'seal'],
	 module.exports,
	 Seal);

module.exports.DemandMatcher = require('./demand-matcher.js').DemandMatcher;
module.exports.Ack = require('./ack.js').Ack;

module.exports.RandomID = require('./randomid.js');
module.exports.DOM = require("./dom-driver.js");
module.exports.JQuery = require("./jquery-driver.js");
module.exports.WakeDetector = require("./wake-detector-driver.js");
module.exports.Reflect = require("./reflect.js");

module.exports.Patch = require("./patch.js");
copyKeys(['emptyPatch',
	  'observe', 'atMeta', 'advertise',
	  'assert', 'retract', 'sub', 'unsub', 'pub', 'unpub',
	  'patchSeq'],
	 module.exports,
	 module.exports.Patch);

module.exports.Ground = require("./ground.js").Ground;
module.exports.Actor = require("./actor.js");
// module.exports.Spy = require("./spy.js").Spy;
// module.exports.WakeDetector = require("./wake-detector.js").WakeDetector;

// var Worker = require("./worker.js");
// module.exports.Worker = Worker.Worker;
// module.exports.WorkerGround = Worker.WorkerGround;
