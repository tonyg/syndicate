var Route = require("./route.js");
var Patch = require("./patch.js");
var Util = require("./util.js");

/*---------------------------------------------------------------------------*/
/* Events and Actions */

var __ = Route.__;
var _$ = Route._$;

function stateChange(patch) {
  return { type: "stateChange", patch: patch };
}

function message(body) {
  return { type: "message", message: body };
}

function spawn(behavior) {
  return { type: "spawn", behavior: behavior };
}

function terminate() {
  return { type: "terminate" };
}

function terminateNetwork() {
  return { type: "terminateNetwork" };
}

/*---------------------------------------------------------------------------*/
/* Network */

function Network(bootFn) {
  
}

// Network

//          prepend-at-meta
//          assert
//          retract
//          sub
//          unsub
//          pub
//          unpub

///////////////////////////////////////////////////////////////////////////

module.exports.__ = __;
module.exports._$ = _$;

// module.exports.sub = sub;
// module.exports.pub = pub;
// module.exports.spawn = spawn;
// module.exports.updateRoutes = updateRoutes;
// module.exports.sendMessage = sendMessage;
// module.exports.shutdownWorld = shutdownWorld;

// module.exports.World = World;
// module.exports.DemandMatcher = DemandMatcher;
// module.exports.Deduplicator = Deduplicator;
module.exports.Route = Route;
