"use strict";

var Immutable = require('immutable');
var Route = require('./route.js');
var Patch = require('./patch.js');

function Mux(nextPid, routingTable, interestTable) {
  this.nextPid = nextPid || 0;
  this.routingTable = routingTable || Route.emptyTrie;
  this.interestTable = interestTable || Immutable.Map(); // pid -> Trie
}

Mux.prototype.shallowCopy = function () {
  return new Mux(this.nextPid, this.routingTable, this.interestTable);
};

Mux.prototype.addStream = function (initialPatch) {
  var newPid = this.nextPid++;
  return this.updateStream(newPid, initialPatch);
};

Mux.prototype.removeStream = function (pid) {
  return this.updateStream(pid, Patch.removeEverythingPatch);
};

Mux.prototype.updateStream = function (pid, unclampedPatch) {
  var oldInterests = this.interestsOf(pid);
  var oldRoutingTable = this.routingTable;
  var delta = unclampedPatch.label(Immutable.Set.of(pid)).limit(oldInterests);
  var deltaAggregate = delta.computeAggregate(pid, oldRoutingTable);
  var newInterests = delta.applyTo(oldInterests);
  var newRoutingTable = delta.applyTo(oldRoutingTable);

  this.routingTable = newRoutingTable;

  if (Route.is_emptyTrie(newInterests)) {
    this.interestTable = this.interestTable.remove(pid);
  } else {
    this.interestTable = this.interestTable.set(pid, newInterests);
  }

  return { pid: pid,
	   delta: delta,
	   deltaAggregate: deltaAggregate };
};

function computeEvents(oldMux, newMux, updateStreamResult) {
  var actingPid = updateStreamResult.pid;
  var delta = updateStreamResult.delta;
  var deltaAggregate = updateStreamResult.deltaAggregate;
  var oldRoutingTable = oldMux.routingTable;
  var newRoutingTable = newMux.routingTable;
  var affectedPids = computeAffectedPids(oldRoutingTable, delta).add(actingPid).remove("meta");
  return {
    eventMap: Immutable.Map().withMutations(function (result) {
      affectedPids.forEach(function (pid) {
	var patchForPid;
	if (pid === actingPid) {
	  var part1 = new Patch.Patch(Patch.biasedIntersection(newRoutingTable, delta.added),
				      Patch.biasedIntersection(oldRoutingTable, delta.removed));
	  var part2 = new Patch.Patch(Patch.biasedIntersection(deltaAggregate.added,
							       newMux.interestsOf(pid)),
				      Patch.biasedIntersection(deltaAggregate.removed,
							       oldMux.interestsOf(pid)));
	  patchForPid = part1.unsafeUnion(part2);
	} else {
	  patchForPid = updateStreamResult.deltaAggregate.viewFrom(oldMux.interestsOf(pid));
	}
	if (patchForPid.isNonEmpty()) {
	  result.set(pid, patchForPid);
	}
      });
    }),
    metaEvents: (actingPid === "meta")
      ? Immutable.List()
      : Immutable.List.of(delta.computeAggregate(actingPid, oldRoutingTable, true).drop())
  };
}

function computeAffectedPids(routingTable, delta) {
  var cover = Route._union(delta.added, delta.removed);
  routingTable = Route.trieStep(routingTable, Route.SOA);
  routingTable = Route.trieStep(routingTable, Patch.$Observe);
  return Route.matchTrie(cover, routingTable, Immutable.Set(),
			 function (v, r, acc) {
			   return acc.union(Route.trieStep(r, Route.EOA).value);
			 });
}

Mux.prototype.routeMessage = function (body) {
  if (Route.matchValue(this.routingTable, body) === null) {
    return Route.matchValue(this.routingTable, Patch.observe(body)) || Immutable.Set();
  } else {
    // Some other stream has declared body
    return Immutable.Set();
  }
};

Mux.prototype.interestsOf = function (pid) {
  return this.interestTable.get(pid, Route.emptyTrie);
};

///////////////////////////////////////////////////////////////////////////

module.exports.Mux = Mux;
module.exports.computeEvents = computeEvents;
module.exports.computeAffectedPids = computeAffectedPids;
