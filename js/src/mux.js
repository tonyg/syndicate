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

var atMetaEverything = Route.compilePattern(true, Patch.atMeta(Route.__));
var atMetaBranchKeys = Immutable.List([[Patch.atMeta.meta.arguments.length, Patch.atMeta.meta]]);
var onlyMeta = Route.trieSuccess(Immutable.Set.of("meta"));

function echoCancelledTrie(t) {
  return Route.subtract(t, atMetaEverything, function (v1, v2) {
    return v1.has("meta") ? onlyMeta : Route.emptyTrie;
  });
}

function computeEvents(oldMux, newMux, updateStreamResult) {
  var actingPid = updateStreamResult.pid;
  var delta = updateStreamResult.delta;
  var deltaAggregate = updateStreamResult.deltaAggregate;
  var deltaAggregateNoEcho = (actingPid === "meta")
      ? delta // because echo-cancellation means that meta-SCNs are always new information
      : new Patch.Patch(Route.triePruneBranch(deltaAggregate.added, atMetaBranchKeys),
			Route.triePruneBranch(deltaAggregate.removed, atMetaBranchKeys));
  var oldRoutingTable = oldMux.routingTable;
  var newRoutingTable = newMux.routingTable;
  var affectedPids =
      computeAffectedPids(oldRoutingTable, deltaAggregateNoEcho).add(actingPid).remove("meta");
  return {
    eventMap: Immutable.Map().withMutations(function (result) {
      affectedPids.forEach(function (pid) {
	var patchForPid;
	if (pid === actingPid) {
	  var part1 = new Patch.Patch(
	    echoCancelledTrie(Patch.biasedIntersection(newRoutingTable, delta.added)),
	    echoCancelledTrie(Patch.biasedIntersection(oldRoutingTable, delta.removed)));
	  var part2 = new Patch.Patch(Patch.biasedIntersection(deltaAggregateNoEcho.added,
							       newMux.interestsOf(pid)),
				      Patch.biasedIntersection(deltaAggregateNoEcho.removed,
							       oldMux.interestsOf(pid)));
	  patchForPid = part1.unsafeUnion(part2);
	} else {
	  patchForPid = deltaAggregateNoEcho.viewFrom(oldMux.interestsOf(pid));
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
  routingTable =
    Route.trieStep(routingTable, Patch.observe.meta.arguments.length, Patch.observe.meta);
  return Route.matchTrie(cover, routingTable, Immutable.Set(),
                         function (v1, v2, acc) { return acc.union(v2); });
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
