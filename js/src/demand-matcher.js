var Immutable = require('immutable');
var Trie = require('./trie.js');
var Patch = require('./patch.js');
var Util = require('./util.js');

///////////////////////////////////////////////////////////////////////////
// Protocol between DemandMatcher and taskSupervisor functions

// Bits:
var IS_CHANGING = 1;
var IS_PRESENT  = 2;

// Bit combinations:
var LOW     = 0                          ;
var RISING  =                IS_CHANGING ;
var HIGH    =   IS_PRESENT               ;
var FALLING =   IS_PRESENT | IS_CHANGING ;

///////////////////////////////////////////////////////////////////////////
// Default task supervision strategy. See syndicate/doc/demand-matcher.md.

function defaultTaskSupervisor(demandState, supplyState, supervisionState, taskFn, onTaskExit) {
  var oldESI = supervisionState ? supervisionState.expectSupplyIncrease : false;
  var oldESD = supervisionState ? supervisionState.expectSupplyDecrease : false;

  var newESI = oldESI;
  var newESD = oldESD;

  if ((demandState === FALLING) && ((supplyState === RISING) ||
                                    (supplyState === HIGH) ||
                                    oldESI)) {
    newESD = true;
  }

  if (!oldESI && ((demandState === RISING) ||
                  (demandState === HIGH)) && ((supplyState === LOW) ||
                                              (supplyState === FALLING))) {
    if ((demandState === HIGH) && !oldESD) {
      onTaskExit();
    } else {
      taskFn();
      newESI = true;
    }
  }

  if (supplyState === FALLING) newESD = false;
  if (supplyState === RISING)  newESI = false;

  if (newESI || newESD) {
    return { expectSupplyIncrease: newESI, expectSupplyDecrease: newESD };
  } else {
    return null;
  }
}

function defaultOnTaskExit(captures) {
  console.error("Syndicate: DemandMatcher detected unexpected drop in supply", captures);
}

///////////////////////////////////////////////////////////////////////////
// DemandMatcher itself

function DemandMatcher(demandSpecs, supplySpecs, startTask, options) {
  options = Util.extend({
    metaLevel: 0,
    demandMetaLevel: null,
    supplyMetaLevel: null,
    taskSupervisor: defaultTaskSupervisor,
    onTaskExit: defaultOnTaskExit
  }, options);

  if (typeof startTask !== 'function') {
    throw new Error("Syndicate: DemandMatcher expects 'startTask' function as third argument");
  }

  this.demandProjectionNames = ensureMatchingProjectionNames(demandSpecs);
  this.supplyProjectionNames = ensureMatchingProjectionNames(supplySpecs);
  ensureMatchingProjectionNames([demandSpecs[0], supplySpecs[0]]);

  this.demandSpecs = demandSpecs;
  this.supplySpecs = supplySpecs;

  this.demandPatterns = demandSpecs.map(function (s) { return Trie.projectionToPattern(s); });
  this.supplyPatterns = supplySpecs.map(function (s) { return Trie.projectionToPattern(s); });

  this.demandMetaLevel =
    (options.demandMetaLevel === null) ? options.metaLevel : options.demandMetaLevel;
  this.supplyMetaLevel =
    (options.supplyMetaLevel === null) ? options.metaLevel : options.supplyMetaLevel;

  function metaWrap(n) {
    return function (s) { return Patch.prependAtMeta(s, n); };
  }
  this.demandProjections = demandSpecs.map(metaWrap(this.demandMetaLevel));
  this.supplyProjections = supplySpecs.map(metaWrap(this.supplyMetaLevel));

  this.taskSupervisor = options.taskSupervisor;
  this.startTask = startTask;
  this.onTaskExit = options.onTaskExit;

  this.currentDemand = Immutable.Set();
  this.currentSupply = Immutable.Set();
  this.supervisionStates = Immutable.Map();
}

function ensureMatchingProjectionNames(specs) {
  if (!(specs.length > 0)) {
    throw new Error("Syndicate: DemandMatcher needs at least one spec");
  }

  var names = null;
  specs.forEach(function (spec) {
    if (names === null) {
      names = Trie.projectionNames(spec);
    } else {
      if (JSON.stringify(names) !== JSON.stringify(Trie.projectionNames(spec))) {
        throw new Error("Syndicate: DemandMatcher needs identical capture names");
      }
    }
  });
  return names;
}

DemandMatcher.prototype.boot = function () {
  var p = Patch.emptyPatch;
  function extend(ml) {
    return function (pat) { p = p.andThen(Patch.sub(pat, ml)); };
  }
  this.demandPatterns.forEach(extend(this.demandMetaLevel));
  this.supplyPatterns.forEach(extend(this.supplyMetaLevel));
  return p;
};

DemandMatcher.prototype.handleEvent = function (e) {
  if (e.type === "stateChange") {
    this.handlePatch(e.patch);
  }
};

DemandMatcher.prototype.handlePatch = function (p) {
  var self = this;

  var dN = self.demandProjectionNames.length;
  var sN = self.supplyProjectionNames.length;
  var addedDemand = self.extractKeys(p.added, self.demandProjections, dN, 'demand');
  var removedDemand = self.extractKeys(p.removed, self.demandProjections, dN, 'demand');
  var addedSupply = self.extractKeys(p.added, self.supplyProjections, sN, 'supply');
  var removedSupply = self.extractKeys(p.removed, self.supplyProjections, sN, 'supply');

  // Though the added and removed sets of patches are always disjoint,
  // *after projection* this may not hold. Cancel out any overlaps.
  var demandOverlap = addedDemand.intersect(removedDemand);
  var supplyOverlap = addedSupply.intersect(removedSupply);
  addedDemand = addedDemand.subtract(demandOverlap);
  removedDemand = removedDemand.subtract(demandOverlap);
  addedSupply = addedSupply.subtract(supplyOverlap);
  removedSupply = removedSupply.subtract(supplyOverlap);

  var allTasks = addedDemand.union(addedSupply).union(removedDemand).union(removedSupply);

  allTasks.forEach(function (captures) {
    function taskFn() {
      self.startTask(Trie.captureToObject(captures, self.demandProjectionNames));
    }
    function onTaskExit() {
      self.onTaskExit(Trie.captureToObject(captures, self.demandProjectionNames));
    }

    var demandState = computeState(self.currentDemand, addedDemand, removedDemand, captures);
    var supplyState = computeState(self.currentSupply, addedSupply, removedSupply, captures);
    var oldSupervisionState = self.supervisionStates.get(captures, null);
    var newSupervisionState = self.taskSupervisor(demandState,
                                                  supplyState,
                                                  oldSupervisionState,
                                                  taskFn,
                                                  onTaskExit);
    if (newSupervisionState === null) {
      self.supervisionStates = self.supervisionStates.remove(captures);
    } else {
      self.supervisionStates = self.supervisionStates.set(captures, newSupervisionState);
    }
  });

  self.currentSupply = self.currentSupply.union(addedSupply).subtract(removedSupply);
  self.currentDemand = self.currentDemand.union(addedDemand).subtract(removedDemand);
};

DemandMatcher.prototype.extractKeys = function (trie, projections, keyCount, whichSide) {
  var ks = Immutable.Set();
  projections.forEach(function (proj) {
    var moreKs = Trie.trieKeys(Trie.project(trie, proj), keyCount);
    if (!moreKs) {
      throw new Error("Syndicate: wildcard "+whichSide+" detected:\n" +
                      JSON.stringify(proj) + "\n" +
                      Trie.prettyTrie(trie));
    }
    ks = ks.union(moreKs);
  });
  return ks;
};

function computeState(current, added, removed, captures) {
  var isPresent = current.has(captures);
  var isChanging = added.has(captures) || removed.has(captures);
  return (isPresent ? IS_PRESENT : 0) | (isChanging ? IS_CHANGING : 0);
}

///////////////////////////////////////////////////////////////////////////

module.exports.DemandMatcher = DemandMatcher;
