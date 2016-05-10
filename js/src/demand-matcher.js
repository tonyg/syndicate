var Immutable = require('immutable');
var Trie = require('./trie.js');
var Patch = require('./patch.js');
var Util = require('./util.js');

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

function defaultHandler(side, movement) {
  return function (captures) {
    console.error("Syndicate: Unhandled "+movement+" in "+side, captures);
  };
}

function DemandMatcher(demandSpecs, supplySpecs, options) {
  options = Util.extend({
    metaLevel: 0,
    demandMetaLevel: null,
    supplyMetaLevel: null,
    onDemandIncrease: defaultHandler('demand', 'increase'),
    onDemandDecrease: function (captures) {},
    onSupplyIncrease: function (captures) {},
    onSupplyDecrease: defaultHandler('supply', 'decrease')
  }, options);

  this.demandProjectionNames = ensureMatchingProjectionNames(demandSpecs);
  this.supplyProjectionNames = ensureMatchingProjectionNames(supplySpecs);

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

  this.onDemandIncrease = options.onDemandIncrease;
  this.onDemandDecrease = options.onDemandDecrease;
  this.onSupplyIncrease = options.onSupplyIncrease;
  this.onSupplyDecrease = options.onSupplyDecrease;

  this.currentDemand = Immutable.Set();
  this.currentSupply = Immutable.Set();
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

DemandMatcher.prototype.handlePatch = function (p) {
  var self = this;

  var dN = self.demandProjectionNames.length;
  var sN = self.supplyProjectionNames.length;
  var addedDemand = this.extractKeys(p.added, self.demandProjections, dN, 'demand');
  var removedDemand = this.extractKeys(p.removed, self.demandProjections, dN, 'demand');
  var addedSupply = this.extractKeys(p.added, self.supplyProjections, sN, 'supply');
  var removedSupply = this.extractKeys(p.removed, self.supplyProjections, sN, 'supply');

  self.currentSupply = self.currentSupply.union(addedSupply);
  self.currentDemand = self.currentDemand.subtract(removedDemand);

  removedSupply.forEach(function (captures) {
    if (self.currentDemand.has(captures)) {
      self.onSupplyDecrease(Trie.captureToObject(captures, self.supplyProjectionNames));
    }
  });
  addedSupply.forEach(function (captures) {
    if (!self.currentDemand.has(captures)) {
      self.onSupplyIncrease(Trie.captureToObject(captures, self.supplyProjectionNames));
    }
  });

  removedDemand.forEach(function (captures) {
    if (self.currentSupply.has(captures)) {
      self.onDemandDecrease(Trie.captureToObject(captures, self.demandProjectionNames));
    }
  });
  addedDemand.forEach(function (captures) {
    if (!self.currentSupply.has(captures)) {
      self.onDemandIncrease(Trie.captureToObject(captures, self.demandProjectionNames));
    }
  });

  self.currentSupply = self.currentSupply.subtract(removedSupply);
  self.currentDemand = self.currentDemand.union(addedDemand);
};

///////////////////////////////////////////////////////////////////////////

module.exports.DemandMatcher = DemandMatcher;
