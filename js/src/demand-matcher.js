var Immutable = require('immutable');
var Syndicate = require('./syndicate.js');
var Route = require('./route.js');
var Patch = require('./patch.js');
var Util = require('./util.js');

function DemandMatcher(demandSpec, supplySpec, options) {
  options = Util.extend({
    metaLevel: 0,
    onDemandIncrease: function (captures) {
      console.error("Syndicate: Unhandled increase in demand", captures);
    },
    onSupplyDecrease: function (captures) {
      console.error("Syndicate: Unhandled decrease in supply", captures);
    }
  }, options);
  this.metaLevel = options.metaLevel;
  this.onDemandIncrease = options.onDemandIncrease;
  this.onSupplyDecrease = options.onSupplyDecrease;
  this.demandSpec = demandSpec;
  this.supplySpec = supplySpec;
  this.demandPattern = Route.projectionToPattern(demandSpec);
  this.supplyPattern = Route.projectionToPattern(supplySpec);
  this.demandProjection = Route.compileProjection(Patch.prependAtMeta(demandSpec, this.metaLevel));
  this.supplyProjection = Route.compileProjection(Patch.prependAtMeta(supplySpec, this.metaLevel));
  this.currentDemand = Immutable.Set();
  this.currentSupply = Immutable.Set();
}

DemandMatcher.prototype.boot = function () {
  return Patch.sub(this.demandPattern, this.metaLevel)
    .andThen(Patch.sub(this.supplyPattern, this.metaLevel));
};

DemandMatcher.prototype.handleEvent = function (e) {
  if (e.type === "stateChange") {
    this.handlePatch(e.patch);
  }
};

DemandMatcher.prototype.handlePatch = function (p) {
  var self = this;

  var addedDemand = Route.trieKeys(Route.project(p.added, self.demandProjection));
  var removedDemand = Route.trieKeys(Route.project(p.removed, self.demandProjection));
  var addedSupply = Route.trieKeys(Route.project(p.added, self.supplyProjection));
  var removedSupply = Route.trieKeys(Route.project(p.removed, self.supplyProjection));

  if (addedDemand === null) {
    throw new Error("Syndicate: wildcard demand detected:\n" +
		    self.demandSpec + "\n" +
		    p.pretty());
  }
  if (addedSupply === null) {
    throw new Error("Syndicate: wildcard supply detected:\n" +
		    self.supplySpec + "\n" +
		    p.pretty());
  }

  self.currentSupply = self.currentSupply.union(addedSupply);
  self.currentDemand = self.currentDemand.subtract(removedDemand);

  removedSupply.forEach(function (captures) {
    if (self.currentDemand.has(captures)) {
      self.onSupplyDecrease(Route.captureToObject(captures, self.supplyProjection));
    }
  });
  addedDemand.forEach(function (captures) {
    if (!self.currentSupply.has(captures)) {
      self.onDemandIncrease(Route.captureToObject(captures, self.demandProjection));
    }
  });

  self.currentSupply = self.currentSupply.subtract(removedSupply);
  self.currentDemand = self.currentDemand.union(addedDemand);
};

///////////////////////////////////////////////////////////////////////////

module.exports.DemandMatcher = DemandMatcher;
