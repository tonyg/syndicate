"use strict";
// Property-based "dataflow"

var Immutable = require("immutable");
var MapSet = require("./mapset.js");

function Graph() {
  this.edgesForward = Immutable.Map();
  this.edgesReverse = Immutable.Map();
  this.damagedNodes = Immutable.Set();
  this.currentSubjectId = null;
  this.enforceSubjectPresence = true;
  this.observablePropertyCounter = 0;
}

Graph.prototype.withSubject = function (subjectId, f) {
  var oldSubjectId = this.currentSubjectId;
  this.currentSubjectId = subjectId;
  var result;
  try {
    result = f();
  } catch (e) {
    this.currentSubjectId = oldSubjectId;
    throw e;
  }
  this.currentSubjectId = oldSubjectId;
  return result;
};

Graph.prototype.recordObservation = function (objectId) {
  if (this.currentSubjectId) {
    this.edgesForward = MapSet.add(this.edgesForward, objectId, this.currentSubjectId);
    this.edgesReverse = MapSet.add(this.edgesReverse, this.currentSubjectId, objectId);
  } else if (this.enforceSubjectPresence) {
    throw new Error('Attempt to observe ' + objectId + ' with no currentSubjectId');
  }
};

Graph.prototype.recordDamage = function (objectId) {
  this.damagedNodes = this.damagedNodes.add(objectId);
};

Graph.prototype.forgetSubject = function (subjectId) {
  var self = this;
  var subjectObjects = self.edgesReverse.get(subjectId) || Immutable.Set();
  self.edgesReverse = self.edgesReverse.remove(subjectId);
  subjectObjects.forEach(function (objectId) {
    self.edgesForward = MapSet.remove(self.edgesForward, objectId, subjectId);
  });
};

Graph.prototype.repairDamage = function (repairNode) {
  var self = this;
  var repairedThisRound = Immutable.Set();
  while (true) {
    var workSet = self.damagedNodes;
    self.damagedNodes = Immutable.Set();

    var alreadyDamaged = workSet.intersect(repairedThisRound);
    if (!alreadyDamaged.isEmpty()) {
      console.warn('Cyclic dependencies involving', alreadyDamaged);
    }

    workSet = workSet.subtract(repairedThisRound);
    repairedThisRound = repairedThisRound.union(workSet);

    if (workSet.isEmpty()) break;

    workSet.forEach(function (objectId) {
      var subjects = self.edgesForward.get(objectId) || Immutable.Set();
      subjects.forEach(function (subjectId) {
        self.forgetSubject(subjectId);
        self.withSubject(subjectId, function () {
          repairNode(subjectId);
        });
      });
    });
  }
};

Graph.prototype.defineObservableProperty = function (obj, prop, value, maybeOptions) {
  var graph = this;
  var options = typeof maybeOptions === 'undefined' ? {} : maybeOptions;
  var objectId = '__' + (options.baseId || prop) + '_' + (graph.observablePropertyCounter++);
  Object.defineProperty(obj, prop, {
    configurable: true,
    enumerable: true,
    get: function () {
      graph.recordObservation(objectId);
      return value;
    },
    set: function (newValue) {
      if (!options.noopGuard || !options.noopGuard(value, newValue)) {
        graph.recordDamage(objectId);
        value = newValue;
      }
    }
  });
  return objectId;
};

Graph.newScope = function (o) {
  function O() {}
  O.prototype = o;
  return new O();
};

///////////////////////////////////////////////////////////////////////////

module.exports.Graph = Graph;
