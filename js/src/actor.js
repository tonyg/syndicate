'use strict';

var Immutable = require('immutable');
var _Dataspace = require('./dataspace.js');
var Dataspace = _Dataspace.Dataspace;
var __ = _Dataspace.__;
var Mux = require('./mux.js');
var Patch = require('./patch.js');
var Trie = require('./trie.js');
var Util = require('./util.js');
var Dataflow = require('./dataflow.js');

//---------------------------------------------------------------------------

function spawnActor(bootFn, optName) {
  Dataspace.spawn(new Actor(bootFn, optName));
}

function Actor(bootFn, optName) {
  this.fields = {};
  this.facets = Immutable.Set();
  this.mux = new Mux.Mux();
  this.previousKnowledge = Trie.emptyTrie;
  this.knowledge = Trie.emptyTrie;
  this.pendingActions = [];
  this.dataflowGraph = new Dataflow.Graph();

  if (typeof optName !== 'undefined') {
    this.name = optName;
  }

  this.boot = function() {
    var self = this;
    withCurrentFacet(null, function () {
      bootFn.call(self.fields);
    });
    this.quiesce();
  };
}

(function () {
  var priorities = ['PRIORITY_QUERY_HIGH',
                    'PRIORITY_QUERY',
                    'PRIORITY_QUERY_HANDLER',
                    'PRIORITY_NORMAL'];
  for (var i = 0; i < priorities.length; i++) {
    Actor[priorities[i]] = i;
  }
})();

Actor.prototype.nextPendingAction = function (probe) {
  for (var i = 0; i < this.pendingActions.length; i++) {
    var q = this.pendingActions[i];
    if (q.length > 0) {
      return probe ? true : q.shift();
    }
  }
  return false;
};

Actor.prototype.handleEvent = function(e) {
  var actor = this;
  if (e.type === 'stateChange') {
    this.previousKnowledge = this.knowledge;
    this.knowledge = e.patch.updateInterests(this.knowledge);
  }
  if (this.nextPendingAction(true)) {
    throw new Error('Syndicate: pendingActions must not be nonempty at start of handleEvent');
  }
  this.facets.forEach(function (f) {
    withCurrentFacet(f, function () { f.handleEvent(e, false); });
  });
  this.quiesce();
};

Actor.prototype.quiesce = function() {
  var actor = this;

  while (true) {
    var entry = this.nextPendingAction(false);
    if (!entry) break;

    withCurrentFacet(entry.facet, entry.action);

    this.dataflowGraph.repairDamage(function (subjectId) {
      var facet = subjectId[0];
      var endpoint = subjectId[1];
      if (!facet.terminated) {
        withCurrentFacet(facet, function () {
          var patch = Patch.retract(__).andThen(endpoint.subscriptionFn.call(facet.fields));
          var r = facet.actor.mux.updateStream(endpoint.eid, patch);
          Dataspace.stateChange(r.deltaAggregate);
        });
      }
    });
  }

  this.checkForTermination();
};

Actor.prototype.pushAction = function (a, priorityOpt) {
  var priority = typeof priorityOpt === 'undefined' ? Actor.PRIORITY_NORMAL : priorityOpt;
  while (this.pendingActions.length < priority + 1) {
    this.pendingActions.push([]);
  }
  this.pendingActions[priority].push({facet: Facet.current, action: a});
};

Actor.prototype.addFacet = function(facet) {
  this.facets = this.facets.add(facet);
};

Actor.prototype.removeFacet = function(facet) {
  this.facets = this.facets.remove(facet);
};

Actor.prototype.checkForTermination = function() {
  if (this.facets.isEmpty()) {
    Dataspace.exit();
  }
};

//---------------------------------------------------------------------------

function Facet(actor) {
  this.actor = actor;
  this.endpoints = Immutable.Map();
  this.initBlocks = Immutable.List();
  this.doneBlocks = Immutable.List();
  this.children = Immutable.Set();
  this.parent = Facet.current;
  this.fields = Dataflow.Graph.newScope((this.parent && this.parent.fields) || actor.fields);
  this.terminated = false;
  this.fid = Facet.nextFid++;
}

Facet.nextFid = 0;
Facet.current = null;

Facet.build = function(f) {
  var facet = new Facet(Dataspace.activeBehavior());
  withCurrentFacet(facet, f);
  facet.completeBuild();
};

function withCurrentFacet(facet, f) {
  var previousFacet = Facet.current;
  Facet.current = facet;
  var result;
  try {
    result = f();
  } catch (e) {
    Facet.current = previousFacet;
    throw e;
  }
  Facet.current = previousFacet;
  return result;
}

Facet.prototype.handleEvent = function(e, isSynthetic) {
  var facet = this;
  facet.endpoints.forEach(function(endpoint) {
    endpoint.handlerFn.call(facet.fields, e, isSynthetic);
  });
};

Facet.prototype.addAssertion = function(assertionFn) {
  return this.addEndpoint(new Endpoint(assertionFn, function(e) {}));
};

Facet.prototype.addOnEventHandler = function(handler, priorityOpt) {
  var facet = this;
  return this.addEndpoint(new Endpoint(function () { return Patch.emptyPatch; }, function (e) {
    facet.actor.pushAction(function () { handler(e); }, priorityOpt);
  }));
};

Facet.prototype.addDataflow = function(subjectFunction) {
  var facet = this;
  return this.addEndpoint(new Endpoint(function () {
    var subjectId = facet.actor.dataflowGraph.currentSubjectId;
    facet.actor.pushAction(function () {
      facet.actor.dataflowGraph.withSubject(subjectId, function () {
        subjectFunction.call(facet.fields);
      });
    });
    return Patch.emptyPatch;
  }, function (e) {}));
};

Facet.prototype.onEvent = function(priority,
                                   isTerminal,
                                   eventType,
                                   subscriptionFn,
                                   projectionFn,
                                   handlerFn)
{
  var facet = this;
  switch (eventType) {

  case 'message':
    return this.addEndpoint(new Endpoint(subscriptionFn, function(e) {
      if (e.type === 'message') {
        var proj = projectionFn.call(facet.fields);
        var spec = Patch.prependAtMeta(proj.assertion, proj.metalevel);
        var match = Trie.matchPattern(e.message, spec);
        // console.log(match);
        if (match) {
          if (isTerminal) { facet.terminate(); }
          facet.actor.pushAction(function () { Util.kwApply(handlerFn, facet.fields, match); },
                                 priority);
        }
      }
    }));

  case 'asserted': /* fall through */
  case 'retracted':
    return this.addEndpoint(new Endpoint(subscriptionFn, function(e, isSynthetic) {
      if (e.type === 'stateChange') {
        var proj = projectionFn.call(facet.fields);
        var spec = Patch.prependAtMeta(proj.assertion, proj.metalevel);
        var objects = Trie.projectObjects(eventType === 'asserted'
                                          ? e.patch.added
                                          : e.patch.removed,
                                          spec);
        if (objects && objects.size > 0) {
          var shouldTerminate = isTerminal;
          objects.forEach(function (o) {
            var instantiated = Trie.instantiateProjection(spec, o);
            if (facet.interestWas(eventType, instantiated, isSynthetic)) {
              if (shouldTerminate) {
                shouldTerminate = false;
                facet.terminate();
              }
              facet.actor.pushAction(function () {
                Util.kwApply(handlerFn, facet.fields, o);
              }, priority);
            }
          });
        }
      }
    }));

  case 'risingEdge':
    var endpoint = new Endpoint(function() {
      var newValue = subscriptionFn.call(facet.fields);
      if (newValue && !this.currentValue) {
        if (isTerminal) { facet.terminate(); }
        facet.actor.pushAction(function () {
          handlerFn.call(facet.fields);
        }, priority);
      }
      this.currentValue = newValue;
      return Patch.emptyPatch;
    }, function(e) {});
    endpoint.currentValue = false;
    return this.addEndpoint(endpoint);

  default:
    throw new Error("Unsupported Facet eventType: " + eventType);
  }
};

Facet.prototype.interestWas = function(assertedOrRetracted, pat, isSyntheticEvent) {
  function orStar(a, b) { return (a || b); }
  var previousKnowledge = isSyntheticEvent ? Trie.emptyTrie : this.actor.previousKnowledge;
  var oldExists = Trie.matchValue(previousKnowledge, pat, false, orStar);
  var newExists = Trie.matchValue(this.actor.knowledge, pat, false, orStar);
  switch (assertedOrRetracted) {
    case 'asserted':
      return !oldExists && newExists;
    case 'retracted':
      return oldExists && !newExists;
    default:
      throw new Error("Unexpected assertedOrRetracted in Facet.interestWas: " + assertedOrRetracted);
  }
};

Facet.prototype.addEndpoint = function(endpoint) {
  var facet = this;
  var patch = facet.actor.dataflowGraph.withSubject([facet, endpoint], function () {
    return endpoint.subscriptionFn.call(facet.fields);
  });
  var r = facet.actor.mux.addStream(patch);
  endpoint.eid = r.pid;
  facet.endpoints = facet.endpoints.set(endpoint.eid, endpoint);
  Dataspace.stateChange(r.deltaAggregate);
  return facet; // for chaining
};

Facet.prototype.addInitBlock = function(thunk) {
  this.initBlocks = this.initBlocks.push(thunk);
  return this;
};

Facet.prototype.addDoneBlock = function(thunk) {
  this.doneBlocks = this.doneBlocks.push(thunk);
  return this;
};

Facet.prototype.completeBuild = function() {
  var facet = this;
  this.actor.addFacet(this);
  if (this.parent) {
    this.parent.children = this.parent.children.add(this);
  }
  withCurrentFacet(facet, function () {
    facet.initBlocks.forEach(function(b) { b.call(facet.fields); });
  });
  var initialEvent = _Dataspace.stateChange(new Patch.Patch(facet.actor.knowledge, Trie.emptyTrie));
  withCurrentFacet(facet, function () { facet.handleEvent(initialEvent, true); });
};

Facet.prototype.terminate = function() {
  var facet = this;

  if (facet.terminated) return;
  facet.terminated = true;

  var aggregate = Patch.emptyPatch;
  this.endpoints.forEach(function(endpoint, eid) {
    var r = facet.actor.mux.removeStream(eid);
    aggregate = aggregate.andThen(r.deltaAggregate);
  });
  Dataspace.stateChange(aggregate);

  this.endpoints = Immutable.Map();
  if (this.parent) {
    this.parent.children = this.parent.children.remove(this);
  }

  this.actor.removeFacet(this);

  this.children.forEach(function (child) {
    child.terminate();
  });

  withCurrentFacet(facet, function () {
    facet.doneBlocks.forEach(function(b) { b.call(facet.fields); });
  });
};

//---------------------------------------------------------------------------

function Endpoint(subscriptionFn, handlerFn) {
  this.subscriptionFn = subscriptionFn;
  this.handlerFn = handlerFn;
  this.eid = 'uninitialized_eid'; // initialized later
}

//---------------------------------------------------------------------------

function referenceField(obj, prop) {
  if (!(prop in obj)) {
    Dataspace.activeBehavior().dataflowGraph.recordObservation(Immutable.List.of(obj, prop));
  }
  return obj[prop];
}

function declareField(obj, prop, init) {
  if (prop in obj) {
    obj[prop] = init;
  } else {
    Dataspace.activeBehavior().dataflowGraph.defineObservableProperty(obj, prop, init, {
      objectId: Immutable.List.of(obj, prop)
    });
  }
}

function deleteField(obj, prop) {
  Dataspace.activeBehavior().dataflowGraph.recordDamage(Immutable.List.of(obj, prop));
  return delete obj[prop];
}

//---------------------------------------------------------------------------

module.exports.spawnActor = spawnActor;
module.exports.Facet = Facet;
module.exports.referenceField = referenceField;
module.exports.declareField = declareField;
module.exports.deleteField = deleteField;
