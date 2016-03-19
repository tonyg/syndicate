'use strict';

var Immutable = require('immutable');
var Network = require('./network.js').Network;
var Mux = require('./mux.js');
var Patch = require('./patch.js');
var Route = require('./route.js');
var Util = require('./util.js');

//---------------------------------------------------------------------------

function spawnActor(state, bootFn) {
  Network.spawn(new Actor(state, bootFn));
}

function Actor(state, bootFn) {
  this.state = state;
  this.facets = Immutable.Set();
  this.mux = new Mux.Mux();

  this.boot = function() {
    bootFn.call(this.state);
    this.checkForTermination();
  };
}

Actor.prototype.handleEvent = function(e) {
  this.facets.forEach(function (f) {
    f.handleEvent(e);
  });
  this.checkForTermination();
};

Actor.prototype.addFacet = function(facet) {
  this.facets = this.facets.add(facet);
};

Actor.prototype.removeFacet = function(facet) {
  this.facets = this.facets.remove(facet);
};

Actor.prototype.checkForTermination = function() {
  if (this.facets.isEmpty()) {
    Network.exit();
  }
};

//---------------------------------------------------------------------------

function createFacet() {
  return new Facet(Network.activeBehavior());
}

function Facet(actor) {
  this.actor = actor;
  this.endpoints = Immutable.Map();
  this.initBlocks = Immutable.List();
  this.doneBlocks = Immutable.List();
}

Facet.prototype.handleEvent = function(e) {
  var facet = this;
  this.endpoints.forEach(function(endpoint) {
    endpoint.handlerFn.call(facet.actor.state, e);
  });
  this.refresh();
};

Facet.prototype.addAssertion = function(assertionFn) {
  return this.addEndpoint(new Endpoint(assertionFn, function(e) {}));
};

Facet.prototype.onEvent = function(isTerminal, eventType, subscriptionFn, projectionFn, handlerFn) {
  var facet = this;
  switch (eventType) {

  case 'message':
    return this.addEndpoint(new Endpoint(subscriptionFn, function(e) {
      if (e.type === 'message') {
        var proj = projectionFn.call(facet.actor.state);
        var spec = Patch.prependAtMeta(proj.assertion, proj.metalevel);
        var match = Route.matchPattern(e.message, spec);
        // console.log(match);
        if (match) {
          if (isTerminal) { facet.terminate(); }
          Util.kwApply(handlerFn, facet.actor.state, match);
        }
      }
    }));

  case 'asserted': /* fall through */
  case 'retracted':
    return this.addEndpoint(new Endpoint(subscriptionFn, function(e) {
      if (e.type === 'stateChange') {
        var proj = projectionFn.call(facet.actor.state);
        var spec = Patch.prependAtMeta(proj.assertion, proj.metalevel);
        var compiledSpec = Route.compileProjection(spec);
        var objects = Route.projectObjects(eventType === 'asserted'
                                           ? e.patch.added
                                           : e.patch.removed,
                                           compiledSpec);
        if (objects) {
          // console.log(objects.toArray());
          if (isTerminal) { facet.terminate(); }
          objects.forEach(function (o) { Util.kwApply(handlerFn, facet.actor.state, o); });
        }
      }
    }));

  case 'risingEdge':
    var endpoint = new Endpoint(function() { return Patch.emptyPatch; },
                                function(e) {
                                  var newValue = subscriptionFn.call(facet.actor.state);
                                  if (newValue && !this.currentValue) {
                                    if (isTerminal) { facet.terminate(); }
                                    handlerFn.call(facet.actor.state);
                                  }
                                  this.currentValue = newValue;
                                });
    endpoint.currentValue = false;
    return this.addEndpoint(endpoint);

  default:
    throw new Error("Unsupported Facet eventType: " + eventType);
  }
};

Facet.prototype.addEndpoint = function(endpoint) {
  var patch = endpoint.subscriptionFn.call(this.actor.state);
  var r = this.actor.mux.addStream(patch);
  this.endpoints = this.endpoints.set(r.pid, endpoint);
  Network.stateChange(r.deltaAggregate);
  return this; // for chaining
};

Facet.prototype.addInitBlock = function(thunk) {
  this.initBlocks = this.initBlocks.push(thunk);
  return this;
};

Facet.prototype.addDoneBlock = function(thunk) {
  this.doneBlocks = this.doneBlocks.push(thunk);
  return this;
};

Facet.prototype.refresh = function() {
  var facet = this;
  var aggregate = Patch.emptyPatch;
  this.endpoints.forEach(function(endpoint, eid) {
    var patch =
        Patch.retract(Syndicate.__).andThen(endpoint.subscriptionFn.call(facet.actor.state));
    var r = facet.actor.mux.updateStream(eid, patch);
    aggregate = aggregate.andThen(r.deltaAggregate);
  });
  Network.stateChange(aggregate);
};

Facet.prototype.completeBuild = function() {
  var facet = this;
  this.actor.addFacet(this);
  this.initBlocks.forEach(function(b) { b.call(facet.actor.state); });
};

Facet.prototype.terminate = function() {
  var facet = this;
  var aggregate = Patch.emptyPatch;
  this.endpoints.forEach(function(endpoint, eid) {
    var r = facet.actor.mux.removeStream(eid);
    aggregate = aggregate.andThen(r.deltaAggregate);
  });
  Network.stateChange(aggregate);
  this.endpoints = Immutable.Map();
  this.actor.removeFacet(this);
  this.doneBlocks.forEach(function(b) { b.call(facet.actor.state); });
};

//---------------------------------------------------------------------------

function Endpoint(subscriptionFn, handlerFn) {
  this.subscriptionFn = subscriptionFn;
  this.handlerFn = handlerFn;
}

//---------------------------------------------------------------------------

module.exports.spawnActor = spawnActor;
module.exports.createFacet = createFacet;
