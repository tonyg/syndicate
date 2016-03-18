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
  };
}

Actor.prototype.handleEvent = function(e) {
  this.facets.forEach(function (f) {
    f.handleEvent(e);
  });
};

Actor.prototype.addFacet = function(facet) {
  this.facets = this.facets.add(facet);
};

//---------------------------------------------------------------------------

function createFacet() {
  return new Facet(Network.activeBehavior());
}

function Facet(actor) {
  this.actor = actor;
  this.endpoints = Immutable.Map();
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
  return this.addEndpoint(new Endpoint(subscriptionFn, function(e) {
    var proj = projectionFn.call(facet.actor.state);
    var spec = Patch.prependAtMeta(proj.assertion, proj.metalevel);

    switch (e.type) {
    case 'message':
      if (eventType === 'message') {
        var match = Route.matchPattern(e.message, spec);
        // console.log(match);
        if (match) {
          if (isTerminal) { facet.terminate(); }
          Util.kwApply(handlerFn, facet.actor.state, match);
        }
      }
      break;
    case 'stateChange':
      {
        var objects;
        switch (eventType) {
        case 'asserted':
          objects = Route.projectObjects(e.patch.added, Route.compileProjection(spec));
          break;
        case 'retracted':
          objects = Route.projectObjects(e.patch.removed, Route.compileProjection(spec));
          break;
        default:
          break;
        }
        if (objects) {
          if (isTerminal) { facet.terminate(); }
          // console.log(objects.toArray());
          objects.forEach(function (o) { Util.kwApply(handlerFn, facet.actor.state, o); });
        }
      }
    }

  }));
};

Facet.prototype.addEndpoint = function(endpoint) {
  var patch = endpoint.subscriptionFn.call(this.actor.state);
  var r = this.actor.mux.addStream(patch);
  this.endpoints = this.endpoints.set(r.pid, endpoint);
  Network.stateChange(r.deltaAggregate);
  return this; // for chaining
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
  this.actor.addFacet(this);
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
};

//---------------------------------------------------------------------------

function Endpoint(subscriptionFn, handlerFn) {
  this.subscriptionFn = subscriptionFn;
  this.handlerFn = handlerFn;
}

//---------------------------------------------------------------------------

module.exports.spawnActor = spawnActor;
module.exports.createFacet = createFacet;
