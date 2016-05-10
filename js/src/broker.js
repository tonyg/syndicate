"use strict";
// WebSocket-based Syndicate broker client

var Immutable = require('immutable');
var Trie = require('./trie.js');
var Patch = require('./patch.js');
var Struct = require('./struct.js');
var DemandMatcher = require('./demand-matcher.js').DemandMatcher;
var Codec = require('./codec');

var Dataspace_ = require("./dataspace.js");
var Dataspace = Dataspace_.Dataspace;
var __ = Dataspace_.__;
var _$ = Dataspace_._$;

var DEFAULT_RECONNECT_DELAY = 100; // ms
var MAX_RECONNECT_DELAY = 30000; // ms
var DEFAULT_IDLE_TIMEOUT = 300000; // ms; i.e., 5 minutes
var DEFAULT_PING_INTERVAL = DEFAULT_IDLE_TIMEOUT - 10000; // ms

var toBroker = Struct.makeConstructor('toBroker', ['url', 'assertion']);
var fromBroker = Struct.makeConstructor('fromBroker', ['url', 'assertion']);
var brokerConnection = Struct.makeConstructor('brokerConnection', ['url']);
var brokerConnected = Struct.makeConstructor('brokerConnected', ['url']);
var forceBrokerDisconnect = Struct.makeConstructor('forceBrokerDisconnect', ['url']);

function spawnBrokerClientDriver() {
  var URL = _$('url'); // capture used to extract URL
  Dataspace.spawn(
    new Dataspace(function () {
      Dataspace.spawn(
        new DemandMatcher([brokerConnection(URL)],
                          [brokerConnection(URL)],
                          {
                            demandMetaLevel: 1,
                            supplyMetaLevel: 0,
                            onDemandIncrease: function (c) {
                              Dataspace.spawn(new BrokerClientConnection(c.url));
                            }
                          }));
    }));
}

function BrokerClientConnection(wsurl) {
  this.wsurl = wsurl;
  this.sock = null;

  this.sendsAttempted = 0;
  this.sendsTransmitted = 0;
  this.receiveCount = 0;
  this.connectionCount = 0;

  this.reconnectDelay = DEFAULT_RECONNECT_DELAY;
  this.idleTimeout = DEFAULT_IDLE_TIMEOUT;
  this.pingInterval = DEFAULT_PING_INTERVAL;

  this.localAssertions = Trie.emptyTrie;
  this.remoteAssertions = Trie.emptyTrie;

  this.activityTimestamp = 0;
  this.idleTimer = null;
  this.pingTimer = null;
}

BrokerClientConnection.prototype.clearHeartbeatTimers = function () {
  if (this.idleTimer) { clearTimeout(this.idleTimer); this.idleTimer = null; }
  if (this.pingTimer) { clearTimeout(this.pingTimer); this.pingTimer = null; }
};

BrokerClientConnection.prototype.recordActivity = function () {
  var self = this;
  this.activityTimestamp = +(new Date());
  this.clearHeartbeatTimers();
  this.idleTimer = setTimeout(function () { self.forceclose(); }, this.idleTimeout);
  this.pingTimer = setTimeout(function () { self.safeSend(JSON.stringify("ping")) },
                              this.pingInterval);
};

BrokerClientConnection.prototype.boot = function () {
  this.reconnect();
  var initialAssertions =
      Patch.sub(toBroker(this.wsurl, __), 1) // read assertions to go out
      .andThen(Patch.sub(Patch.observe(fromBroker(this.wsurl, __)), 1)) // and monitor interests
      .andThen(Patch.assert(brokerConnection(this.wsurl))) // signal to DemandMatcher that we exist
      .andThen(Patch.sub(brokerConnection(this.wsurl), 1)) // track demand
      .andThen(Patch.sub(forceBrokerDisconnect(this.wsurl), 1))
  ;
  return initialAssertions;
};

BrokerClientConnection.prototype.trapexit = function () {
  this.forceclose();
};

BrokerClientConnection.prototype.isConnected = function () {
  return this.sock && this.sock.readyState === this.sock.OPEN;
};

BrokerClientConnection.prototype.safeSend = function (m) {
  // console.log('safeSend', m);
  try {
    this.sendsAttempted++;
    if (this.isConnected()) {
      this.sock.send(m);
      this.sendsTransmitted++;
    }
  } catch (e) {
    console.warn("Trapped exn while sending", e);
  }
};

BrokerClientConnection.prototype.sendPatch = function (p) {
  var j = JSON.stringify(Codec.encodeEvent(Syndicate.stateChange(p)));
  this.safeSend(j);
};

BrokerClientConnection.prototype.handleEvent = function (e) {
  // console.log("BrokerClientConnection.handleEvent", e);
  switch (e.type) {
    case "stateChange":
      if (e.patch.project(Patch.atMeta(brokerConnection(_$))).hasRemoved()) {
        // console.log("Client is no longer interested in this connection", this.wsurl);
        Dataspace.exit();
      }

      var pTo = e.patch.project(Patch.atMeta(toBroker(__, _$)));

      var pObsFrom = e.patch.project(Patch.atMeta(Patch.observe(fromBroker(__, _$))));
      pObsFrom = new Patch.Patch(
        Trie.compilePattern(true, Patch.observe(Trie.embeddedTrie(pObsFrom.added))),
        Trie.compilePattern(true, Patch.observe(Trie.embeddedTrie(pObsFrom.removed))));

      var newLocalAssertions = this.localAssertions;
      newLocalAssertions = pTo.label(Immutable.Set.of("to")).applyTo(newLocalAssertions);
      newLocalAssertions = pObsFrom.label(Immutable.Set.of("obsFrom")).applyTo(newLocalAssertions);

      var trueSet = Immutable.Set.of(true);
      var alwaysTrueSet = function (v) { return trueSet; };
      var p = Patch.computePatch(Trie.relabel(this.localAssertions, alwaysTrueSet),
                                 Trie.relabel(newLocalAssertions, alwaysTrueSet));

      this.localAssertions = newLocalAssertions;
      // console.log("localAssertions");
      // console.log(Trie.prettyTrie(this.localAssertions));
      // console.log(p.pretty());
      this.sendPatch(p);
      break;

    case "message":
      var m = e.message;
      if (Patch.atMeta.isClassOf(m)) {
        m = m[0];
        if (toBroker.isClassOf(m)) {
          var j = JSON.stringify(Codec.encodeEvent(Syndicate.message(m[1])));
          this.safeSend(j);
        } else if (forceBrokerDisconnect.isClassOf(m)) {
          this.forceclose();
        }
      }
      break;
  }
};

BrokerClientConnection.prototype.forceclose = function (keepReconnectDelay) {
  if (!keepReconnectDelay) {
    this.reconnectDelay = DEFAULT_RECONNECT_DELAY;
  }
  this.clearHeartbeatTimers();
  if (this.sock) {
    console.log("BrokerClientConnection.forceclose called");
    this.sock.close();
    this.sock = null;
  }
};

BrokerClientConnection.prototype.reconnect = function () {
  var self = this;
  this.forceclose(true);
  this.connectionCount++;
  this.sock = new WebSocket(this.wsurl);
  this.sock.onopen = Dataspace.wrap(function (e) { return self.onopen(e); });
  this.sock.onmessage = Dataspace.wrap(function (e) {
    self.receiveCount++;
    return self.onmessage(e);
  });
  this.sock.onclose = Dataspace.wrap(function (e) { return self.onclose(e); });
};

BrokerClientConnection.prototype.onopen = function (e) {
  console.log("connected to " + this.sock.url);
  this.recordActivity();
  Dataspace.stateChange(Patch.assert(brokerConnected(this.wsurl), 1));
  this.reconnectDelay = DEFAULT_RECONNECT_DELAY;
  this.sendPatch((new Patch.Patch(this.localAssertions, Trie.emptyTrie)).strip());
};

BrokerClientConnection.prototype.onmessage = function (wse) {
  // console.log("onmessage", wse);
  this.recordActivity();

  var j = JSON.parse(wse.data, Struct.reviver);
  if (j === "ping") {
    this.safeSend(JSON.stringify("pong"));
    return;
  } else if (j === "pong") {
    return; // recordActivity already took care of our timers
  }

  var e = Codec.decodeAction(j);
  switch (e.type) {
    case "stateChange": {
      var added = fromBroker(this.wsurl, Trie.embeddedTrie(e.patch.added));
      var removed = fromBroker(this.wsurl, Trie.embeddedTrie(e.patch.removed));
      var p = Patch.assert(added, 1).andThen(Patch.retract(removed, 1));
      // console.log('incoming stateChange');
      // console.log(p.pretty());
      Dataspace.stateChange(p);
      break;
    }
    case "message": {
      Dataspace.send(fromBroker(this.wsurl, e.message), 1);
      break;
    }
  }
};

BrokerClientConnection.prototype.onclose = function (e) {
  var self = this;

  // console.log("onclose", e);
  Dataspace.stateChange(Patch.retract(brokerConnected(this.wsurl), 1));

  console.log("reconnecting to " + this.wsurl + " in " + this.reconnectDelay + "ms");
  setTimeout(Dataspace.wrap(function () { self.reconnect(); }), this.reconnectDelay);

  this.reconnectDelay = this.reconnectDelay * 1.618 + (Math.random() * 1000);
  this.reconnectDelay =
    this.reconnectDelay > MAX_RECONNECT_DELAY
    ? MAX_RECONNECT_DELAY + (Math.random() * 1000)
    : this.reconnectDelay;
};

///////////////////////////////////////////////////////////////////////////

module.exports.toBroker = toBroker;
module.exports.fromBroker = fromBroker;
module.exports.brokerConnection = brokerConnection;
module.exports.brokerConnected = brokerConnected;
module.exports.forceBrokerDisconnect = forceBrokerDisconnect;

module.exports.spawnBrokerClientDriver = spawnBrokerClientDriver;
module.exports.BrokerClientConnection = BrokerClientConnection;
