"use strict";

var Immutable = require('immutable');
var Route = require('./route.js');
var Patch = require('./patch.js');
var Mux = require('./mux.js');

/*---------------------------------------------------------------------------*/
/* Events and Actions */

function stateChange(patch) {
  return { type: 'stateChange', patch: patch };
}

function message(body) {
  return { type: 'message', message: body };
}

function spawn(behavior) {
  return { type: 'spawn', behavior: behavior };
}

function terminate() {
  return { type: 'terminate' };
}

function terminateNetwork() {
  return { type: 'terminateNetwork' };
}

/*---------------------------------------------------------------------------*/
/* Network */

function Network(bootFn) {
  this.pendingActions = Immutable.List(); // of [pid, Action]
  this.processTable = Immutable.Map(); // pid -> Behavior
  this.runnablePids = Immutable.Set(); // of pid
  this.mux = new Mux.Mux();
  this.onStateChange = function (mux, deltaAggregate) {};
  this.asChild('meta', function () { return bootFn() }, true);
}

// Class state and methods

Network.stack = Immutable.List();

Network.current = function () {
  return Network.stack.last().network;
};

Network.activePid = function () {
  return Network.stack.last().activePid;
};

Network.activeBehavior = function () {
  var entry = Network.stack.last();
  var p = entry.network.processTable.get(entry.activePid);
  return p ? p.behavior : null;
};

Network.withNetworkStack = function (stack, f) {
  var oldStack = Network.stack;
  Network.stack = stack;
  var result;
  try {
    result = f();
  } catch (e) {
    Network.stack = oldStack;
    throw e;
  }
  Network.stack = oldStack;
  return result;
};

Network.wrap = function (f) {
  var savedStack = Network.stack;
  return function () {
    var actuals = arguments;
    return Network.withNetworkStack(savedStack, function () {
      var result = Network.current().asChild(Network.activePid(), function () {
	return f.apply(null, actuals);
      });
      Network.stack.reverse().forEach(function (entry) {
	entry.network.markRunnable(entry.activePid);
      });
      return result;
    });
  };
};

Network.enqueueAction = function (action) {
  var entry = Network.stack.last();
  entry.network.enqueueAction(entry.activePid, action);
};

Network.send = function (body, metaLevel) {
  Network.enqueueAction(message(Patch.prependAtMeta(body, metaLevel || 0)));
};

Network.stateChange = function (patch) {
  Network.enqueueAction(stateChange(patch));
};

Network.spawn = function (behavior) {
  Network.enqueueAction(spawn(behavior));
};

Network.exit = function (exn) {
  Network.current().kill(Network.activePid(), exn);
};

Network.exitNetwork = function () {
  Network.enqueueAction(terminateNetwork());
};

Network.inertBehavior = {
  handleEvent: function (e) {}
};

// Instance methods

Network.prototype.asChild = function (pid, f, omitLivenessCheck) {
  var self = this;
  var p = this.processTable.get(pid, null);
  if (!omitLivenessCheck && (p === null)) {
    console.warn("Network.asChild eliding invocation of dead process", pid);
    return;
  }

  return Network.withNetworkStack(
    Network.stack.push({ network: this, activePid: pid }),
    function () {
      try {
	return f(p);
      } catch (e) {
	self.kill(pid, e);
      }
    });
};

Network.prototype.kill = function (pid, exn) {
  if (exn && exn.stack) {
    console.log("Process exiting", pid, exn, exn.stack);
  } else {
    console.log("Process exiting", pid, exn);
  }
  var p = this.processTable.get(pid);
  this.processTable = this.processTable.set(pid, { behavior: Network.inertBehavior });
  if (p) {
    if (p.behavior.trapexit) {
      this.asChild(pid, function () { return p.behavior.trapexit(exn); }, true);
    }
    this.enqueueAction(pid, terminate());
  }
};

Network.prototype.boot = function () {
  // Needed in order for a new Network to be marked as "runnable", so
  // its initial actions get performed.
};

Network.prototype.handleEvent = function (e) {
  switch (e.type) {
  case 'stateChange':
    this.enqueueAction('meta', stateChange(e.patch.lift()));
    break;
  case 'message':
    this.enqueueAction('meta', message(Patch.atMeta(e.message)));
    break;
  default:
    var exn = new Error("Event type " + e.type + " not understood");
    exn.event = e;
    throw exn;
  }
  return true;
};

Network.prototype.step = function () {
  return this.dispatchActions()
    && this.runRunnablePids()
    && ((this.pendingActions.size > 0) || (this.runnablePids.size > 0));
};

Network.prototype.enqueueAction = function (pid, action) {
  this.pendingActions = this.pendingActions.push([pid, action]);
};

Network.prototype.dispatchActions = function () {
  var self = this;
  var actionQueue = this.pendingActions;
  this.pendingActions = Immutable.List();
  var alive = true;
  actionQueue.forEach(function (entry) {
    var pid = entry[0];
    var action = entry[1];
    if (!self.interpretAction(pid, action)) {
      alive = false;
      return false;
    }
  });
  return alive;
};

Network.prototype.markRunnable = function (pid) {
  this.runnablePids = this.runnablePids.add(pid);
};

Network.prototype.runRunnablePids = function () {
  var self = this;
  var pidSet = this.runnablePids;
  this.runnablePids = Immutable.Set();
  pidSet.forEach(function (pid) {
    var childBusy = self.asChild(pid, function (p) {
      return p.behavior.step // exists, haven't called it yet
	&& p.behavior.step();
    });
    if (childBusy) self.markRunnable(pid);
  });
  return true;
};

Network.prototype.interpretAction = function (pid, action) {
  var self = this;

  switch (action.type) {
  case 'stateChange':
    var oldMux = this.mux.shallowCopy();
    this.deliverPatches(oldMux, this.mux.updateStream(pid, action.patch));
    return true;

  case 'message':
    if (Patch.isObserve(action.message)) {
      console.warn('Process ' + pid + ' send message containing query', action.message);
    }
    if (pid !== 'meta' && Patch.isAtMeta(action.message)) {
      Network.send(action.message[1]);
    } else {
      this.mux.routeMessage(action.message).forEach(function (pid) {
	self.deliverEvent(pid, action);
      });
    }
    return true;

  case 'spawn':
    var oldMux = this.mux.shallowCopy();
    var p = { behavior: action.behavior };
    var pid = this.mux.nextPid;
    this.processTable = this.processTable.set(pid, p);
    var initialPatch = Patch.emptyPatch;
    if (p.behavior.boot) {
      initialPatch = this.asChild(pid, function () { return p.behavior.boot() });
      initialPatch = initialPatch || Patch.emptyPatch;
      this.markRunnable(pid);
    }
    this.deliverPatches(oldMux, this.mux.addStream(initialPatch));
    return true;

  case 'terminate':
    var oldMux = this.mux.shallowCopy();
    this.deliverPatches(oldMux, this.mux.removeStream(pid));
    console.log("Process exit complete", pid);
    this.processTable = this.processTable.remove(pid);
    return true;

  case 'terminateNetwork':
    Network.exit();
    return false;

  default:
    var exn = new Error("Action type " + action.type + " not understood");
    exn.action = action;
    throw exn;
  }
};

Network.prototype.deliverPatches = function (oldMux, updateStreamResult) {
  var self = this;
  var events = Mux.computeEvents(oldMux, this.mux, updateStreamResult);
  events.eventMap.forEach(function (patch, pid) {
    self.deliverEvent(pid, stateChange(patch));
  });
  events.metaEvents.forEach(Network.stateChange);
  this.onStateChange(this.mux, updateStreamResult.deltaAggregate);
};

Network.prototype.deliverEvent = function (pid, event) {
  var childBusy = this.asChild(pid, function (p) { return p.behavior.handleEvent(event); });
  if (childBusy) this.markRunnable(pid);
};

///////////////////////////////////////////////////////////////////////////

module.exports.stateChange = stateChange;
module.exports.message = message;
module.exports.spawn = spawn;
module.exports.terminate = terminate;
module.exports.terminateNetwork = terminateNetwork;

module.exports.Network = Network;
