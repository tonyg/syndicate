"use strict";

var Immutable = require('immutable');
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

function terminateDataspace() {
  return { type: 'terminateDataspace' };
}

/*---------------------------------------------------------------------------*/
/* Dataspace */

function Dataspace(bootFn) {
  this.pendingActions = Immutable.List(); // of [pid, Action]
  this.processTable = Immutable.Map(); // pid -> Behavior
  this.runnablePids = Immutable.Set(); // of pid
  this.mux = new Mux.Mux();
  this.onStateChange = function (mux, deltaAggregate) {};
  this.asChild('meta', function () { return bootFn() }, true);
}

// Class state and methods

Dataspace.stack = Immutable.List();

Dataspace.current = function () {
  return Dataspace.stack.last().dataspace;
};

Dataspace.activePid = function () {
  return Dataspace.stack.last().activePid;
};

Dataspace.activeBehavior = function () {
  var entry = Dataspace.stack.last();
  var p = entry.dataspace.processTable.get(entry.activePid);
  return p ? p.behavior : null;
};

Dataspace.withDataspaceStack = function (stack, f) {
  var oldStack = Dataspace.stack;
  Dataspace.stack = stack;
  var result;
  try {
    result = f();
  } catch (e) {
    Dataspace.stack = oldStack;
    throw e;
  }
  Dataspace.stack = oldStack;
  return result;
};

Dataspace.wrap = function (f) {
  var savedStack = Dataspace.stack;
  return function () {
    var actuals = arguments;
    return Dataspace.withDataspaceStack(savedStack, function () {
      var result = Dataspace.current().asChild(Dataspace.activePid(), function () {
	return f.apply(null, actuals);
      });
      Dataspace.stack.reverse().forEach(function (entry) {
	entry.dataspace.markRunnable(entry.activePid);
      });
      return result;
    });
  };
};

Dataspace.enqueueAction = function (action) {
  var entry = Dataspace.stack.last();
  entry.dataspace.enqueueAction(entry.activePid, action);
};

Dataspace.send = function (body, metaLevel) {
  Dataspace.enqueueAction(message(Patch.prependAtMeta(body, metaLevel || 0)));
};

Dataspace.stateChange = function (patch) {
  Dataspace.enqueueAction(stateChange(patch));
};

Dataspace.spawn = function (behavior) {
  Dataspace.enqueueAction(spawn(behavior));
};

Dataspace.exit = function (exn) {
  Dataspace.current().kill(Dataspace.activePid(), exn);
};

Dataspace.exitDataspace = function () {
  Dataspace.enqueueAction(terminateDataspace());
};

Dataspace.inertBehavior = {
  handleEvent: function (e) {}
};

// Instance methods

Dataspace.prototype.asChild = function (pid, f, omitLivenessCheck) {
  var self = this;
  var p = this.processTable.get(pid, null);
  if (!omitLivenessCheck && (p === null)) {
    console.warn("Dataspace.asChild eliding invocation of dead process", pid);
    return;
  }

  return Dataspace.withDataspaceStack(
    Dataspace.stack.push({ dataspace: this, activePid: pid }),
    function () {
      try {
	return f(p);
      } catch (e) {
	self.kill(pid, e);
      }
    });
};

Dataspace.prototype.kill = function (pid, exn) {
  if (exn && exn.stack) {
    console.log("Process exiting", pid, exn, exn.stack);
  } else {
    console.log("Process exiting", pid, exn);
  }
  var p = this.processTable.get(pid);
  this.processTable = this.processTable.set(pid, { behavior: Dataspace.inertBehavior });
  if (p) {
    if (p.behavior.trapexit) {
      this.asChild(pid, function () { return p.behavior.trapexit(exn); }, true);
    }
    this.enqueueAction(pid, terminate());
  }
};

Dataspace.prototype.boot = function () {
  // Needed in order for a new Dataspace to be marked as "runnable", so
  // its initial actions get performed.
};

Dataspace.prototype.handleEvent = function (e) {
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

Dataspace.prototype.step = function () {
  return this.dispatchActions()
    && this.runRunnablePids()
    && ((this.pendingActions.size > 0) || (this.runnablePids.size > 0));
};

Dataspace.prototype.enqueueAction = function (pid, action) {
  this.pendingActions = this.pendingActions.push([pid, action]);
};

Dataspace.prototype.dispatchActions = function () {
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

Dataspace.prototype.markRunnable = function (pid) {
  this.runnablePids = this.runnablePids.add(pid);
};

Dataspace.prototype.runRunnablePids = function () {
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

Dataspace.prototype.interpretAction = function (pid, action) {
  var self = this;

  switch (action.type) {
  case 'stateChange':
    var oldMux = this.mux.shallowCopy();
    this.deliverPatches(oldMux, this.mux.updateStream(pid, action.patch));
    return true;

  case 'message':
    if (Patch.observe.isClassOf(action.message)) {
      console.warn('Process ' + pid + ' send message containing query', action.message);
    }
    if (pid !== 'meta' && Patch.atMeta.isClassOf(action.message)) {
      Dataspace.send(action.message.assertion);
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

  case 'terminateDataspace':
    Dataspace.exit();
    return false;

  default:
    var exn = new Error("Action type " + action.type + " not understood");
    exn.action = action;
    throw exn;
  }
};

Dataspace.prototype.deliverPatches = function (oldMux, updateStreamResult) {
  var self = this;
  var events = Mux.computeEvents(oldMux, this.mux, updateStreamResult);
  events.eventMap.forEach(function (patch, pid) {
    self.deliverEvent(pid, stateChange(patch));
  });
  events.metaEvents.forEach(Dataspace.stateChange);
  this.onStateChange(this.mux, updateStreamResult.deltaAggregate);
};

Dataspace.prototype.deliverEvent = function (pid, event) {
  var childBusy = this.asChild(pid, function (p) { return p.behavior.handleEvent(event); });
  if (childBusy) this.markRunnable(pid);
};

///////////////////////////////////////////////////////////////////////////

module.exports.stateChange = stateChange;
module.exports.message = message;
module.exports.spawn = spawn;
module.exports.terminate = terminate;
module.exports.terminateDataspace = terminateDataspace;

module.exports.Dataspace = Dataspace;
