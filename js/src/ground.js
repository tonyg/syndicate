"use strict";

var Immutable = require('immutable');
var Network = require('./network.js').Network;

function Ground(bootFn) {
  var self = this;
  this.stepperId = null;
  this.baseStack = Immutable.List.of({ network: this, activePid: -1 });
  Network.withNetworkStack(this.baseStack, function () {
    self.network = new Network(bootFn);
  });
}

Ground.prototype.step = function () {
  var self = this;
  return Network.withNetworkStack(this.baseStack, function () {
    return self.network.step();
  });
};

Ground.prototype.checkPid = function (pid) {
  if (pid !== -1) console.error('Weird pid in Ground', pid);
};

Ground.prototype.markRunnable = function (pid) {
  this.checkPid(pid);
  this.startStepping();
};

Ground.prototype.startStepping = function () {
  var self = this;
  if (this.stepperId) return;
  if (this.step()) {
    this.stepperId = setTimeout(function () {
      self.stepperId = null;
      self.startStepping();
    }, 0);
  }
};

Ground.prototype.stopStepping = function () {
  if (this.stepperId) {
    clearTimeout(this.stepperId);
    this.stepperId = null;
  }
};

Ground.prototype.kill = function (pid, exn) {
  this.checkPid(pid);
  console.log("Ground network terminated");
  this.stopStepping();
};

Ground.prototype.enqueueAction = function (pid, action) {
  this.checkPid(pid);

  switch (action.type) {
  case 'stateChange':
    if (action.patch.isNonEmpty()) {
      console.error('You have subscribed to a nonexistent event source.',
		    action.patch.pretty());
    }
    break;

  case 'message':
    console.error('You have sent a message into the outer void.', action);
    break;

  default:
    console.error('Internal error: unexpected action at ground level', action);
    break;
  }
};

///////////////////////////////////////////////////////////////////////////

module.exports.Ground = Ground;
