"use strict";
// Wake detector - notices when something (such as
// suspension/sleeping!) has caused periodic activities to be
// interrupted, and warns others about it
// Inspired by http://blog.alexmaccaw.com/javascript-wake-event

var Patch = require("./patch.js");
var Struct = require('./struct.js');

var Dataspace_ = require("./dataspace.js");
var Dataspace = Dataspace_.Dataspace;
var __ = Dataspace_.__;
var _$ = Dataspace_._$;

var wakeEvent = Struct.makeConstructor('wakeEvent', []);

function spawnWakeDetector(periodOpt) {
  Dataspace.spawn(new WakeDetector(periodOpt));
}

function WakeDetector(periodOpt) {
  this.period = periodOpt || 10000;
  this.mostRecentTrigger = +(new Date());
  this.timerId = null;
}

WakeDetector.prototype.boot = function () {
  var self = this;
  this.timerId = setInterval(Dataspace.wrap(function () { self.trigger(); }), this.period);
  return Patch.pub(wakeEvent());
};

WakeDetector.prototype.handleEvent = function (e) {};

WakeDetector.prototype.trigger = function () {
  var now = +(new Date());
  if (now - this.mostRecentTrigger > this.period * 1.5) {
    Dataspace.send(wakeEvent());
  }
  this.mostRecentTrigger = now;
};

///////////////////////////////////////////////////////////////////////////

module.exports.spawnWakeDetector = spawnWakeDetector;
module.exports.WakeDetector = WakeDetector;
module.exports.wakeEvent = wakeEvent;
