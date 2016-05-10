// Timer event driver
var Patch = require("./patch.js");
var DemandMatcher = require('./demand-matcher.js').DemandMatcher;
var Struct = require('./struct.js');

var Dataspace_ = require("./dataspace.js");
var Dataspace = Dataspace_.Dataspace;
var __ = Dataspace_.__;
var _$ = Dataspace_._$;

var periodicTick = Struct.makeConstructor('periodicTick', ['intervalMS']); // message
var timeLaterThan = Struct.makeConstructor('timeLaterThan', ['deadlineMS']); // assertion

function spawnTimerDriver() {
  Dataspace.spawn(
    new DemandMatcher([Patch.observe(periodicTick(_$('intervalMS')))],
		      [Patch.advertise(periodicTick(_$('intervalMS')))],
		      {
			onDemandIncrease: function (c) {
			  Dataspace.spawn(new Tick(c.intervalMS));
			}
		      }));
  Dataspace.spawn(
    new DemandMatcher([Patch.observe(timeLaterThan(_$('deadlineMS')))],
		      [Patch.advertise(timeLaterThan(_$('deadlineMS')))],
		      {
			onDemandIncrease: function (c) {
			  Dataspace.spawn(new Alarm(c.deadlineMS));
			}
		      }));
}

function Tick(intervalMS) {
  this.intervalMS = intervalMS;
  this.handle = null;
}

Tick.prototype.boot = function () {
  var self = this;
  this.handle = setInterval(Dataspace.wrap(function () {
    Dataspace.send(periodicTick(self.intervalMS));
  }), this.intervalMS);

  return Patch.sub(Patch.observe(periodicTick(this.intervalMS))) // monitor interest
    .andThen(Patch.pub(periodicTick(this.intervalMS))) // signal we exist to DemandMatcher
  ;
};

Tick.prototype.trapexit = function () {
  this.cancelTimer();
};

Tick.prototype.cancelTimer = function () {
  if (this.handle !== null) {
    clearInterval(this.handle);
    this.handle = null;
  }
};

Tick.prototype.handleEvent = function (e) {
  if (e.type === 'stateChange' && e.patch.hasRemoved()) {
    Dataspace.exit();
  }
};

function Alarm(deadlineMS) {
  this.deadlineMS = deadlineMS;
  this.handle = null;
}

Alarm.prototype.boot = function () {
  this.checkTimeout();

  return Patch.sub(Patch.observe(timeLaterThan(this.deadlineMS))) // monitor interest
    .andThen(Patch.pub(timeLaterThan(this.deadlineMS))) // signal we exist to DemandMatcher
  ;
};

Alarm.prototype.checkTimeout = function () {
  var now = +(new Date());
  var delta = this.deadlineMS - now;
  if (delta <= 0) {
    Dataspace.stateChange(Patch.assert(timeLaterThan(this.deadlineMS)));
    this.cancelTimer();
  } else if (this.handle === null) {
    var self = this;
    this.handle = setTimeout(Dataspace.wrap(function () { self.checkTimeout(); }), delta);
  }
};

Alarm.prototype.cancelTimer = function () {
  if (this.handle !== null) {
    clearTimeout(this.handle);
    this.handle = null;
  }
};

Alarm.prototype.trapexit = function () {
  this.cancelTimer();
};

Alarm.prototype.handleEvent = function (e) {
  if (e.type === 'stateChange' && e.patch.hasRemoved()) {
    Dataspace.exit();
  }
};

///////////////////////////////////////////////////////////////////////////

module.exports.spawnTimerDriver = spawnTimerDriver;
module.exports.periodicTick = periodicTick;
module.exports.timeLaterThan = timeLaterThan;
module.exports.Tick = Tick;
module.exports.Alarm = Alarm;
