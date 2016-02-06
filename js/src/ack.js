// Utility protocol for measuring when a stateChange takes effect.

var RandomID = require('./randomid.js');
var Syndicate = require('./syndicate.js');
var Route = require('./route.js');
var Patch = require('./patch.js');

var $Ack = new Route.$Special('ack');

function Ack(metaLevel, id) {
  this.metaLevel = metaLevel || 0;
  this.id = id || RandomID.randomId(16);
  this.done = false;
}

Ack.prototype.arm = function () {
  Syndicate.Network.stateChange(Patch.sub([$Ack, this.id], this.metaLevel));
  Syndicate.Network.send([$Ack, this.id], this.metaLevel);
};

Ack.prototype.disarm = function () {
  Syndicate.Network.stateChange(Patch.unsub([$Ack, this.id], this.metaLevel));
};

Ack.prototype.check = function (e) {
  if (!this.done) {
    if (e.type === 'message') {
      var m = Patch.stripAtMeta(e.message, this.metaLevel);
      if (m && m[0] === $Ack && m[1] === this.id) {
	this.disarm();
	this.done = true;
      }
    }
  }
  return this.done;
};

///////////////////////////////////////////////////////////////////////////

module.exports.$Ack = $Ack;
module.exports.Ack = Ack;
