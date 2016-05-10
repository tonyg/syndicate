// Utility protocol for measuring when a stateChange takes effect.

var RandomID = require('./randomid.js');
var Dataspace = require('./dataspace.js').Dataspace;
var Struct = require('./struct.js');
var Patch = require('./patch.js');

var ack = Struct.makeConstructor('ack', ['id']);

function Ack(metaLevel, id) {
  this.metaLevel = metaLevel || 0;
  this.id = id || RandomID.randomId(16);
  this.done = false;
}

Ack.prototype.arm = function () {
  Dataspace.stateChange(Patch.sub(ack(this.id), this.metaLevel));
  Dataspace.send(ack(this.id), this.metaLevel);
};

Ack.prototype.disarm = function () {
  Dataspace.stateChange(Patch.unsub(ack(this.id), this.metaLevel));
};

Ack.prototype.check = function (e) {
  if (!this.done) {
    if (e.type === 'message') {
      var m = Patch.stripAtMeta(e.message, this.metaLevel);
      if (ack.isClassOf(m) && m[0] === this.id) {
	this.disarm();
	this.done = true;
      }
    }
  }
  return this.done;
};

///////////////////////////////////////////////////////////////////////////

module.exports.ack = ack;
module.exports.Ack = Ack;
