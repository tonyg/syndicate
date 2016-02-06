var Immutable = require('immutable');

function Seal(contents) {
  this.sealContents = contents;
  Object.freeze(this);
}

Seal.prototype.equals = function (other) {
  if (!(other instanceof Seal)) return false;
  return Immutable.is(this.sealContents, other.sealContents);
};

function seal(contents) {
  return new Seal(contents);
}

module.exports.Seal = Seal;
module.exports.seal = seal;
