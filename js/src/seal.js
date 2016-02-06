function Seal(contents) {
  this.sealContents = contents;
}

Seal.prototype.equals = function (other) {
  if (!(other instanceof Seal)) return false;
  return Immutable.is(this.sealContents, other.sealContents);
};

module.exports.Seal = Seal;
