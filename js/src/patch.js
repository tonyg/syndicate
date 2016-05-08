"use strict";

var Trie = require("./trie.js");
var Struct = require("./struct.js");
var Immutable = require("immutable");

var __ = Trie.__;
var _$ = Trie._$;

function Patch(added, removed) {
  this.added = added;
  this.removed = removed;
}

var emptyPatch = new Patch(Trie.emptyTrie, Trie.emptyTrie);
var removeEverythingPatch = new Patch(Trie.emptyTrie, Trie.compilePattern(true, __));
var trueLabel = Trie.trieSuccess(true);

var observe = Struct.makeStructureConstructor('observe', ['assertion']);
var atMeta = Struct.makeStructureConstructor('atMeta', ['assertion']);
var advertise = Struct.makeStructureConstructor('advertise', ['assertion']);

function prependAtMeta(p, level) {
  while (level--) {
    p = atMeta(p);
  }
  return p;
}

function stripAtMeta(p, level) {
  while (level--) {
    if (atMeta.isClassOf(p)) {
      p = p.assertion;
    } else {
      return null;
    }
  }
  return p;
}

function observeAtMeta(p, level) {
  if (level === 0) {
    return Trie.compilePattern(true, observe(p));
  } else {
    return Trie._union(
      Trie.compilePattern(true, observe(prependAtMeta(p, level))),
      Trie.compilePattern(true, atMeta(Trie.embeddedTrie(observeAtMeta(p, level - 1)))));
  }
}

function _check(p) {
  if (p instanceof Patch) {
    throw new Error("Cannot construct patch pattern using an embedded patch");
  }
  return p;
}

function assert(p, metaLevel) {
  return new Patch(Trie.compilePattern(true, prependAtMeta(_check(p), metaLevel || 0)),
		   Trie.emptyTrie);
}

function retract(p, metaLevel) {
  return new Patch(Trie.emptyTrie,
		   Trie.compilePattern(true, prependAtMeta(_check(p), metaLevel || 0)));
}

function sub(p, metaLevel) {
  return new Patch(observeAtMeta(_check(p), metaLevel || 0), Trie.emptyTrie);
}

function unsub(p, metaLevel) {
  return new Patch(Trie.emptyTrie, observeAtMeta(_check(p), metaLevel || 0));
}

function pub(p, metaLevel) {
  return assert(advertise(_check(p)), metaLevel);
}

function unpub(p, metaLevel) {
  return retract(advertise(_check(p)), metaLevel);
}

///////////////////////////////////////////////////////////////////////////

Patch.prototype.equals = function (other) {
  if (!(other instanceof Patch)) return false;
  return Immutable.is(this.added, other.added) && Immutable.is(this.removed, other.removed);
};

Patch.prototype.isEmpty = function () {
  return this.added === Trie.emptyTrie && this.removed === Trie.emptyTrie;
};

Patch.prototype.isNonEmpty = function () {
  return !this.isEmpty();
};

Patch.prototype.hasAdded = function () {
  return this.added !== Trie.emptyTrie;
};

Patch.prototype.hasRemoved = function () {
  return this.removed !== Trie.emptyTrie;
};

Patch.prototype.lift = function () {
  return new Patch(Trie.compilePattern(true, atMeta(Trie.embeddedTrie(this.added))),
		   Trie.compilePattern(true, atMeta(Trie.embeddedTrie(this.removed))));
};

var atMetaProj = atMeta(_$);
Patch.prototype.drop = function () {
  return new Patch(Trie.project(this.added, atMetaProj),
		   Trie.project(this.removed, atMetaProj));
};

Patch.prototype.strip = function () {
  return new Patch(Trie.relabel(this.added, function (v) { return true; }),
		   Trie.relabel(this.removed, function (v) { return true; }));
};

Patch.prototype.label = function (labelValue) {
  return new Patch(Trie.relabel(this.added, function (v) { return labelValue; }),
		   Trie.relabel(this.removed, function (v) { return labelValue; }));
};

Patch.prototype.limit = function (bound) {
  return new Patch(Trie.subtract(this.added, bound, function (v1, v2) { return Trie.emptyTrie; }),
		   Trie.intersect(this.removed, bound,
                                  function (v1, v2) { return Trie.trieSuccess(v1); }));
};

var metaLabelSet = Immutable.Set(["meta"]);
Patch.prototype.computeAggregate = function (label, base, removeMeta /* optional flag */) {
  return new Patch(Trie.subtract(this.added, base, addCombiner),
		   Trie.subtract(this.removed, base, removeCombiner));

  function addCombiner(v1, v2) {
    if (removeMeta && Immutable.is(v2, metaLabelSet)) {
      return Trie.trieSuccess(v1);
    } else {
      return Trie.emptyTrie;
    }
  }

  function removeCombiner(v1, v2) {
    if (v2.size === 1) {
      return Trie.trieSuccess(v1);
    } else {
      if (removeMeta && v2.size === 2 && v2.has("meta")) {
	return Trie.trieSuccess(v1);
      } else {
	return Trie.emptyTrie;
      }
    }
  }
};

Patch.prototype.applyTo = function (base) {
  return Trie._union(Trie.subtract(base, this.removed), this.added);
};

Patch.prototype.updateInterests = function (base) {
  return Trie._union(Trie.subtract(base,
                                   this.removed,
                                   function (v1, v2) { return Trie.emptyTrie; }),
		     this.added,
		     function (v1, v2) { return trueLabel; });
};

Patch.prototype.unapplyTo = function (base) {
  return Trie._union(Trie.subtract(base, this.added), this.removed);
};

Patch.prototype.andThen = function (nextPatch) {
  return new Patch(nextPatch.updateInterests(this.added),
		   Trie._union(Trie.subtract(this.removed,
					     nextPatch.added,
					     function (v1, v2) { return Trie.emptyTrie; }),
			       nextPatch.removed,
			       function (v1, v2) { return trueLabel; }));
};

function patchSeq(/* patch, patch, ... */) {
  var p = emptyPatch;
  for (var i = 0; i < arguments.length; i++) {
    p = p.andThen(arguments[i]);
  }
  return p;
}

function computePatch(oldBase, newBase) {
  return new Patch(Trie.subtract(newBase, oldBase),
		   Trie.subtract(oldBase, newBase));
}

function biasedIntersection(object, subject) {
  subject = Trie.trieStep(subject, observe.meta.arguments.length, observe.meta);
  return Trie.intersect(object, subject, function (v1, v2) { return Trie.trieSuccess(v1); });
}

Patch.prototype.viewFrom = function (interests) {
  return new Patch(biasedIntersection(this.added, interests),
		   biasedIntersection(this.removed, interests));
};

Patch.prototype.unsafeUnion = function (other) {
  // Unsafe because does not necessarily preserve invariant that added
  // and removed are disjoint.
  return new Patch(Trie._union(this.added, other.added),
		   Trie._union(this.removed, other.removed));
};

Patch.prototype.project = function (compiledProjection) {
  return new Patch(Trie.project(this.added, compiledProjection),
		   Trie.project(this.removed, compiledProjection));
};

Patch.prototype.projectObjects = function (compiledProjection) {
  return [Trie.projectObjects(this.added, compiledProjection),
	  Trie.projectObjects(this.removed, compiledProjection)];
};

Patch.prototype.pretty = function () {
  return ("<<<<<<<< Removed:\n" + Trie.prettyTrie(this.removed) + "\n" +
	  "======== Added:\n" + Trie.prettyTrie(this.added) + "\n" +
	  ">>>>>>>>");
}

///////////////////////////////////////////////////////////////////////////

module.exports.Patch = Patch;
module.exports.emptyPatch = emptyPatch;
module.exports.removeEverythingPatch = removeEverythingPatch;

module.exports.observe = observe;
module.exports.atMeta = atMeta;
module.exports.advertise = advertise;

module.exports.prependAtMeta = prependAtMeta;
module.exports.stripAtMeta = stripAtMeta;
module.exports.observeAtMeta = observeAtMeta;
module.exports.assert = assert;
module.exports.retract = retract;
module.exports.sub = sub;
module.exports.unsub = unsub;
module.exports.pub = pub;
module.exports.unpub = unpub;

module.exports.patchSeq = patchSeq;
module.exports.computePatch = computePatch;
module.exports.biasedIntersection = biasedIntersection;
