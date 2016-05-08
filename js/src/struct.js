"use strict";
// "Structures": Simple named-tuple-like records.
// TODO: shore up $SyndicateMeta$, making it a proper object

var Immutable = require("immutable");
var $Special = require('./special.js');

/* Defined here rather than in trie.js because we need it in makeStructureConstructor. */
var __ = new $Special("wildcard"); /* wildcard marker */

function instantiateStructure($SyndicateMeta$, argvals) {
  var result = {"$SyndicateMeta$": $SyndicateMeta$};
  var argnames = $SyndicateMeta$.arguments;
  for (var i = 0; i < argnames.length; i++) {
    result[argnames[i]] = argvals[i];
  }
  return result;
}

function makeStructureConstructor(label, argumentNames) {
  var $SyndicateMeta$ = {
    label: label,
    arguments: argumentNames
  };
  var ctor = function() {
    return instantiateStructure($SyndicateMeta$, arguments);
  };
  ctor.meta = $SyndicateMeta$;
  ctor.isClassOf = function (v) { return v && v.$SyndicateMeta$ === $SyndicateMeta$; };
  ctor.pattern = ctor.apply(null, Immutable.Repeat(__, argumentNames.length).toArray());
  return ctor;
}

function isSyndicateMeta(m) {
  // TODO: include more structure in $SyndicateMeta$ objects to make
  // this judgement less sloppy.
  return m && m.label && Array.isArray(m.arguments);
}

function isStructure(s) {
  return (s !== null) && (typeof s === 'object') && ("$SyndicateMeta$" in s);
}

function structureToArray(s, excludeLabel) {
  var result = excludeLabel ? [] : [s.$SyndicateMeta$.label];
  var args = s.$SyndicateMeta$.arguments;
  for (var i = 0; i < args.length; i++) {
    result.push(s[args[i]]);
  }
  return result;
}

///////////////////////////////////////////////////////////////////////////

module.exports.__ = __;
module.exports.instantiateStructure = instantiateStructure;
module.exports.makeStructureConstructor = makeStructureConstructor;
module.exports.isSyndicateMeta = isSyndicateMeta;
module.exports.isStructure = isStructure;
module.exports.structureToArray = structureToArray;
