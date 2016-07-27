"use strict";
// Utilities for Maps of Sets

var Immutable = require('immutable');

function add(ms, key, val) {
  return ms.set(key, (ms.get(key) || Immutable.Set()).add(val));
}

function remove(ms, key, val) {
  var oldSet = ms.get(key);
  if (oldSet) {
    var newSet = oldSet.remove(val);
    if (newSet.isEmpty()) {
      ms = ms.remove(key);
    } else {
      ms = ms.set(key, newSet);
    }
  }
  return ms;
}

///////////////////////////////////////////////////////////////////////////

module.exports.add = add;
module.exports.remove = remove;
