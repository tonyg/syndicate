"use strict";
// Wire protocol representation of events and actions

var Trie = Syndicate.Trie;
var Struct = Syndicate.Struct;

function _encode(e) {
  switch (e.type) {
    case "stateChange":
      return ["patch", e.patch.toJSON()];
    case "message":
      return ["message", e.message];
  }
}

function _decode(what) {
  return function (j) {
    switch (j[0]) {
      case "patch":
        return Syndicate.stateChange(Patch.fromJSON(j[1]));
      case "message":
        return Syndicate.message(j[1]);
      default:
        throw new Error("Invalid JSON-encoded " + what + ": " + JSON.stringify(j));
    }
  };
}

///////////////////////////////////////////////////////////////////////////

// module.exports.encodeEvent = _encode;
// module.exports.decodeEvent = _decode("event");
// module.exports.encodeAction = _encode;
// module.exports.decodeAction = _decode("action");

var Codec = {
  encodeEvent: _encode,
  decodeEvent: _decode("event"),
  encodeAction: _encode,
  decodeAction: _decode("action")
};
