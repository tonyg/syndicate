"use strict";
// "Structures": Simple named-tuple-like records.

var Immutable = require("immutable");
var $Special = require('./special.js');

/* Defined here rather than in trie.js because we need it in makeConstructor. */
var __ = new $Special("wildcard"); /* wildcard marker */

function StructureType(label, arity) {
  this.label = label;
  this.arity = arity;
  this.pattern = this.instantiate(Immutable.Repeat(__, arity).toArray());

  var self = this;
  this.ctor = function () {
    return self.instantiate(Array.prototype.slice.call(arguments));
  };
  this.ctor.meta = this;
  this.ctor.pattern = this.pattern;
  this.ctor.isClassOf = function (v) { return self.isClassOf(v); };
}

function makeConstructor(label, fieldNames) {
  return new StructureType(label, fieldNames.length).ctor;
}

StructureType.prototype.equals = function (other) {
  if (!(other instanceof StructureType)) return false;
  return this.arity === other.arity && this.label === other.label;
};

StructureType.prototype.instantiate = function (fields) {
  return new Structure(this, fields);
};

StructureType.prototype.isClassOf = function (v) {
  return v && (v instanceof Structure) && (v.meta.equals(this));
};

function Structure(meta, fields) {
  if (!isStructureType(meta)) {
    throw new Error("Structure: requires structure type");
  }
  if (fields.length !== meta.arity) {
    throw new Error("Structure: cannot instantiate meta "+JSON.stringify(meta.label)+
                    " expecting "+meta.arity+" fields with "+fields.length+" fields");
  }
  this.meta = meta;
  this.length = meta.arity;
  this.fields = fields.slice(0);
  for (var i = 0; i < fields.length; i++) {
    this[i] = fields[i];
  }
}

function reviveStructs(j) {
  if (Array.isArray(j)) {
    return j.map(reviveStructs);
  }

  if ((j !== null) && typeof j === 'object') {
    if ((typeof j['@type'] === 'string') && Array.isArray(j['fields'])) {
      return (new StructureType(j['@type'], j['fields'].length)).instantiate(j['fields']);
    } else {
      for (var k in j) {
        if (Object.prototype.hasOwnProperty.call(j, k)) {
          j[k] = reviveStructs(j[k]);
        }
      }
      return j;
    }
  }

  return j;
}

function reviver(k, v) {
  if (k === '') {
    return reviveStructs(v);
  }
  return v;
};

Structure.prototype.toJSON = function () {
  return { '@type': this.meta.label, 'fields': this.fields };
};

function isStructureType(v) {
  return v && (v instanceof StructureType);
}

function isStructure(v) {
  return v && (v instanceof Structure);
}

///////////////////////////////////////////////////////////////////////////

module.exports.__ = __;
module.exports.StructureType = StructureType;
module.exports.makeConstructor = makeConstructor;
module.exports.Structure = Structure;
module.exports.reviveStructs = reviveStructs;
module.exports.reviver = reviver;
module.exports.isStructureType = isStructureType;
module.exports.isStructure = isStructure;
