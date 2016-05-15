"use strict";

var Immutable = require("immutable");
var Struct = require('./struct.js');
var $Special = require('./special.js');

function die(message) {
  throw new Error(message);
}

///////////////////////////////////////////////////////////////////////////
// Trie representations
//
// A Trie is one of
//  - emptyTrie
//  - an instance of $Success
//  - an instance of $Branch

var emptyTrie = new $Special("Mt");

function is_emptyTrie(m) {
  return m === emptyTrie;
}

function $Success(value) {
  this.value = value;
}

$Success.prototype.equals = function (other) {
  if (!(other instanceof $Success)) return false;
  return Immutable.is(this.value, other.value);
};

function $Branch(wild, edges, count) {
  this.wild = wild || emptyTrie; // Trie
  this.edges = edges || Immutable.Map(); // Map from arity to Map from key-like-thing to Trie
  // (Sigmas are 0-ary.)
  this.count = count || 0;
}

$Branch.prototype.equals = function (other) {
  if (!(other instanceof $Branch)) return false;
  return (this.count === other.count) && Immutable.is(this.wild, other.wild) && Immutable.is(this.edges, other.edges);
};

///////////////////////////////////////////////////////////////////////////
// Patterns, projections and captures

var __ = Struct.__; /* imported rather than defined here because of cyclic dep in Struct */
var SOA = new $Special("array"); /* key for start-of-array */

function $Embedded(trie, arrayLength) {
  this.trie = trie;
  this.arrayLength = arrayLength;
}

function embeddedTrie(trie) {
  return new $Embedded(trie, null);
}

function embeddedTrieArray(trie, arrayLength) {
  return new $Embedded(trie, arrayLength);
}

// The name argument should be a string or null; it defaults to null.
// The pattern argument defaults to wildcard, __.
function $Capture(name, pattern) {
  this.name = name || null;
  this.pattern = (typeof pattern === 'undefined' ? __ : pattern);
}

// Abbreviation: _$(...) <==> new $Capture(...)
function _$(name, pattern) {
  return new $Capture(name, pattern);
}

function isCapture(x) { return x instanceof $Capture || x === _$; }
function captureName(x) { return x instanceof $Capture ? x.name : null; }
function capturePattern(x) { return x instanceof $Capture ? x.pattern : __; }

///////////////////////////////////////////////////////////////////////////
// Constructors

function rsuccess(v) {
  return new $Success(v);
}

function rseq(arity, key, r) {
  if (is_emptyTrie(r)) return emptyTrie;
  return new $Branch(emptyTrie, Immutable.Map.of(arity, Immutable.Map.of(key, r)), 1);
}

function rwild(r) {
  if (is_emptyTrie(r)) return emptyTrie;
  return new $Branch(r, Immutable.Map(), 0);
}

function rcopybranch(r) {
  return new $Branch(r.wild, r.edges, r.count);
}

function prepend_wilds(n, r) {
  while (n-- > 0) { r = rwild(r); }
  return r;
}

// true iff r1 could have been the output of prepend_wilds(n, r2).
function equal_upto_wilds(n, r1, r2) {
  while (true) {
    if (n === 0 || is_emptyTrie(r1)) {
      return Immutable.is(r1, r2);
    }
    if (!(r1 instanceof $Branch) || r1.count > 0) {
      return false;
    }
    n = n - 1;
    r1 = r1.wild;
  }
}

function rupdate_inplace(r, arity, key, k) {
  if (equal_upto_wilds(arity, k, r.wild)) {
    var m = r.edges.get(arity);
    if (!m) return;
    if (m.has(key)) r.count--;
    m = m.remove(key);
    r.edges = m.isEmpty() ? r.edges.remove(arity) : r.edges.set(arity, m);
  } else {
    var m = r.edges.get(arity) || Immutable.Map();
    if (!m.has(key)) r.count++;
    r.edges = r.edges.set(arity, m.set(key, k));
  }
}

function rlookup(r, arity, key) {
  var m = r.edges.get(arity);
  m = m && m.get(key);
  return m || prepend_wilds(arity, r.wild);
}

///////////////////////////////////////////////////////////////////////////

function collapse(r) {
  if ((r instanceof $Branch) && is_emptyTrie(r.wild) && (r.count === 0)) {
    return emptyTrie;
  } else {
    return r;
  }
}

// use with extreme care
function newEmptyBranch() {
  return new $Branch(emptyTrie, Immutable.Map(), 0);
}

var canonicalExpandedEmpty = newEmptyBranch();
function expand(r) {
  if (is_emptyTrie(r)) {
    return canonicalExpandedEmpty;
  } else {
    return r;
  }
}

///////////////////////////////////////////////////////////////////////////

function compilePattern(v, p) {
  if (!p) die("compilePattern: missing pattern");
  return walk(p, rsuccess(v));

  function walk(p, acc) {
    if (p === __) return rwild(acc);

    if (Array.isArray(p)) {
      for (var i = p.length - 1; i >= 0; i--) {
	acc = walk(p[i], acc);
      }
      return rseq(p.length, SOA, acc);
    }

    if (Immutable.List.isList(p)) {
      p.reverse().forEach(function (element) {
	acc = walk(element, acc);
      });
      return rseq(p.size, SOA, acc);
    }

    if (Struct.isStructure(p)) {
      for (var i = p.meta.arity - 1; i >= 0; i--) {
        acc = walk(p[i], acc);
      }
      return rseq(p.meta.arity, p.meta, acc);
    }

    if (p instanceof $Embedded) {
      acc = appendTrie(p.trie, function (v) { return acc; });
      if (p.arrayLength !== null) {
        acc = rseq(p.arrayLength, SOA, acc);
      }
      return acc;
    } else {
      return rseq(0, p, acc);
    }
  }
}

function matchPattern(v, p) {
  var captureCount = 0;
  var result = {};
  try {
    walk(v, p);
  } catch (e) {
    if (e.matchPatternFailed) return null;
    throw e;
  }
  result.length = captureCount;
  return result;

  function walk(v, p) {
    if (p === v) return;

    if (p === __) return;

    if (Struct.isStructure(p) && Struct.isStructure(v) && (p.meta.equals(v.meta)))
    {
      for (var i = 0; i < p.meta.arity; i++) {
        walk(v[i], p[i]);
      }
      return;
    }

    if (Array.isArray(p) && Array.isArray(v) && p.length === v.length) {
      for (var i = 0; i < p.length; i++) {
        walk(v[i], p[i]);
      }
      return;
    }

    if (isCapture(p)) {
      var thisCapture = captureCount++;
      walk(v, capturePattern(p));
      result[captureName(p) || ('$' + thisCapture)] = v;
      return;
    }

    if (p instanceof $Embedded) {
      die("$Embedded patterns not supported in matchPattern()");
    }

    throw {matchPatternFailed: true};
  }
}

///////////////////////////////////////////////////////////////////////////

function combine(combineSuccess, leftEmpty, rightEmpty, leftBase, rightBase, r1, r2) {
  return walk(r1, r2);

  function walk(r1, r2) {
    if (is_emptyTrie(r1)) return collapse(leftEmpty(r2));
    if (is_emptyTrie(r2)) return collapse(rightEmpty(r1));

    if ((r1 instanceof $Success) || (r2 instanceof $Success)) {
      return collapse(combineSuccess(r1, r2));
    }

    if (!(r1 instanceof $Branch) || !(r2 instanceof $Branch)) {
      die("Invalid trie given to combine");
    }

    /* fold-over-keys */
    var w = walk(r1.wild, r2.wild);

    var acc;
    if (!is_emptyTrie(r1.wild) && !is_emptyTrie(r2.wild)) {
      acc = rcopybranch(expand(rwild(w)));
      var seen = Immutable.Map();
      r1.edges.forEach(function (keymap, arity) {
        keymap.forEach(function (r1v, key) {
          var r2v = rlookup(r2, arity, key);
          rupdate_inplace(acc, arity, key, walk(r1v, r2v));
          seen = seen.set(arity, (seen.get(arity) || Immutable.Set()).add(key));
        });
      });
      r2.edges.forEach(function (keymap, arity) {
        keymap.forEach(function (r2v, key) {
          var r1v = rlookup(r1, arity, key);
          var s = seen.get(arity);
          s = s && s.has(key);
          if (!s) rupdate_inplace(acc, arity, key, walk(r1v, r2v));
        });
      });
    } else if (!is_emptyTrie(r1.wild) || (is_emptyTrie(r2.wild) && (r1.count > r2.count))) {
      acc = rcopybranch(expand(leftBase(r1)));
      acc.wild = w;
      r2.edges.forEach(function (keymap, arity) {
        keymap.forEach(function (r2v, key) {
          var r1v = rlookup(r1, arity, key);
          rupdate_inplace(acc, arity, key, walk(r1v, r2v));
        });
      });
    } else {
      acc = rcopybranch(expand(rightBase(r2)));
      acc.wild = w;
      r1.edges.forEach(function (keymap, arity) {
        keymap.forEach(function (r1v, key) {
          var r2v = rlookup(r2, arity, key);
          rupdate_inplace(acc, arity, key, walk(r1v, r2v));
        });
      });
    }

    return collapse(acc);
  }
}

function asymmetricTrieError(r1, r2) {
  die("Asymmetric tries: " + r1 + ", " + r2);
}

///////////////////////////////////////////////////////////////////////////

var unionSuccessesDefault = function (v1, v2) {
  return rsuccess(v1.union(v2));
};

function union(o1, o2, unionSuccessesOpt) {
  var unionSuccesses = unionSuccessesOpt || unionSuccessesDefault;
  return combine(unionCombiner,
                 function (x) { return x; },
                 function (x) { return x; },
                 function (x) { return x; },
                 function (x) { return x; },
                 o1,
                 o2);

  function unionCombiner(r1, r2) {
    if ((r1 instanceof $Success) && (r2 instanceof $Success))
      return unionSuccesses(r1.value, r2.value);
    if (is_emptyTrie(r1)) return r2;
    if (is_emptyTrie(r2)) return r1;
    asymmetricTrieError(r1, r2);
  }
}

function unionN() {
  var acc = emptyTrie;
  for (var i = 0; i < arguments.length; i++) {
    acc = union(acc, arguments[i]);
  }
  return acc;
}

///////////////////////////////////////////////////////////////////////////

var intersectSuccessesDefault = unionSuccessesDefault;

function intersect(o1, o2, intersectSuccessesOpt) {
  var intersectSuccesses = intersectSuccessesOpt || intersectSuccessesDefault;
  return combine(intersectCombiner,
                 function (x) { return emptyTrie; },
                 function (x) { return emptyTrie; },
                 function (x) { return emptyTrie; },
                 function (x) { return emptyTrie; },
                 o1,
                 o2);

  function intersectCombiner(r1, r2) {
    if ((r1 instanceof $Success) && (r2 instanceof $Success))
      return intersectSuccesses(r1.value, r2.value);
    if (is_emptyTrie(r1)) return emptyTrie;
    if (is_emptyTrie(r2)) return emptyTrie;
    asymmetricTrieError(r1, r2);
  }
}

///////////////////////////////////////////////////////////////////////////

var subtractSuccessesDefault = function (v1, v2) {
  var r = v1.subtract(v2);
  if (r.isEmpty()) return emptyTrie;
  return rsuccess(r);
};

function subtract(o1, o2, subtractSuccessesOpt) {
  var subtractSuccesses = subtractSuccessesOpt || subtractSuccessesDefault;
  return combine(subtractCombiner,
                 function (x) { return emptyTrie; },
                 function (x) { return x; },
                 function (x) { return x; },
                 function (x) { return emptyTrie; },
                 o1,
                 o2);

  function subtractCombiner(r1, r2) {
    if ((r1 instanceof $Success) && (r2 instanceof $Success))
      return subtractSuccesses(r1.value, r2.value);
    if (is_emptyTrie(r1)) return emptyTrie;
    if (is_emptyTrie(r2)) return r1;
    asymmetricTrieError(r1, r2);
  }
}

///////////////////////////////////////////////////////////////////////////

// Returns failureResult on failed match, otherwise the appropriate success
// value contained in the trie r.
function matchValue(r, v, failureResultOpt) {
  var failureResult = failureResultOpt || null;

  var vs = Immutable.List.of(v);

  while (!is_emptyTrie(r)) {
    if (r instanceof $Success) {
      return vs.isEmpty() ? r.value : failureResult;
    }

    if (vs.isEmpty()) return failureResult;
    var v = vs.first();
    vs = vs.shift();

    if (typeof v === 'string' && v.substring(0, 2) === '__') {
      die("Cannot match special string starting with __");
    }

    if (Array.isArray(v)) {
      r = rlookup(r, v.length, SOA);
      vs = Immutable.List(v).concat(vs);
    } else if (Struct.isStructure(v)) {
      r = rlookup(r, v.meta.arity, v.meta);
      vs = Immutable.List(v.fields).concat(vs);
    } else {
      r = rlookup(r, 0, v);
    }
  }

  return failureResult;
}

function matchTrie(o1, o2, seed, combiner) {
  var acc = typeof seed === 'undefined' ? Immutable.Set() : seed; // variable updated imperatively
  walk(o1, o2);
  return acc;

  function walk(r1, r2) {
    if (is_emptyTrie(r1) || is_emptyTrie(r2)) return;

    if ((r1 instanceof $Success) && (r2 instanceof $Success)) {
      acc = combiner(r1.value, r2.value, acc);
      return;
    }

    if (!(r1 instanceof $Branch) || !(r2 instanceof $Branch)) {
      asymmetricTrieError(r1, r2);
    }

    walk(r1.wild, r2.wild);

    function examineKeys(keymap, arity) {
      keymap.forEach(function (_val, key) {
        walk(rlookup(r1, arity, key), rlookup(r2, arity, key));
      });
    }

    if (is_emptyTrie(r1.wild)) {
      if (is_emptyTrie(r2.wild)) {
	(r1.count < r2.count ? r1 : r2).edges.forEach(examineKeys);
      } else {
	r1.edges.forEach(examineKeys);
      }
    } else {
      if (is_emptyTrie(r2.wild)) {
	r2.edges.forEach(examineKeys);
      } else {
	r1.edges.forEach(examineKeys);
	r2.edges.forEach(examineKeys);
      }
    }
  }
}

function appendTrie(m, mTailFn) {
  return walk(m);

  function walk(m) {
    if (is_emptyTrie(m)) return emptyTrie;
    if (m instanceof $Success) return mTailFn(m.value);

    var target = newEmptyBranch();
    target.wild = walk(m.wild);
    m.edges.forEach(function (keymap, arity) {
      keymap.forEach(function (k, key) {
        rupdate_inplace(target, arity, key, walk(k));
      });
    });
    return collapse(target);
  }
}

// DANGEROUS: prefer subtract() instead.
//
// function triePruneBranch(m, arityKeys) {
//   if (arityKeys.isEmpty()) return emptyTrie;
//   if (!(m instanceof $Branch)) return m;
//   var arityKey = arityKeys.first();
//   var rest = arityKeys.shift();
//   var arity = arityKey[0];
//   var key = arityKey[1];
//   m = rcopybranch(m);
//   rupdate_inplace(m, arity, key, triePruneBranch(rlookup(m, arity, key), rest));
//   return collapse(m);
// }

function trieStep(m, arity, key) {
  if (typeof key === 'undefined') {
    // Cope with API change which would otherwise silently cause problems
    die("trieStep: missing 'key' argument");
  }
  if (is_emptyTrie(m)) return emptyTrie;
  if (m instanceof $Success) return emptyTrie;
  return rlookup(m, arity, key);
}

function relabel(m, f) {
  return appendTrie(m, function (v) {
    var v1 = f(v);
    return v1 ? rsuccess(v1) : emptyTrie;
  });
}

///////////////////////////////////////////////////////////////////////////

function projectionNames(p) {
  var names = [];
  walk(p);
  return names;

  function walk(p) {
    if (isCapture(p)) {
      names.push(captureName(p));
      walk(capturePattern(p));
      return;
    }

    if (Array.isArray(p)) {
      for (var i = 0; i < p.length; i++) walk(p[i]);
      return;
    }

    if (Struct.isStructure(p)) {
      for (var i = 0; i < p.meta.arity; i++) walk(p[i]);
      return;
    }
  }
}

function projectionArity(p) {
  return projectionNames(p).length;
}

function projectionToPattern(p) {
  return walk(p);

  function walk(p) {
    if (isCapture(p)) return walk(capturePattern(p));

    if (Array.isArray(p)) {
      var result = [];
      for (var i = 0; i < p.length; i++) {
	result.push(walk(p[i]));
      }
      return result;
    }

    if (Struct.isStructure(p)) {
      var resultFields = [];
      for (var i = 0; i < p.meta.arity; i++) {
        resultFields[i] = walk(p[i]);
      }
      return new Struct.Structure(p.meta, resultFields);
    }

    return p;
  }
}

function project(t, wholeSpec, projectSuccessOpt, combinerOpt) {
  return projectMany(t, Immutable.List.of(wholeSpec), projectSuccessOpt, combinerOpt);
}

function projectMany(t, wholeSpecs, projectSuccessOpt, combinerOpt) {
  var projectSuccess = projectSuccessOpt || rsuccess;
  var combiner = combinerOpt || unionSuccessesDefault;

  return walk(false, t, Immutable.List(wholeSpecs), function (t) {
    if (t instanceof $Success) {
      return projectSuccess(t.value);
    } else {
      return emptyTrie;
    }
  });

  function walk(isCapturing, t, specs, kont) {
    if (specs.isEmpty()) return kont(t);
    if (!(t instanceof $Branch)) return emptyTrie;

    var spec = specs.first();
    var specsRest = specs.rest();

    if (isCapture(spec)) {
      if (isCapturing) {
        die("projectMany: nested capture in projection: " + wholeSpecs);
      }
      return walk(true, t, Immutable.List.of(capturePattern(spec)), function (intermediate) {
        return walk(false, intermediate, specsRest, kont);
      });
    }

    if (spec === __) {
      if (isCapturing) {
        var target = newEmptyBranch();
        target.wild = walk(isCapturing, t.wild, specsRest, kont);
        t.edges.forEach(function (keymap, arity) {
          var innerSpecs = Immutable.Repeat(__, arity);
          keymap.forEach(function (k, key) {
            rupdate_inplace(target, arity, key,
                            walk(isCapturing, k, innerSpecs, function (intermediate) {
                              return walk(isCapturing, intermediate, specsRest, kont);
                            }));
          });
        });
        return collapse(target);
      } else {
        var seed = walk(isCapturing, t.wild, specsRest, kont);
        t.edges.forEach(function (keymap, arity) {
          var innerSpecs = Immutable.Repeat(__, arity);
          keymap.forEach(function (k, key) {
            seed = union(seed,
                         walk(isCapturing, k, innerSpecs, function (intermediate) {
                           return walk(isCapturing, intermediate, specsRest, kont);
                         }),
                         combiner);
          });
        });
        return seed;
      }
      return target;
    }

    if (Struct.isStructure(spec)) {
      var intermediate = walk(isCapturing,
                              rlookup(t, spec.meta.arity, spec.meta),
                              Immutable.List(spec.fields),
                              function (intermediate) {
                                return walk(isCapturing, intermediate, specsRest, kont);
                              });
      return isCapturing ? rseq(spec.meta.arity, spec.meta, intermediate) : intermediate;
    }

    if (Array.isArray(spec)) {
      var intermediate = walk(isCapturing,
                              rlookup(t, spec.length, SOA),
                              Immutable.List(spec),
                              function (intermediate) {
                                return walk(isCapturing, intermediate, specsRest, kont);
                              });
      return isCapturing ? rseq(spec.length, SOA, intermediate) : intermediate;
    }

    if (spec instanceof $Embedded) {
      die("$Embedded patterns not supported in projectMany()");
    }

    /* It is a normal atom */
    var intermediate = walk(isCapturing, rlookup(t, 0, spec), specsRest, kont);
    return isCapturing ? rseq(0, spec, intermediate) : intermediate;
  }
}

function reconstructSequence(key, items) {
  if (key === SOA) {
    return items.toArray();
  } else {
    return key.instantiate(items.toArray());
  }
}

function trieKeys(m, takeCount0) {
  if (typeof takeCount0 !== 'number') {
    // Cope with API change which would otherwise silently cause problems
    die("Missing mandatory argument 'takeCount' to Trie.trieKeys");
  }

  if (is_emptyTrie(m)) return Immutable.Set();
  return walk(m, takeCount0, Immutable.List(),
              function (items, tail) {
                if (is_emptyTrie(tail)) return Immutable.Set();
                if (tail instanceof $Success) return Immutable.Set.of(items);
                die("Trie contains more than the requested "+takeCount0+" items");
              });

  function walk(m, takeCount, valsRev, kont) {
    if (takeCount === 0) return kont(valsRev.reverse(), m);

    if (is_emptyTrie(m)) return Immutable.Set();
    if (m instanceof $Success) {
      die("Trie contains fewer than the requested "+takeCount0+" items");
    }

    if (!is_emptyTrie(m.wild)) return false;

    var result = Immutable.Set();
    m.edges.forEach(function (keymap, arity) {
      if (result === false) return false; // break out of iteration
      keymap.forEach(function (k, key) {
        if (result === false) return false; // break out of iteration

        var piece;
        if (Struct.isStructureType(key) || key === SOA) {
          piece = walk(k, arity, Immutable.List(), function (items, m1) {
            var item = reconstructSequence(key, items);
            return walk(m1, takeCount - 1, valsRev.unshift(item), kont);
          });
        } else {
          piece = walk(k, takeCount - 1, valsRev.unshift(key), kont);
        }

        result = (piece === false) ? false : result.union(piece);
      });
    });
    return result;
  }
}

function captureToObject(captures, names) {
  var d = {};
  captures.forEach(function (key, index) {
    d[names[index] || ('$' + index)] = key;
  });
  return d;
}

function trieKeysToObjects(trieKeysResult, names) {
  if (trieKeysResult === false) return false;
  return trieKeysResult.toList().map(function (e) { return captureToObject(e, names); });
}

function projectObjects(m, projection) {
  var names = projectionNames(projection);
  return trieKeysToObjects(trieKeys(project(m, projection), names.length), names);
}

function prettyTrie(m, initialIndent) {
  var acc = [];
  walk(initialIndent || 0, m);
  return acc.join('');

  function walk(i, m) {
    if (m instanceof $Success) {
      var v = m.value;
      if (Immutable.Set.isSet(v)) { v = v.toArray(); }
      acc.push(" {" + JSON.stringify(v) + "}");
      return;
    }

    if (is_emptyTrie(m)) {
      acc.push(" ::: nothing");
      return;
    }

    var needSep = false;
    if (!is_emptyTrie(m.wild)) {
      var key = "★";
      needSep = true;
      acc.push(" ");
      acc.push(key);
      walk(i + key.length + 1, m.wild);
    }
    m.edges
      .toOrderedMap()
      .sortBy(function (keymap, arity) { return arity })
      .forEach(function (keymap, arity) {
        keymap
          .toOrderedMap()
          .sortBy(function (k, key) { return key })
          .forEach(function (k, key) {
	    if (needSep) {
	      acc.push("\n");
	      acc.push(indentStr(i));
	    } else {
	      needSep = true;
	    }
	    acc.push(" ");
	    if (key === SOA) key = '<' + arity + '>';
            else if (Struct.isStructureType(key)) key = key.label + '<' + arity + '>';
	    else if (key instanceof $Special) key = key.name;
            else key = JSON.stringify(key);

            if (typeof key === 'undefined') key = 'undefined';
	    acc.push(key);
	    walk(i + key.length + 1, k);
          });
      });
  }

  function indentStr(i) {
    return new Array(i + 1).join(' '); // eww
  }
}

///////////////////////////////////////////////////////////////////////////

function parenTypeToString(key) {
  if (Struct.isStructureType(key)) {
    return ':' + key.label;
  } else {
    return 'L';
  }
}

function stringToParenType(arity, key) {
  if (key[0] === ':') {
    return new Struct.StructureType(key.slice(1), arity);
  } else if (key === 'L') {
    return SOA;
  }
  throw new Error("Unsupported JSON trie paren type: "+key);
}

function trieToJSON(t) {
  if (is_emptyTrie(t)) { return []; }
  if (t instanceof $Success) { return [true]; } // TODO: consider generalizing

  // It's a $Branch.

  var jParens = [];
  var jAtoms = [];
  t.edges.forEach(function (keymap, arity) {
    keymap.forEach(function (k, key) {
      var jk = trieToJSON(k);
      if (Struct.isStructureType(key) || key === SOA) {
        jParens.push([arity, parenTypeToString(key), jk]);
      } else {
        jAtoms.push([key, jk]);
      }
    });
  });
  return [jParens, trieToJSON(t.wild), jAtoms];
}

function badJSON(j) {
  die("Cannot deserialize JSON trie: " + JSON.stringify(j));
}

function trieFromJSON(j) {
  return decode(j);

  function decode(j) {
    if (!Array.isArray(j)) badJSON(j);

    switch (j.length) {
    case 0: return emptyTrie;
    case 1: return rsuccess(true); // TODO: consider generalizing
    case 3: {
      var result = rcopybranch(expand(rwild(decode(j[1]))));
      j[0].forEach(function (entry) {
        var arity = entry[0];
        if (typeof arity !== 'number') badJSON(j);
        var key = stringToParenType(arity, entry[1]);
        rupdate_inplace(result, arity, key, decode(entry[2]));
      });
      j[2].forEach(function (entry) {
        var key = entry[0];
        rupdate_inplace(result, 0, key, decode(entry[1]));
      });
      return collapse(result);
    }
    default: badJSON(j);
    }
  }
}

///////////////////////////////////////////////////////////////////////////

module.exports.__ = __;
module.exports.SOA = SOA;
module.exports.$Capture = $Capture;
module.exports.$Special = $Special;
module.exports._$ = _$;
module.exports.is_emptyTrie = is_emptyTrie;
module.exports.emptyTrie = emptyTrie;
module.exports.embeddedTrie = embeddedTrie;
module.exports.embeddedTrieArray = embeddedTrieArray;
module.exports.compilePattern = compilePattern;
module.exports.matchPattern = matchPattern;
module.exports._union = union;
module.exports.union = unionN;
module.exports.intersect = intersect;
module.exports.subtract = subtract;
module.exports.matchValue = matchValue;
module.exports.matchTrie = matchTrie;
module.exports.appendTrie = appendTrie;
// module.exports.triePruneBranch = triePruneBranch;
module.exports.trieStep = trieStep;
module.exports.trieSuccess = rsuccess;
module.exports.relabel = relabel;
module.exports.projectionNames = projectionNames;
module.exports.projectionArity = projectionArity;
module.exports.projectionToPattern = projectionToPattern;
module.exports.project = project;
module.exports.projectMany = projectMany;
module.exports.trieKeys = trieKeys;
module.exports.captureToObject = captureToObject;
module.exports.trieKeysToObjects = trieKeysToObjects;
module.exports.projectObjects = projectObjects;
module.exports.prettyTrie = prettyTrie;
module.exports.trieToJSON = trieToJSON;
module.exports.trieFromJSON = trieFromJSON;

// For testing
module.exports._testing = {
  rsuccess: rsuccess,
  rseq: rseq,
  rwild: rwild
};
