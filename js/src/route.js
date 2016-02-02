var Immutable = require("immutable");

function $Special(name) {
  this.name = name;
}

var __ = new $Special("wildcard"); /* wildcard marker */

var SOA = new $Special("["); // start of array
var EOA = new $Special("]"); // end of array

function die(message) {
  throw new Error(message);
}

function $Embedded(trie) {
  this.trie = trie;
}

function embeddedTrie(trie) {
  return new $Embedded(trie);
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

var SOC = new $Special("{"); // start of capture
var EOC = new $Special("}"); // end of capture

function $Success(value) {
  this.value = value;
}

$Success.prototype.equals = function (other) {
  if (!(other instanceof $Success)) return false;
  return Immutable.is(this.value, other.value);
};

function $WildcardSequence(trie) {
  this.trie = trie;
}

$WildcardSequence.prototype.equals = function (other) {
  if (!(other instanceof $WildcardSequence)) return false;
  return Immutable.is(this.trie, other.trie);
};

function is_emptyTrie(m) {
  return Immutable.is(m, emptyTrie);
}

///////////////////////////////////////////////////////////////////////////
// Constructors

var emptyTrie = Immutable.Map();

function rsuccess(v) {
  return (v === null) ? emptyTrie : new $Success(v);
}

function rseq(e, r) {
  if (r === emptyTrie) return emptyTrie;
  return emptyTrie.set(e, r);
}

function rwild(r) {
  return rseq(__, r);
}

function rwildseq(r) {
  return (r === emptyTrie) ? emptyTrie : new $WildcardSequence(r);
}

///////////////////////////////////////////////////////////////////////////

function compilePattern(v, p) {
  if (!p) die("compilePattern: missing pattern");
  return walk(p, rseq(EOA, rsuccess(v)));

  function walk(p, acc) {
    if (p === __) return rwild(acc);

    if (Array.isArray(p)) {
      acc = rseq(EOA, acc);
      for (var i = p.length - 1; i >= 0; i--) {
	acc = walk(p[i], acc);
      }
      return rseq(SOA, acc);
    }

    if (p instanceof $Embedded) {
      return appendTrie(p.trie, function (v) { return acc; });
    } else {
      return rseq(p, acc);
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

function rupdate(r, key, k) {
  var oldWild = r.get(__, emptyTrie);
  if (Immutable.is(k, oldWild)) {
    return r.remove(key);
  } else {
    return r.set(key, k);
  }
}

function rlookup(r, key) {
  return r.get(key, emptyTrie);
}

function rlookupWild(r, key) {
  return r.get(key, false);
}

function is_keyOpen(k) {
  return k === SOA;
}

function is_keyClose(k) {
  return k === EOA;
}

function is_keyNormal(k) {
  return !(is_keyOpen(k) || is_keyClose(k));
}

///////////////////////////////////////////////////////////////////////////

var unionSuccessesDefault = function (v1, v2) {
  if (v1 === true) return v2;
  if (v2 === true) return v1;
  return v1.union(v2);
};

var intersectSuccessesDefault = function (v1, v2) {
  return v1;
};

var subtractSuccessesDefault = function (v1, v2) {
  var r = v1.subtract(v2);
  if (r.isEmpty()) return null;
  return r;
};

var matchTrieSuccesses = function (v1, v2, acc) {
  return acc.union(v2);
};

var projectSuccess = function (v) {
  return v;
};

///////////////////////////////////////////////////////////////////////////

function expandWildseq(r) {
  return union(rwild(rwildseq(r)), rseq(EOA, r));
}

function union(o1, o2, unionSuccessesOpt) {
  var unionSuccesses = unionSuccessesOpt || unionSuccessesDefault;
  return merge(o1, o2);

  function merge(o1, o2) {
    if (is_emptyTrie(o1)) return o2;
    if (is_emptyTrie(o2)) return o1;
    return walk(o1, o2);
  }

  function walk(r1, r2) {
    if (r1 instanceof $WildcardSequence) {
      if (r2 instanceof $WildcardSequence) {
	return rwildseq(walk(r1.trie, r2.trie));
      }
      r1 = expandWildseq(r1.trie);
    } else if (r2 instanceof $WildcardSequence) {
      r2 = expandWildseq(r2.trie);
    }

    if (r1 instanceof $Success) {
      if (r2 instanceof $Success) {
	return rsuccess(unionSuccesses(r1.value, r2.value));
      } else {
	die("Route.union: left short!");
      }
    } else if (r2 instanceof $Success) {
      die("Route.union: right short!");
    }

    var w = merge(rlookup(r1, __), rlookup(r2, __));
    if (is_emptyTrie(w)) {
      var smaller = r1.size < r2.size ? r1 : r2;
      var larger  = r1.size < r2.size ? r2 : r1;
      var target = larger;
      smaller.forEach(function (val, key) {
	var k = merge(rlookup(smaller, key), rlookup(larger, key));
	target = rupdate(target, key, k);
      });
      return target;
    } else {
      var target = rwild(w);
      function examineKey(rA, key, rB) {
	if ((key !== __) && !target.has(key)) {
	  var k = merge(rlookup(rA, key), rlookup(rB, key));
	  if (is_keyOpen(key)) {
	    target = rupdate(target, key, merge(rwildseq(w), k));
	  } else if (is_keyClose(key)) {
	    if (w instanceof $WildcardSequence) {
	      target = rupdate(target, key, merge(w.trie, k));
	    } else {
	      target = rupdate(target, key, k);
	    }
	  } else {
	    target = rupdate(target, key, merge(w, k));
	  }
	}
      }
      r1.forEach(function (val, key) { examineKey(r1, key, r2) });
      r2.forEach(function (val, key) { examineKey(r2, key, r1) });
      return target;
    }
  }
}

function unionN() {
  var acc = emptyTrie;
  for (var i = 0; i < arguments.length; i++) {
    acc = union(acc, arguments[i]);
  }
  return acc;
}

function intersect(o1, o2, intersectSuccessesOpt, leftShortOpt) {
  var intersectSuccesses = intersectSuccessesOpt || intersectSuccessesDefault;
  var leftShort = leftShortOpt || function (v, r) {
    die("Route.intersect: left side short!");
  };
  return walk(o1, o2);

  function walkFlipped(r2, r1) { return walk(r1, r2); }

  function walk(r1, r2) {
    // INVARIANT: r1 is a part of the original o1, and
    // likewise for r2. This is so that the first arg to
    // intersectSuccesses always comes from r1, and the second
    // from r2.
    if (is_emptyTrie(r1)) return emptyTrie;
    if (is_emptyTrie(r2)) return emptyTrie;

    if (r1 instanceof $WildcardSequence) {
      if (r2 instanceof $WildcardSequence) {
	return rwildseq(walk(r1.trie, r2.trie));
      }
      r1 = expandWildseq(r1.trie);
    } else if (r2 instanceof $WildcardSequence) {
      r2 = expandWildseq(r2.trie);
    }

    if (r1 instanceof $Success) {
      if (r2 instanceof $Success) {
	return rsuccess(intersectSuccesses(r1.value, r2.value));
      } else {
	return leftShort(r1.value, r2);
      }
    }

    var w1 = rlookup(r1, __);
    var w2 = rlookup(r2, __);
    var w = walk(w1, w2);

    var target = emptyTrie;

    function examineKey(key) {
      if ((key !== __) && !target.has(key)) {
	var k1 = rlookup(r1, key);
	var k2 = rlookup(r2, key);
	if (is_emptyTrie(k1)) {
	  if (is_emptyTrie(k2)) {
	    target = rupdate(target, key, emptyTrie);
	  } else {
	    target = rupdate(target, key, walkWild(walk, w1, key, k2));
	  }
	} else {
	  if (is_emptyTrie(k2)) {
	    target = rupdate(target, key, walkWild(walkFlipped, w2, key, k1));
	  } else {
	    target = rupdate(target, key, walk(k1, k2));
	  }
	}
      }
    }

    if (is_emptyTrie(w1)) {
      if (is_emptyTrie(w2)) {
	(r1.size < r2.size ? r1 : r2).forEach(function (val, key) { examineKey(key) });
      } else {
	r1.forEach(function (val, key) { examineKey(key) });
      }
    } else {
      if (is_emptyTrie(w2)) {
	r2.forEach(function (val, key) { examineKey(key) });
      } else {
	target = rupdate(target, __, w);
	r1.forEach(function (val, key) { examineKey(key) });
	r2.forEach(function (val, key) { examineKey(key) });
      }
    }
    return target;
  }

  function walkWild(walker, w, key, k) {
    if (is_emptyTrie(w)) return emptyTrie;
    if (is_keyOpen(key)) return walker(rwildseq(w), k);
    if (is_keyClose(key)) {
      if (w instanceof $WildcardSequence) return walker(w.trie, k);
      return emptyTrie;
    }
    return walker(w, k);
  }
}

// The subtractSuccesses function should return null to signal "no
// remaining success values".
function subtract(o1, o2, subtractSuccessesOpt) {
  var subtractSuccesses = subtractSuccessesOpt || subtractSuccessesDefault;
  return walk(o1, o2);

  function walkFlipped(r2, r1) { return walk(r1, r2); }

  function walk(r1, r2) {
    if (is_emptyTrie(r1)) {
      return emptyTrie;
    } else {
      if (is_emptyTrie(r2)) {
	return r1;
      }
    }

    if (r1 instanceof $WildcardSequence) {
      if (r2 instanceof $WildcardSequence) {
	return rwildseq(walk(r1.trie, r2.trie));
      }
      r1 = expandWildseq(r1.trie);
    } else if (r2 instanceof $WildcardSequence) {
      r2 = expandWildseq(r2.trie);
    }

    if (r1 instanceof $Success && r2 instanceof $Success) {
      return rsuccess(subtractSuccesses(r1.value, r2.value));
    }

    var w1 = rlookup(r1, __);
    var w2 = rlookup(r2, __);
    var w = walk(w1, w2);
    var target;

    function examineKey(key) {
      if (key !== __) {
	var k1 = rlookupWild(r1, key);
	var k2 = rlookupWild(r2, key);
	var updatedK;
	if (!k1) {
	  if (!k2) {
	    return;
	  }
	  // There is an entry in r2 but not r1 for our key.
	  updatedK =
	    is_emptyTrie(w1) ? emptyTrie :
	    is_keyOpen(key) ? walk(rwildseq(w1), k2) :
	    is_keyClose(key) ? ((w1 instanceof $WildcardSequence) ? walk(w1.trie, k2) : emptyTrie) :
	    walk(w1, k2);
	} else if (!k2) {
	  // There is an entry in r1 but not r2 for our key.
	  updatedK =
	    is_emptyTrie(w2) ? k1 :
	    is_keyOpen(key) ? walk(k1, rwildseq(w2)) :
	    is_keyClose(key) ? ((w2 instanceof $WildcardSequence) ? walk(k1, w2.trie) : k1) :
	    walk(k1, w2);
	} else {
	  updatedK = walk(k1, k2);
	}
	// Here we ensure a "minimal" remainder in cases
	// where after an erasure, a particular key's
	// continuation is the same as the wildcard's
	// continuation. TODO: the equals check may
	// be expensive. If so, how can it be made
	// cheaper?
	if (is_keyOpen(key)) {
	  target = rupdate(target, key,
			   ((updatedK instanceof $WildcardSequence) &&
			    Immutable.is(updatedK.trie, w))
			   ? w
			   : updatedK);
	} else {
	  target = rupdate(target, key, updatedK);
	}
      }
    }

    if (is_emptyTrie(w2)) {
      target = r1;
      r2.forEach(function (val, key) { examineKey(key) });
    } else {
      target = emptyTrie;
      target = rupdate(target, __, w);
      r1.forEach(function (val, key) { examineKey(key) });
      r2.forEach(function (val, key) { examineKey(key) });
    }

    // Here, the target is complete. If it has only two keys,
    // one wild and one is_keyClose, and wild's continuation
    // is a $WildcardSequence and the other continuation is
    // identical to the sequence's continuation, then replace
    // the whole thing with a nested $WildcardSequence.
    // (We know w === rlookup(target, __) from before.)
    //
    // TODO: I suspect actually this applies even if there are
    // more than two keys, so long as all their continuations
    // are identical and there's at least one is_keyClose
    // alongside a wild.
    if (target.size === 2) {
      var finalW = rlookup(target, __);
      if (finalW instanceof $WildcardSequence) {
	target.forEach(function (k, key) {
	  if ((key !== __) && is_keyClose(key)) {
	    if (Immutable.is(k, finalW.trie)) {
	      target = finalW;
	      return false; // terminate the iteration early
	    }
	  }
	});
      }
    }

    return target;
  }
}

// Returns null on failed match, otherwise the appropriate success
// value contained in the trie r.
function matchValue(r, v) {
  var failureResult = null;

  var vs = Immutable.List.of(v);
  var stack = [Immutable.List()];

  while (!is_emptyTrie(r)) {
    if (r instanceof $WildcardSequence) {
      if (stack.length === 0) return failureResult;
      vs = stack.pop();
      r = r.trie;
      continue;
    }

    if (r instanceof $Success) {
      if (vs.size === 0 && stack.length === 0) return r.value;
      return failureResult;
    }

    if (vs.size === 0) {
      if (stack.length === 0) return failureResult;
      vs = stack.pop();
      r = rlookup(r, EOA);
      continue;
    }

    var v = vs.first();
    vs = vs.shift();

    if (typeof v === 'string' && v.substring(0, 2) === '__') {
      die("Cannot match special string starting with __");
    }

    if (Array.isArray(v)) {
      if (r.has(SOA)) {
	r = rlookup(r, SOA);
	stack.push(vs);
	vs = Immutable.List(v);
      } else {
	r = rlookup(r, __);
      }
    } else {
      if (r.has(v)) {
	r = rlookup(r, v);
      } else {
	r = rlookup(r, __);
      }
    }
  }

  return failureResult;
}

function matchTrie(o1, o2, seed, leftShortOpt) {
  var acc = typeof seed === 'undefined' ? Immutable.Set() : seed; // variable updated imperatively
  var leftShort = leftShortOpt || function (v, r, acc) {
    die("Route.matchTrie: left side short!");
  };
  walk(o1, o2);
  return acc;

  function walkFlipped(r2, r1) { return walk(r1, r2); }

  function walk(r1, r2) {
    if (is_emptyTrie(r1) || is_emptyTrie(r2)) return;

    if (r1 instanceof $WildcardSequence) {
      if (r2 instanceof $WildcardSequence) {
	walk(r1.trie, r2.trie);
	return;
      }
      r1 = expandWildseq(r1.trie);
    } else if (r2 instanceof $WildcardSequence) {
      r2 = expandWildseq(r2.trie);
    }

    if (r1 instanceof $Success) {
      if (r2 instanceof $Success) {
	acc = matchTrieSuccesses(r1.value, r2.value, acc);
      } else {
	acc = leftShort(r1.value, r2, acc);
      }
      return;
    } else if (r2 instanceof $Success) {
      die("Route.matchTrie: right side short!");
    }

    var w1 = rlookup(r1, __);
    var w2 = rlookup(r2, __);
    walk(w1, w2);

    function examineKey(key) {
      if (key !== __) {
	var k1 = rlookup(r1, key);
	var k2 = rlookup(r2, key);
	if (is_emptyTrie(k1)) {
	  if (is_emptyTrie(k2)) {
	    return;
	  } else {
	    walkWild(walk, w1, key, k2);
	  }
	} else {
	  if (is_emptyTrie(k2)) {
	    walkWild(walkFlipped, w2, key, k1);
	  } else {
	    walk(k1, k2);
	  }
	}
      }
    }

    // Optimize similarly to intersect().
    if (is_emptyTrie(w1)) {
      if (is_emptyTrie(w2)) {
	(r1.size < r2.size ? r1 : r2).forEach(function (val, key) { examineKey(key) });
      } else {
	r1.forEach(function (val, key) { examineKey(key) });
      }
    } else {
      if (is_emptyTrie(w2)) {
	r2.forEach(function (val, key) { examineKey(key) });
      } else {
	r1.forEach(function (val, key) { examineKey(key) });
	r2.forEach(function (val, key) { examineKey(key) });
      }
    }
  }

  function walkWild(walker, w, key, k) {
    if (is_emptyTrie(w)) return;
    if (is_keyOpen(key)) {
      walker(rwildseq(w), k);
      return;
    }
    if (is_keyClose(key)) {
      if (w instanceof $WildcardSequence) walker(w.trie, k);
      return;
    }
    walker(w, k);
  }
}

function appendTrie(m, mTailFn) {
  return walk(m);

  function walk(m) {
    if (is_emptyTrie(m)) return emptyTrie;
    if (m instanceof $WildcardSequence) return rwildseq(walk(m.trie));
    if (m instanceof $Success) die("Ill-formed trie");

    var target = emptyTrie;
    m.forEach(function (k, key) {
      if (is_keyClose(key) && (k instanceof $Success)) {
	target = union(target, mTailFn(k.value));
      } else {
	target = rupdate(target, key, walk(k));
      }
    });
    return target;
  }
}

function trieStep(m, key) {
  if (is_emptyTrie(m)) return emptyTrie;
  if (m instanceof $WildcardSequence) return (is_keyClose(key) ? m.trie : m);
  if (m instanceof $Success) return emptyTrie;
  var result = rlookupWild(m, key);
  if (result) return result;
  var wildEdge = rlookup(m, __);
  if (is_keyOpen(key)) return rwildseq(wildEdge);
  if (is_keyClose(key)) return (wildEdge instanceof $WildcardSequence) ? wildEdge.trie : emptyTrie;
  return wildEdge;
}

function relabel(m, f) {
  return walk(m);

  function walk(m) {
    if (is_emptyTrie(m)) return emptyTrie;
    if (m instanceof $WildcardSequence) return rwildseq(walk(m.trie));
    if (m instanceof $Success) return rsuccess(f(m.value));

    var target = emptyTrie;
    m.forEach(function (k, key) {
      target = rupdate(target, key, walk(k));
    });
    return target;
  }
}

function compileProjection(/* projection, projection, ... */) {
  var names = [];
  var acc = [];
  for (var i = 0; i < arguments.length; i++) {
    walk(arguments[i]);
  }
  acc.push(EOA);
  return {names: names, spec: acc};

  function walk(p) {
    if (isCapture(p)) {
      names.push(captureName(p));
      acc.push(SOC);
      walk(capturePattern(p));
      acc.push(EOC);
      return;
    }

    if (Array.isArray(p)) {
      acc.push(SOA);
      for (var i = 0; i < p.length; i++) {
	walk(p[i]);
      }
      acc.push(EOA);
      return;
    }

    if (p instanceof $Embedded) {
      die("Cannot embed trie in projection");
    } else {
      acc.push(p);
    }
  }
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

    if (p instanceof $Embedded) {
      return p.trie;
    } else {
      return p;
    }
  }
}

function project(m, compiledProjection) {
  var spec = compiledProjection.spec;
  return walk(false, m, 0);

  function walk(isCapturing, m, specIndex) {
    if (specIndex >= spec.length) {
      if (isCapturing) die("Bad specification: unclosed capture");
      if (m instanceof $Success) {
	return rseq(EOA, rsuccess(projectSuccess(m.value)));
      } else {
	return emptyTrie;
      }
    }

    if (is_emptyTrie(m)) return emptyTrie;

    var item = spec[specIndex];
    var nextIndex = specIndex + 1;

    if (item === EOC) {
      if (!isCapturing) die("Bad specification: unexpected EOC");
      return walk(false, m, nextIndex);
    }

    if (item === SOC) {
      if (isCapturing) die("Bad specification: nested capture");
      return walk(true, m, nextIndex);
    }

    if (item === __) {
      if (m instanceof $WildcardSequence) {
	if (isCapturing) {
	  return rwild(walk(isCapturing, m, nextIndex));
	} else {
	  return walk(isCapturing, m, nextIndex);
	}
      }

      if (m instanceof $Success) {
	return emptyTrie;
      }

      var target;
      if (isCapturing) {
	target = emptyTrie;
	target = rupdate(target, __, walk(isCapturing, rlookup(m, __), nextIndex));
	m.forEach(function (mk, key) {
	  if (key !== __) {
	    if (is_keyOpen(key)) {
	      function cont(mk2) { return walk(isCapturing, mk2, nextIndex); }
	      target = rupdate(target, key, captureNested(mk, cont));
	    } else if (is_keyClose(key)) {
	      // do nothing
	    } else {
	      target = rupdate(target, key, walk(isCapturing, mk, nextIndex));
	    }
	  }
	});
      } else {
	target = walk(isCapturing, rlookup(m, __), nextIndex);
	m.forEach(function (mk, key) {
	  if (key !== __) {
	    if (is_keyOpen(key)) {
	      function cont(mk2) { return walk(isCapturing, mk2, nextIndex); }
	      target = union(target, skipNested(mk, cont));
	    } else if (is_keyClose(key)) {
	      // do nothing
	    } else {
	      target = union(target, walk(isCapturing, mk, nextIndex));
	    }
	  }
	});
      }
      return target;
    }

    var result;
    if (m instanceof $WildcardSequence) {
      if (is_keyOpen(item)) {
	result = walk(isCapturing, rwildseq(m), nextIndex);
      } else if (is_keyClose(item)) {
	result = walk(isCapturing, m.trie, nextIndex);
      } else {
	result = walk(isCapturing, m, nextIndex);
      }
    } else if (m instanceof $Success) {
      result = emptyTrie;
    } else {
      if (is_keyOpen(item)) {
	result = walk(isCapturing, rwildseq(rlookup(m, __)), nextIndex);
      } else if (is_keyClose(item)) {
	result = emptyTrie;
      } else {
	result = walk(isCapturing, rlookup(m, __), nextIndex);
      }
      result = union(result, walk(isCapturing, rlookup(m, item), nextIndex));
    }
    if (isCapturing) {
      result = rseq(item, result);
    }
    return result;
  }

  function captureNested(m, cont) {
    if (m instanceof $WildcardSequence) {
      return rwildseq(cont(m.trie));
    }

    if (is_emptyTrie(m) || (m instanceof $Success)) {
      return emptyTrie;
    }

    var target = emptyTrie;
    target = rupdate(target, __, captureNested(rlookup(m, __), cont));
    m.forEach(function (mk, key) {
      if (key !== __) {
	if (is_keyOpen(key)) {
	  function cont2(mk2) { return captureNested(mk2, cont); }
	  target = rupdate(target, key, captureNested(mk, cont2));
	} else if (is_keyClose(key)) {
	  target = rupdate(target, key, cont(mk));
	} else {
	  target = rupdate(target, key, captureNested(mk, cont));
	}
      }
    });
    return target;
  }

  function skipNested(m, cont) {
    if (m instanceof $WildcardSequence) {
      return cont(m.trie);
    }

    if (is_emptyTrie(m) || (m instanceof $Success)) {
      return emptyTrie;
    }

    var target = skipNested(rlookup(m, __), cont);
    m.forEach(function (mk, key) {
      if (key !== __) {
	if (is_keyOpen(key)) {
	  function cont2(mk2) { return skipNested(mk2, cont); }
	  target = union(target, skipNested(mk, cont2));
	} else if (is_keyClose(key)) {
	  target = union(target, cont(mk));
	} else {
	  target = union(target, skipNested(mk, cont));
	}
      }
    });
    return target;
  }
}

function trieKeys(m) {
  if (is_emptyTrie(m)) return [];
  var result = walkSeq(m, function (vss, vsk) { return vss; });
  if (result === null) return null;
  return result.map(function (vs) { return vs.toArray() });

  function walk(m, k) {
    if (m instanceof $WildcardSequence) return null;
    if (m instanceof $Success) return [];
    if (m.has(__)) return null;
    var acc = [];
    m.forEach(function (mk, key) {
      var piece;
      if (is_keyOpen(key)) {
	function seqK(vss, vsk) {
	  var acc = [];
	  for (var i = 0; i < vss.length; i++) {
	    var vs = vss[i];
	    acc = acc.concat(k(transformSeqs(vs, key), vsk));
	  }
	  return acc;
	}
	piece = walkSeq(mk, seqK);
      } else if (is_keyClose(key)) {
	die("trieKeys: internal error: unexpected key-close");
      } else {
	piece = k(key, mk);
      }
      if (piece === null) return null;
      acc = acc.concat(piece);
    });
    return acc;
  }

  function walkSeq(m, k) {
    if (m instanceof $WildcardSequence) return null;
    if (m instanceof $Success) return k([], emptyTrie); // TODO: ??
    if (m.has(__)) return null;
    var acc = [];
    m.forEach(function (mk, key) {
      var piece;
      if (is_keyClose(key)) {
	piece = k([Immutable.List()], mk);
      } else {
	function outerK(v, vk) {
	  return walkSeq(vk, function (vss, vsk) {
	    var acc = [];
	    for (var i = 0; i < vss.length; i++) {
	      acc.push(vss[i].unshift(v));
	    }
	    return k(acc, vsk);
	  });
	}
	piece = walk(rseq(key, mk), outerK);
      }
      if (piece === null) return null;
      acc = acc.concat(piece);
    });
    return acc;
  }

  function transformSeqs(vs, opener) {
    if (opener === SOA) return vs;
    die("Internal error: unknown opener " + opener);
  }
}

function trieKeysToObjects(trieKeysResult, compiledProjection) {
  if (trieKeysResult === null) return null;
  var result = [];
  for (var i = 0; i < trieKeysResult.length; i++) {
    var e = trieKeysResult[i];
    var d = {};
    for (var j = 0; j < e.length; j++) {
      d[compiledProjection.names[j] || ('$' + j)] = e[j];
    }
    result.push(d);
  }
  return result;
}

function projectObjects(m, compiledProjection) {
  return trieKeysToObjects(trieKeys(project(m, compiledProjection)), compiledProjection);
}

function prettyTrie(m, initialIndent) {
  var acc = [];
  walk(initialIndent || 0, m);
  return acc.join('');

  function walk(i, m) {
    if (m instanceof $WildcardSequence) {
      acc.push("...>");
      walk(i + 4, m.trie);
      return;
    }
    if (m instanceof $Success) {
      var v = m.value;
      if (Immutable.Set.isSet(v)) { v = v.toArray(); }
      acc.push("{" + JSON.stringify(v) + "}");
      return;
    }

    if (m.size === 0) {
      acc.push("::: nothing");
      return;
    }

    var needSep = false;
    m.toOrderedMap()
      .sortBy(function (k, key) { return key })
      .forEach(function (k, key) {
	if (needSep) {
	  acc.push("\n");
	  acc.push(indentStr(i));
	} else {
	  needSep = true;
	}
	acc.push(" ");
	if (key === __) key = '★';
	else if (key === SOA) key = '<';
	else if (key === EOA) key = '>';
	else if (key instanceof $Special) key = key.name;
	else key = JSON.stringify(key);
	acc.push(key);
	walk(i + key.length + 1, k);
      });
  }

  function indentStr(i) {
    return new Array(i + 1).join(' '); // eww
  }
}

///////////////////////////////////////////////////////////////////////////

module.exports.__ = __;
module.exports.SOA = SOA;
module.exports.EOA = EOA;
module.exports.$Capture = $Capture;
module.exports.$Special = $Special;
module.exports._$ = _$;
module.exports.is_emptyTrie = is_emptyTrie;
module.exports.emptyTrie = emptyTrie;
module.exports.embeddedTrie = embeddedTrie;
module.exports.compilePattern = compilePattern;
module.exports.matchPattern = matchPattern;
module.exports._union = union;
module.exports.union = unionN;
module.exports.intersect = intersect;
module.exports.subtract = subtract;
module.exports.matchValue = matchValue;
module.exports.matchTrie = matchTrie;
module.exports.appendTrie = appendTrie;
module.exports.trieStep = trieStep;
module.exports.relabel = relabel;
module.exports.compileProjection = compileProjection;
module.exports.projectionToPattern = projectionToPattern;
module.exports.project = project;
module.exports.trieKeys = trieKeys;
module.exports.trieKeysToObjects = trieKeysToObjects;
module.exports.projectObjects = projectObjects;
module.exports.prettyTrie = prettyTrie;

// For testing
module.exports._testing = {
  rsuccess: rsuccess,
  rseq: rseq,
  rwild: rwild,
  rwildseq: rwildseq
};
