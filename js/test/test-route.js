"use strict";

var Immutable = require('immutable');
var expect = require('expect.js');
var util = require('util');
var r = require("../src/route.js");

function checkPrettyTrie(m, expected) {
  expect(r.prettyTrie(m)).to.equal(expected.join('\n'));
}

function checkTrieKeys(actual, expected) {
  expect(actual.equals(Immutable.Set(expected).map(Immutable.List))).to.be(true);
}

describe("basic pattern compilation", function () {
  var sAny = Immutable.Set(['mAny']);
  var sAAny = Immutable.Set(['mAAny']);
  var mAny = r.compilePattern(sAny, r.__);
  var mAAny = r.compilePattern(sAAny, ['A', r.__]);

  it("should print as expected", function () {
    checkPrettyTrie(mAny, [' ★ >{["mAny"]}']);
    checkPrettyTrie(mAAny, [' < "A" ★ > >{["mAAny"]}']);
  });

  describe("of wildcard", function () {
    it("should match anything", function () {
      expect(r.matchValue(mAny, 'hi')).to.eql(sAny);
      expect(r.matchValue(mAny, ['A', 'hi'])).to.eql(sAny);
      expect(r.matchValue(mAny, ['B', 'hi'])).to.eql(sAny);
      expect(r.matchValue(mAny, ['A', [['hi']]])).to.eql(sAny);
    });
  });

  describe("of A followed by wildcard", function () {
    it("should match A followed by anything", function () {
      expect(r.matchValue(mAAny, 'hi')).to.be(null);
      expect(r.matchValue(mAAny, ['A', 'hi'])).to.eql(sAAny);
      expect(r.matchValue(mAAny, ['B', 'hi'])).to.be(null);
      expect(r.matchValue(mAAny, ['A', [['hi']]])).to.eql(sAAny);
    });
  });

  it("should observe basic (in)equivalences", function () {
    expect(Immutable.is(mAny, mAAny)).to.be(false);
    expect(Immutable.is(mAny, mAny)).to.be(true);
    expect(Immutable.is(mAAny, mAAny)).to.be(true);
  });
});

describe("unions", function () {
  it("should collapse common prefix wildcard", function () {
    checkPrettyTrie(r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 'A']),
			    r.compilePattern(Immutable.Set(['B']), [r.__, 'B'])),
		    [' < ★ "A" > >{["A"]}',
		     '     "B" > >{["B"]}']);
  });

  it("should unroll wildcard unioned with nonwildcard", function () {
    checkPrettyTrie(r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 'A']),
			    r.compilePattern(Immutable.Set(['W']), r.__)),
		    [' ★ >{["W"]}',
		     ' < ★ "A" ★...> >{["W"]}',
		     '         > >{["W","A"]}',
		     '     ★...> >{["W"]}',
		     '     > >{["W"]}',
		     '   > >{["W"]}']);
  });

  it("should properly multiply out", function () {
    checkPrettyTrie(r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 2]),
			    r.compilePattern(Immutable.Set(['C']), [1, 3]),
			    r.compilePattern(Immutable.Set(['B']), [3, 4])),
		    [' < ★ 2 > >{["A"]}',
		     '   1 2 > >{["A"]}',
		     '     3 > >{["C"]}',
		     '   3 2 > >{["A"]}',
		     '     4 > >{["B"]}']);

    checkPrettyTrie(r.union(r.compilePattern(Immutable.Set(['C']), [1, 3]),
			    r.compilePattern(Immutable.Set(['B']), [3, 4])),
		    [' < 1 3 > >{["C"]}',
		     '   3 4 > >{["B"]}']);

    checkPrettyTrie(r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 2]),
			    r.compilePattern(Immutable.Set(['C']), [1, 3])),
		    [' < ★ 2 > >{["A"]}',
		     '   1 2 > >{["A"]}',
		     '     3 > >{["C"]}']);

    checkPrettyTrie(r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 2]),
			    r.compilePattern(Immutable.Set(['B']), [3, 4])),
		    [' < ★ 2 > >{["A"]}',
		     '   3 2 > >{["A"]}',
		     '     4 > >{["B"]}']);
  });

  it("should correctly construct intermediate values", function () {
    var MU = r.emptyTrie;
    MU = r.union(MU, r.compilePattern(Immutable.Set(['A']), [r.__, 2]));
    checkPrettyTrie(MU, [' < ★ 2 > >{["A"]}']);
    MU = r.union(MU, r.compilePattern(Immutable.Set(['C']), [1, 3]));
    checkPrettyTrie(MU, [' < ★ 2 > >{["A"]}',
			 '   1 2 > >{["A"]}',
			 '     3 > >{["C"]}']);
    MU = r.union(MU, r.compilePattern(Immutable.Set(['B']), [3, 4]));
    checkPrettyTrie(MU, [' < ★ 2 > >{["A"]}',
			 '   1 2 > >{["A"]}',
			 '     3 > >{["C"]}',
			 '   3 2 > >{["A"]}',
			 '     4 > >{["B"]}']);
  });

  it("should handle identical patterns with different pids", function () {
    var m = r.union(r.compilePattern(Immutable.Set('B'), [2]),
		    r.compilePattern(Immutable.Set('C'), [3]));
    checkPrettyTrie(m, [' < 2 > >{["B"]}',
			'   3 > >{["C"]}']);
    m = r.union(r.compilePattern(Immutable.Set('A'), [2]), m);
    checkPrettyTrie(m, [' < 2 > >{["A","B"]}',
			'   3 > >{["C"]}']);
  });

  it('should work with subtraction and wildcards', function () {
    var x = r.compilePattern(Immutable.Set(["A"]), [r.__]);
    var y = r.compilePattern(Immutable.Set(["A"]), ["Y"]);
    var z = r.compilePattern(Immutable.Set(["A"]), ["Z"]);
    var expected = [' < "Y"::: nothing',
		    '   ★ > >{["A"]}'];
    checkPrettyTrie(r.subtract(r.union(x, z), y), expected);
    checkPrettyTrie(r.union(r.subtract(x, y), z), expected);
  });
});

describe("projections", function () {
  describe("with picky structure", function () {
    var proj = r.compileProjection(r._$("v", [[r.__]]));

    it("should include things that match as well as wildcards", function () {
      checkPrettyTrie(r.project(r.union(r.compilePattern(Immutable.Set(['A']), r.__),
					r.compilePattern(Immutable.Set(['B']), [['b']])),
				proj),
		      [' < < ★ > > >{["A"]}',
		       '     "b" > > >{["A","B"]}']);
    });

    it("should exclude things that lack the required structure", function () {
      checkPrettyTrie(r.project(r.union(r.compilePattern(Immutable.Set(['A']), r.__),
					r.compilePattern(Immutable.Set(['B']), ['b'])),
				proj),
		      [' < < ★ > > >{["A"]}']);
    });
  });

  describe("simple positional", function () {
    var proj = r.compileProjection([r._$, r._$]);

    it("should collapse common prefixes", function () {
      checkPrettyTrie(r.project(r.union(r.compilePattern(Immutable.Set(['A']), [1, 2]),
					r.compilePattern(Immutable.Set(['C']), [1, 3]),
					r.compilePattern(Immutable.Set(['B']), [3, 4])),
				proj),
		      [' 1 2 >{["A"]}',
		       '   3 >{["C"]}',
		       ' 3 4 >{["B"]}']);
    });

    it("should yield a correct set of results", function () {
      var u = r.union(r.compilePattern(Immutable.Set(['A']), [1, 2]),
		      r.compilePattern(Immutable.Set(['C']), [1, 3]),
		      r.compilePattern(Immutable.Set(['B']), [3, 4]));
      checkTrieKeys(r.trieKeys(r.project(u, proj)), [[1, 2], [1, 3], [3, 4]]);
    });
  });
});

describe("subtraction", function () {
  it("should basically work", function () {
    checkPrettyTrie(r.subtract(r.compilePattern(true, r.__),
			       r.compilePattern(true, 3),
			       function (v1, v2) { return null; }),
		    [" ★ >{true}",
		     " 3::: nothing"]);
    checkPrettyTrie(r.subtract(r.compilePattern(true, r.__),
			       r.compilePattern(true, [3]),
			       function (v1, v2) { return null; }),
		    [" ★ >{true}",
		     " < ★...> >{true}",
		     "   > >{true}",
		     "   3 ★...> >{true}",
		     "     >::: nothing"]);
  });

  it("should be idempotent if the subtrahend doesn't overlap the minuend", function () {
    checkPrettyTrie(r.compilePattern(true, 1),
		    [' 1 >{true}']);
    checkPrettyTrie(r.subtract(r.compilePattern(true, 1),
			       r.compilePattern(true, 2)),
		    [' 1 >{true}']);
    checkPrettyTrie(r.subtract(r.compilePattern(true, 1),
			       r.compilePattern(true, 2),
			       function (v1, v2) { return null; }),
		    [' 1 >{true}']);
  });
});

describe("subtract after union", function () {
  var R1 = r.compilePattern(Immutable.Set(['A']), [r.__, "B"]);
  var R2 = r.compilePattern(Immutable.Set(['B']), ["A", r.__]);
  var R12 = r.union(R1, R2);

  it("should have sane preconditions", function () { // Am I doing this right?
    checkPrettyTrie(R1, [' < ★ "B" > >{["A"]}']);
    checkPrettyTrie(R2, [' < "A" ★ > >{["B"]}']);
    checkPrettyTrie(R12, [' < "A" "B" > >{["B","A"]}',
			  '       ★ > >{["B"]}',
			  '   ★ "B" > >{["A"]}']);
  });

  it("should yield the remaining ingredients of the union", function () {
    expect(Immutable.is(r.subtract(R12, R1), R2)).to.be(true);
    expect(Immutable.is(r.subtract(R12, R2), R1)).to.be(true);
    expect(Immutable.is(r.subtract(R12, R1), R1)).to.be(false);
  });
});

describe("trie equality", function () {
  it("should not rely on object identity", function () {
    expect(Immutable.is(r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 'A']),
				r.compilePattern(Immutable.Set(['B']), [r.__, 'B'])),
			r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 'A']),
				r.compilePattern(Immutable.Set(['B']), [r.__, 'B']))))
      .to.be(true);
  });

  it("should respect commutativity of union", function () {
    expect(Immutable.is(r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 'A']),
				r.compilePattern(Immutable.Set(['B']), [r.__, 'B'])),
			r.union(r.compilePattern(Immutable.Set(['B']), [r.__, 'B']),
				r.compilePattern(Immutable.Set(['A']), [r.__, 'A']))))
      .to.be(true);
  });
});

describe("trieKeys on wild tries", function () {
  var M = r.union(r.compilePattern(Immutable.Set(['A']), [r.__, 2]),
		  r.compilePattern(Immutable.Set(['C']), [1, 3]),
		  r.compilePattern(Immutable.Set(['B']), [3, 4]));

  it("should yield null to signal an infinite result", function () {
    expect(r.trieKeys(r.project(M, r.compileProjection([r._$, r._$])))).to.be(null);
  });

  it("should extract just the second array element successfully", function () {
    checkTrieKeys(r.trieKeys(r.project(M, r.compileProjection([r.__, r._$]))),
		  [[2],[3],[4]]);
  });

  var M2 = r.project(M, r.compileProjection([r._$, r._$]));

  it("should survive double-projection", function () {
    checkTrieKeys(r.trieKeys(r.project(M2, r.compileProjection(r.__, r._$))),
		  [[2],[3],[4]]);
  });

  it("should survive embedding and reprojection", function () {
    checkTrieKeys(r.trieKeys(r.project(r.compilePattern(true, [r.embeddedTrie(M2)]),
				       r.compileProjection([r.__, r._$]))),
		  [[2],[3],[4]]);
    checkTrieKeys(r.trieKeys(r.project(r.compilePattern(true, [[r.embeddedTrie(M2)]]),
				       r.compileProjection([[r.__, r._$]]))),
		  [[2],[3],[4]]);
  });
});

describe("trieKeys using multiple-values in projections", function () {
  var M = r.union(r.compilePattern(Immutable.Set(['A']), [1, 2]),
		  r.compilePattern(Immutable.Set(['C']), [1, 3]),
		  r.compilePattern(Immutable.Set(['B']), [3, 4]));
  var proj = r.compileProjection([r._$, r._$]);
  var M2 = r.project(M, proj);

  it("should be able to extract ordinary values", function () {
    checkTrieKeys(r.trieKeys(M2), [[1,2],[1,3],[3,4]]);
  });

  it("should be able to be reprojected as a sequence of more than one value", function () {
    checkTrieKeys(r.trieKeys(r.project(M2, r.compileProjection(r._$, r._$))),
		  [[1,2],[1,3],[3,4]]);
  });

  it("should be convertible into objects with $-indexed fields", function () {
    expect(r.trieKeysToObjects(r.trieKeys(M2), proj).toArray())
      .to.eql([{'$0': 3, '$1': 4}, {'$0': 1, '$1': 2}, {'$0': 1, '$1': 3}]);
    expect(r.projectObjects(M, proj).toArray())
      .to.eql([{'$0': 3, '$1': 4}, {'$0': 1, '$1': 2}, {'$0': 1, '$1': 3}]);
  });
});

describe("trieKeys using multiple-values in projections, with names", function () {
  var M = r.union(r.compilePattern(Immutable.Set(['A']), [1, 2]),
		  r.compilePattern(Immutable.Set(['C']), [1, 3]),
		  r.compilePattern(Immutable.Set(['B']), [3, 4]));

  it("should yield named fields", function () {
    expect(r.projectObjects(M, r.compileProjection([r._$("fst"), r._$("snd")])).toArray())
      .to.eql([{'fst': 3, 'snd': 4}, {'fst': 1, 'snd': 2}, {'fst': 1, 'snd': 3}]);
  });

  it("should yield numbered fields where names are missing", function () {
    expect(r.projectObjects(M, r.compileProjection([r._$, r._$("snd")])).toArray())
      .to.eql([{'$0': 3, 'snd': 4}, {'$0': 1, 'snd': 2}, {'$0': 1, 'snd': 3}]);
    expect(r.projectObjects(M, r.compileProjection([r._$("fst"), r._$])).toArray())
      .to.eql([{'fst': 3, '$1': 4}, {'fst': 1, '$1': 2}, {'fst': 1, '$1': 3}]);
  });
});

describe("complex erasure", function () {
  var A = r.compilePattern(Immutable.Set(['A']), r.__);
  var B = r.union(r.compilePattern(Immutable.Set(['B']), [[[["foo"]]]]),
		  r.compilePattern(Immutable.Set(['B']), [[[["bar"]]]]));
  describe("after a union", function () {
    var R0 = r.union(A, B);
    var R1a = r.subtract(R0, B);
    var R1b = r.subtract(R0, A);

    it("should yield the other parts of the union", function () {
      expect(Immutable.is(R1a, A)).to.be(true);
      expect(Immutable.is(R1b, B)).to.be(true);
    });
  });
});

describe("embedding tries in patterns", function () {
  var M1a =
      r.compilePattern(Immutable.Set(['A']),
		       [1, r.embeddedTrie(r.compilePattern(Immutable.Set(['B']), [2, 3])), 4]);
  var M1b =
      r.compilePattern(Immutable.Set(['A']), [1, [2, 3], 4]);
  var M2a =
      r.compilePattern(Immutable.Set(['A']),
		       [r.embeddedTrie(r.compilePattern(Immutable.Set(['B']), [1, 2])),
		        r.embeddedTrie(r.compilePattern(Immutable.Set(['C']), [3, 4]))]);
  var M2b =
      r.compilePattern(Immutable.Set(['A']), [[1, 2], [3, 4]]);

  it("should yield tries equivalent to the original patterns", function () {
    expect(Immutable.is(M1a, M1b)).to.be(true);
    expect(Immutable.is(M2a, M2b)).to.be(true);
  });
});

describe("Projection with no captures", function () {
  it("should yield the empty sequence when there's a match", function () {
    var emptySequence = [' >{["A"]}'];

    checkPrettyTrie(r.project(r.compilePattern(Immutable.Set(['A']), ["X", r.__]),
			      r.compileProjection(r.__)),
		    emptySequence);
    checkPrettyTrie(r.project(r.compilePattern(Immutable.Set(['A']), ["X", r.__]),
			      r.compileProjection([r.__, r.__])),
		    emptySequence);
    checkPrettyTrie(r.project(r.compilePattern(Immutable.Set(['A']), ["X", r.__]),
			      r.compileProjection(["X", r.__])),
		    emptySequence);
  });

  it("should yield the empty trie when there's no match", function () {
    expect(r.project(r.compilePattern(Immutable.Set(['A']), ["X", r.__]),
		     r.compileProjection(["Y", r.__]))).to.be(r.emptyTrie);
  });

  it("should yield nonempty sequences when there are captures after all", function () {
    checkPrettyTrie(r.project(r.compilePattern(Immutable.Set(['A']), ["X", r.__]),
			      r.compileProjection([r.__, r._$])),
		    [' ★ >{["A"]}']);
    checkPrettyTrie(r.project(r.compilePattern(Immutable.Set(['A']), ["X", r.__]),
			      r.compileProjection([r._$, r._$])),
		    [' "X" ★ >{["A"]}']);
  });
});

describe('trieStep', function () {
  it('should expand wildcard when given SOA', function () {
    expect(Immutable.is(r.trieStep(r.compilePattern(true, r.__), r.SOA),
			r._testing.rwildseq(r._testing.rseq(r.EOA, r._testing.rsuccess(true)))))
      .to.be(true);
  });
});

describe('intersect', function () {
  it('should compute no-op patch limits properly', function () {
    var x = r.compilePattern(Immutable.Set([0]), ["fieldContents", r.__, r.__]);
    var y = r.compilePattern(Immutable.Set([0]), ["fieldContents", "initial", 7]);
    checkPrettyTrie(r.subtract(x, y), [
      ' < "fieldContents" ★ ★ > >{[0]}',
      '                   "initial" ★ > >{[0]}',
      '                             7::: nothing']);
    checkPrettyTrie(r.intersect(r.subtract(x, y), y), ['::: nothing']);
  });
});

describe('triePruneBranch', function () {
  it('should not affect empty trie', function () {
    checkPrettyTrie(r.triePruneBranch(r.emptyTrie, Immutable.List([])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(r.emptyTrie, Immutable.List([r.SOA])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(r.emptyTrie, Immutable.List(["x"])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(r.emptyTrie, Immutable.List([r.SOA, "x"])), ['::: nothing']);
  });

  it('should leave a hole in a full trie', function () {
    var full = r.compilePattern(true, r.__);
    checkPrettyTrie(r.triePruneBranch(full, Immutable.List([])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(full, Immutable.List([r.SOA])),
		    [' ★ >{true}',
		     ' <::: nothing']);
    checkPrettyTrie(r.triePruneBranch(full, Immutable.List(["x"])),
		    [' ★ >{true}',
		     ' "x"::: nothing']);
    checkPrettyTrie(r.triePruneBranch(full, Immutable.List([r.SOA, "x"])),
		    [' ★ >{true}',
		     ' < ★...> >{true}',
		     '   > >{true}',
		     '   "x"::: nothing']);
  });

  it('should prune in a finite tree and leave the rest alone', function () {
    var A = r.compilePattern(true, ["y"])
    var B = r.union(r.compilePattern(true, ["x"]), A);
    var C = r.union(r.compilePattern(true, "z"), B);
    checkPrettyTrie(r.triePruneBranch(A, Immutable.List([])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(B, Immutable.List([])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(C, Immutable.List([])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(A, Immutable.List(["z"])), [' < "y" > >{true}']);
    checkPrettyTrie(r.triePruneBranch(B, Immutable.List(["z"])), [' < "x" > >{true}',
								  '   "y" > >{true}']);
    checkPrettyTrie(r.triePruneBranch(C, Immutable.List(["z"])), [' < "x" > >{true}',
								  '   "y" > >{true}']);
    checkPrettyTrie(r.triePruneBranch(A, Immutable.List([r.SOA])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(B, Immutable.List([r.SOA])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(C, Immutable.List([r.SOA])), [' "z" >{true}']);
    checkPrettyTrie(r.triePruneBranch(A, Immutable.List([r.SOA, "x"])), [' < "y" > >{true}']);
    checkPrettyTrie(r.triePruneBranch(B, Immutable.List([r.SOA, "x"])), [' < "y" > >{true}']);
    checkPrettyTrie(r.triePruneBranch(C, Immutable.List([r.SOA, "x"])), [' < "y" > >{true}',
									 ' "z" >{true}']);
    checkPrettyTrie(r.triePruneBranch(A, Immutable.List([r.SOA, "y"])), ['::: nothing']);
    checkPrettyTrie(r.triePruneBranch(B, Immutable.List([r.SOA, "y"])), [' < "x" > >{true}']);
    checkPrettyTrie(r.triePruneBranch(C, Immutable.List([r.SOA, "y"])), [' < "x" > >{true}',
									 ' "z" >{true}']);
  });
});

describe('makeStructureConstructor', function () {
  it('should produce the right metadata', function () {
    var ctor = r.makeStructureConstructor('foo', ['bar', 'zot']);
    var inst = ctor(123, 234);
    expect(inst.$SyndicateMeta$.label).to.equal('foo');
    expect(inst.$SyndicateMeta$.arguments).to.eql(['bar', 'zot']);
  });

  it('should produce the right instance data', function () {
    var ctor = r.makeStructureConstructor('foo', ['bar', 'zot']);
    var inst = ctor(123, 234);
    expect(inst.bar).to.equal(123);
    expect(inst.zot).to.equal(234);
  });

  it('should work with compilePattern and matchValue', function () {
    var sA = Immutable.Set(["A"]);
    var ctor = r.makeStructureConstructor('foo', ['bar', 'zot']);
    var inst = ctor(123, 234);
    var x = r.compilePattern(sA, ctor(123, r.__));
    checkPrettyTrie(x, [' < "foo" 123 ★ > >{["A"]}']);
    expect(r.matchValue(x, ctor(123, 234))).to.eql(sA);
    expect(r.matchValue(x, ctor(234, 123))).to.eql(null);
  });
});
