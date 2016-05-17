"use strict";

// There are also some trie-ish tests scattered around in test-route.js.

var expect = require('expect.js');
var Immutable = require('immutable');

var Syndicate = require('../src/main.js');
var Dataspace = Syndicate.Dataspace;
var Patch = Syndicate.Patch;
var Trie = Syndicate.Trie;

var __ = Syndicate.__;
var _$ = Syndicate._$;

function checkPrettyTrie(m, expected) {
  expect(Trie.prettyTrie(m)).to.equal(expected.join('\n'));
}

function expectSetsEqual(a, bArray) {
  return expect(Immutable.is(a, Immutable.Set(bArray))).to.be(true);
}

describe('matchValue', function () {
  it('rejects wildcard when wildcardUnionOpt is falsy', function () {
    var t = Trie.union(Trie.compilePattern(Immutable.Set.of('A'), ['a', 123]),
                       Trie.compilePattern(Immutable.Set.of('B'), ['b', 234]),
                       Trie.compilePattern(Immutable.Set.of('C'), ['c', 123]));
    expect(function () { Trie.matchValue(t, [__, 123], Immutable.Set()) }).to.throwError();
    expect(function () { Trie.matchValue(t, ['a', __], Immutable.Set()) }).to.throwError();
    expectSetsEqual(Trie.matchValue(t, ['a', 123], Immutable.Set()), ['A']);
  });

  it('accepts wildcard when wildcardUnionOpt is non-falsy', function () {
    var t = Trie.union(Trie.compilePattern(Immutable.Set.of('A'), ['a', 123]),
                       Trie.compilePattern(Immutable.Set.of('A1'), ['a', ['x', 'y']]),
                       Trie.compilePattern(Immutable.Set.of('B'), ['b', 234]),
                       Trie.compilePattern(Immutable.Set.of('C'), ['c', 123]));
    function u(a, b) {
      return a.union(b);
    }
    expectSetsEqual(Trie.matchValue(t, [__, 123], Immutable.Set(), u), ['A', 'C']);
    expectSetsEqual(Trie.matchValue(t, ['a', __], Immutable.Set(), u), ['A', 'A1']);
    expectSetsEqual(Trie.matchValue(t, ['a', 123], Immutable.Set(), u), ['A']);
    expectSetsEqual(Trie.matchValue(t, [__, 234], Immutable.Set(), u), ['B']);
    expectSetsEqual(Trie.matchValue(t, [__, __], Immutable.Set(), u), ['A', 'A1', 'B', 'C']);
  });
});
