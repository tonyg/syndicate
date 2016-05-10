"use strict";

var expect = require('expect.js');
var Immutable = require('immutable');

var Trie = require('../src/trie.js');
var Patch = require('../src/patch.js');

var __ = Trie.__;
var _$ = Trie._$;

function checkPrettyPatch(p, expectedAdded, expectedRemoved) {
  expect(p.pretty()).to.equal(
    ('<<<<<<<< Removed:\n' + expectedRemoved.join('\n') + '\n' +
     '======== Added:\n' + expectedAdded.join('\n') + '\n' +
     '>>>>>>>>'));
}

describe('basic patch compilation', function () {
  it('should print as expected', function () {
    checkPrettyPatch(Patch.assert([1, 2]),
    		     [' <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.assert(__),
    		     [' ★ {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.sub(__),
    		     [' observe<1> ★ {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2]),
    		     [' observe<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.pub('x'),
    		     [' advertise<1> "x" {true}'],
    		     [' ::: nothing']);
  });

  it('should work at nonzero metalevel', function () {
    checkPrettyPatch(Patch.assert([1, 2], 0),
    		     [' <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.assert([1, 2], 1),
    		     [' at-meta<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.assert([1, 2], 2),
    		     [' at-meta<1> at-meta<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);

    checkPrettyPatch(Patch.sub([1, 2], 0),
    		     [' observe<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2], 1),
    		     [' at-meta<1> observe<1> <2> 1 2 {true}',
		      ' observe<1> at-meta<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2], 2),
    		     [' at-meta<1> at-meta<1> observe<1> <2> 1 2 {true}',
		      '            observe<1> at-meta<1> <2> 1 2 {true}',
		      ' observe<1> at-meta<1> at-meta<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);
  });
});

describe('patch sequencing', function () {
  it('should do the right thing in simple cases', function () {
    checkPrettyPatch(Patch.assert(__).andThen(Patch.retract(3)),
    		     [' ★ {true}',
		      ' 3 ::: nothing'],
    		     [' 3 {true}']);
    checkPrettyPatch(Patch.assert(3).andThen(Patch.retract(__)),
    		     [' ::: nothing'],
    		     [' ★ {true}']);
    checkPrettyPatch(Patch.assert(__).andThen(Patch.retract(__)),
    		     [' ::: nothing'],
    		     [' ★ {true}']);
    checkPrettyPatch(Patch.assert(3).andThen(Patch.retract(3)),
    		     [' ::: nothing'],
		     [' 3 {true}']);
    checkPrettyPatch(Patch.sub([1, __]).andThen(Patch.unsub([1, 2])),
    		     [' observe<1> <2> 1 ★ {true}',
		      '                  2 ::: nothing'],
    		     [' observe<1> <2> 1 2 {true}']);
    checkPrettyPatch(Patch.sub([__, 2]).andThen(Patch.unsub([1, 2])),
    		     [' observe<1> <2> ★ 2 {true}',
		      '                1 ::: nothing'],
    		     [' observe<1> <2> 1 2 {true}']);
    checkPrettyPatch(Patch.sub([__, __]).andThen(Patch.unsub([1, 2])),
    		     [' observe<1> <2> ★ ★ {true}',
		      '                1 ★ {true}',
		      '                  2 ::: nothing'],
    		     [' observe<1> <2> 1 2 {true}']);
  });

  it('works for longer chains of asserts and retracts', function () {
    var rawPatch =
	Patch.assert(1)
	.andThen(Patch.retract(2))
	.andThen(Patch.retract(3))
	.andThen(Patch.assert(4))
	.andThen(Patch.retract(99));
      checkPrettyPatch(rawPatch,
		       [' 1 {true}',
		        ' 4 {true}'],
		       [' 2 {true}',
		        ' 3 {true}',
		        ' 99 {true}']);

  });
});

describe('patch lifting', function () {
  it('should basically work', function () {
    checkPrettyPatch(Patch.assert([1, 2]).lift(),
    		     [' at-meta<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2]).lift(),
    		     [' at-meta<1> observe<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.assert([1, 2]).andThen(Patch.assert(Patch.atMeta([1, 2]))).lift(),
    		     [' at-meta<1> at-meta<1> <2> 1 2 {true}',
		      '            <2> 1 2 {true}'],
    		     [' ::: nothing']);
  });
});

describe('patch dropping', function () {
  it('should basically work', function () {
    checkPrettyPatch(Patch.assert([1, 2]).drop(),
    		     [' ::: nothing'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2]).drop(),
    		     [' ::: nothing'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2], 1).drop(),
    		     [' observe<1> <2> 1 2 {true}'],
    		     [' ::: nothing']);
    checkPrettyPatch(Patch.assert([1, 2]).andThen(Patch.assert(Patch.atMeta([1, 2]))).drop(),
    		     [' <2> 1 2 {true}'],
    		     [' ::: nothing']);
  });
});
