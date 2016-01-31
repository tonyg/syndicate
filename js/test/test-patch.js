var expect = require('expect.js');
var Immutable = require('immutable');

var Route = require('../src/route.js');
var Patch = require('../src/patch.js');

var __ = Route.__;
var _$ = Route._$;

function checkPrettyTrie(m, expected) {
  expect(r.prettyTrie(m)).to.equal(expected.join('\n'));
}

function checkPrettyPatch(p, expectedAdded, expectedRemoved) {
  expect(Patch.prettyPatch(p)).to.equal(
    ('<<<<<<<< Removed:\n' + expectedRemoved.join('\n') +
     '======== Added:\n' + expectedAdded.join('\n') +
     '>>>>>>>>\n'));
}

describe('basic patch compilation', function () {
  it('should print as expected', function () {
    checkPrettyPatch(Patch.assert([1, 2]),
    		     [' < 1 2 > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.assert(__),
    		     [' ★ >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.sub(__),
    		     [' < $Observe ★ > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2]),
    		     [' < $Observe < 1 2 > > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.pub('x'),
    		     [' < $Advertise "x" > >{true}'],
    		     ['::: nothing']);
  });

  it('should work at nonzero metalevel', function () {
    checkPrettyPatch(Patch.assert([1, 2], 0),
    		     [' < 1 2 > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.assert([1, 2], 1),
    		     [' < $AtMeta < 1 2 > > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.assert([1, 2], 2),
    		     [' < $AtMeta < $AtMeta < 1 2 > > > >{true}'],
    		     ['::: nothing']);

    checkPrettyPatch(Patch.sub([1, 2], 0),
    		     [' < $Observe < 1 2 > > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2], 1),
    		     [' < $AtMeta < $Observe < 1 2 > > > >{true}',
		      '   $Observe < $AtMeta < 1 2 > > > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2], 2),
    		     [' < $AtMeta < $AtMeta < $Observe < 1 2 > > > > >{true}',
		      '             $Observe < $AtMeta < 1 2 > > > > >{true}',
		      '   $Observe < $AtMeta < $AtMeta < 1 2 > > > > >{true}'],
    		     ['::: nothing']);
  });
});

describe('patch sequencing', function () {
  it('should do the right thing in simple cases', function () {
    checkPrettyPatch(Patch.assert(__).andThen(Patch.retract(3)),
    		     [' ★ >{true}',
		      ' 3::: nothing'],
    		     [' 3 >{true}']);
    checkPrettyPatch(Patch.assert(3).andThen(Patch.retract(__)),
    		     ['::: nothing'],
    		     [' ★ >{true}']);
    checkPrettyPatch(Patch.assert(__).andThen(Patch.retract(__)),
    		     ['::: nothing'],
    		     [' ★ >{true}']);
    checkPrettyPatch(Patch.assert(3).andThen(Patch.retract(3)),
    		     ['::: nothing'],
		     [' 3 >{true}']);
    checkPrettyPatch(Patch.sub([1, __]).andThen(Patch.unsub([1, 2])),
    		     [' < $Observe < 1 ★ > > >{true}',
		      '                2::: nothing'],
    		     [' < $Observe < 1 2 > > >{true}']);
    checkPrettyPatch(Patch.sub([__, 2]).andThen(Patch.unsub([1, 2])),
    		     [' < $Observe < ★ 2 > > >{true}',
		      '              1::: nothing'],
    		     [' < $Observe < 1 2 > > >{true}']);
    checkPrettyPatch(Patch.sub([__, __]).andThen(Patch.unsub([1, 2])),
    		     [' < $Observe < ★ ★ > > >{true}',
		      '              1 ★ > > >{true}',
		      '                2::: nothing'],
    		     [' < $Observe < 1 2 > > >{true}']);
  });
});

describe('patch lifting', function () {
  it('should basically work', function () {
    checkPrettyPatch(Patch.assert([1, 2]).lift(),
    		     [' < $AtMeta < 1 2 > > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2]).lift(),
    		     [' < $AtMeta < $Observe < 1 2 > > > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.assert([1, 2]).andThen(Patch.assert(Patch.atMeta([1, 2]))).lift(),
    		     [' < $AtMeta < $AtMeta < 1 2 > > > >{true}',
		      '             1 2 > > >{true}'],
    		     ['::: nothing']);
  });
});

describe('patch dropping', function () {
  it('should basically work', function () {
    checkPrettyPatch(Patch.assert([1, 2]).drop(),
    		     ['::: nothing'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2]).drop(),
    		     ['::: nothing'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.sub([1, 2], 1).drop(),
    		     [' < $Observe < 1 2 > > >{true}'],
    		     ['::: nothing']);
    checkPrettyPatch(Patch.assert([1, 2]).andThen(Patch.assert(Patch.atMeta([1, 2]))).drop(),
    		     [' < 1 2 > >{true}'],
    		     ['::: nothing']);
  });
});
