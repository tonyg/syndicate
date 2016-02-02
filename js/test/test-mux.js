var expect = require('expect.js');
var Immutable = require('immutable');

var Route = require('../src/route.js');
var Patch = require('../src/patch.js');
var Mux = require('../src/mux.js');

var __ = Route.__;
var _$ = Route._$;

function checkPrettyTrie(m, expected) {
  expect(Route.prettyTrie(m)).to.equal(expected.join('\n'));
}

function checkPrettyPatch(p, expectedAdded, expectedRemoved) {
  expect(Patch.prettyPatch(p)).to.equal(
    ('<<<<<<<< Removed:\n' + expectedRemoved.join('\n') +
     '======== Added:\n' + expectedAdded.join('\n') +
     '>>>>>>>>\n'));
}

describe('mux stream', function () {
  function getM() {
    var m = new Mux.Mux();
    expect(m.addStream(Patch.assert(1).andThen(Patch.assert(2))).pid).to.equal(0);
    expect(m.addStream(Patch.assert(3).andThen(Patch.assert(2))).pid).to.equal(1);
    return m;
  }

  describe('addition', function () {
    it('should union interests appropriately', function () {
      var m = getM();
      checkPrettyTrie(m.routingTable, [' 1 >{[0]}',
				       ' 2 >{[0,1]}',
				       ' 3 >{[1]}']);
      checkPrettyTrie(m.interestsOf(0), [' 1 >{[0]}',
					 ' 2 >{[0]}']);
      checkPrettyTrie(m.interestsOf(1), [' 2 >{[1]}',
					 ' 3 >{[1]}']);
    });
  });

  describe('update', function () {
    it('should update interests appropriately', function () {
      var rawPatch =
	  Patch.assert(1)
	  .andThen(Patch.retract(2))
	  .andThen(Patch.retract(3))
	  .andThen(Patch.assert(4))
	  .andThen(Patch.retract(99));
      checkPrettyPatch(rawPatch,
		       [' 1 >{true}',
		        ' 4 >{true}'],
		       [' 2 >{true}',
		        ' 3 >{true}',
		        ' 99 >{true}']);

      var m = getM();
      var updateStreamResult = m.updateStream(1, rawPatch);
      expect(updateStreamResult.pid).to.equal(1);
      checkPrettyPatch(updateStreamResult.delta,
		       [' 1 >{[1]}',
			' 4 >{[1]}'],
		       [' 2 >{[1]}',
			' 3 >{[1]}']);
      checkPrettyTrie(m.routingTable, [' 1 >{[0,1]}',
				       ' 2 >{[0]}',
				       ' 4 >{[1]}']);
      checkPrettyTrie(m.interestsOf(0), [' 1 >{[0]}',
					 ' 2 >{[0]}']);
      checkPrettyTrie(m.interestsOf(1), [' 1 >{[1]}',
					 ' 4 >{[1]}']);
      checkPrettyPatch(updateStreamResult.deltaAggregate,
		       [' 4 >{[1]}'],
		       [' 3 >{[1]}']);
    });
  });
});

