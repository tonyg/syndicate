"use strict";

var expect = require('expect.js');
var Immutable = require('immutable');

var Trie = require('../src/trie.js');
var Patch = require('../src/patch.js');
var Mux = require('../src/mux.js');

var __ = Trie.__;
var _$ = Trie._$;

function checkPrettyTrie(m, expected) {
  expect(Trie.prettyTrie(m)).to.equal(expected.join('\n'));
}

function checkPrettyPatch(p, expectedAdded, expectedRemoved) {
  expect(p.pretty()).to.equal(
    ('\n-' + expectedRemoved.join('\n-') +
     '\n+' + expectedAdded.join('\n+') + '\n'));
}

function getM() {
  var m = new Mux.Mux();
  expect(m.addStream(Patch.assert(1).andThen(Patch.assert(2))).pid).to.equal(0);
  expect(m.addStream(Patch.assert(3).andThen(Patch.assert(2))).pid).to.equal(1);
  return m;
}

describe('mux stream', function () {
  describe('addition', function () {
    it('should union interests appropriately', function () {
      var m = getM();
      checkPrettyTrie(m.routingTable, [' 1 {[0]}',
				       ' 2 {[0,1]}',
				       ' 3 {[1]}']);
      checkPrettyTrie(m.interestsOf(0), [' 1 {[0]}',
					 ' 2 {[0]}']);
      checkPrettyTrie(m.interestsOf(1), [' 2 {[1]}',
					 ' 3 {[1]}']);
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
		       [' 1 {true}',
		        ' 4 {true}'],
		       [' 2 {true}',
		        ' 3 {true}',
		        ' 99 {true}']);

      var m = getM();
      var updateStreamResult = m.updateStream(1, rawPatch);
      expect(updateStreamResult.pid).to.equal(1);
      checkPrettyPatch(updateStreamResult.delta,
		       [' 1 {[1]}',
			' 4 {[1]}'],
		       [' 2 {[1]}',
			' 3 {[1]}']);
      checkPrettyTrie(m.routingTable, [' 1 {[0,1]}',
				       ' 2 {[0]}',
				       ' 4 {[1]}']);
      checkPrettyTrie(m.interestsOf(0), [' 1 {[0]}',
					 ' 2 {[0]}']);
      checkPrettyTrie(m.interestsOf(1), [' 1 {[1]}',
					 ' 4 {[1]}']);
      checkPrettyPatch(updateStreamResult.deltaAggregate,
		       [' 4 {[1]}'],
		       [' 3 {[1]}']);
    });
  });

  describe('removal', function () {
    it('should remove streams properly', function () {
      var m = getM();
      var updateStreamResult = m.removeStream(1);
      expect(updateStreamResult.pid).to.equal(1);
      checkPrettyPatch(updateStreamResult.delta,
		       [' ::: nothing'],
		       [' 2 {[1]}',
			' 3 {[1]}']);
      checkPrettyTrie(m.routingTable, [' 1 {[0]}',
				       ' 2 {[0]}']);
      checkPrettyTrie(m.interestsOf(0), [' 1 {[0]}',
					 ' 2 {[0]}']);
      checkPrettyTrie(m.interestsOf(1), [' ::: nothing']);
      checkPrettyPatch(updateStreamResult.deltaAggregate,
		       [' ::: nothing'],
		       [' 3 {[1]}']);
    });
  });
});

describe('computeEvents', function () {
  describe('for new assertions and existing specific interest', function () {
    var oldM = new Mux.Mux();
    oldM.addStream(Patch.sub(1));
    var newM = oldM.shallowCopy();
    var updateStreamResult = newM.addStream(Patch.assert(1).andThen(Patch.assert(2)));
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should yield just the assertion of interest', function () {
      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(0).strip().equals(Patch.assert(1))).to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);
    });
  });

  describe('for removed assertions', function () {
    var oldM = new Mux.Mux();
    oldM.addStream(Patch.sub([__]));
    var newM = oldM.shallowCopy();
    newM.addStream(Patch.assert([1]).andThen(Patch.assert([2])));
    var updateStreamResult = newM.updateStream(1, Patch.retract([1]));
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should yield just the specific retraction', function () {
      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(0).strip().equals(Patch.retract([1]))).to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);
    });
  });

  describe('for new assertions and existing general interest', function () {
    var oldM = new Mux.Mux();
    oldM.addStream(Patch.sub([__]));
    var newM = oldM.shallowCopy();
    var updateStreamResult = newM.addStream(Patch.assert([1]).andThen(Patch.assert([2])));
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should yield both new assertions', function () {
      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(0).strip().equals(Patch.assert([1]).andThen(Patch.assert([2]))))
	.to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);
    });
  })

  describe('for new specific interest and existing assertion', function () {
    var oldM = new Mux.Mux();
    oldM.addStream(Patch.assert(1).andThen(Patch.assert(2)));
    var newM = oldM.shallowCopy();
    var updateStreamResult = newM.addStream(Patch.sub(1));
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should yield just the assertion of interest', function () {
      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(1).strip().equals(Patch.assert(1))).to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);
    });
  });

  describe('for new general interest and existing assertion', function () {
    var oldM = new Mux.Mux();
    oldM.addStream(Patch.assert([1]).andThen(Patch.assert([2])));
    var newM = oldM.shallowCopy();
    var updateStreamResult = newM.addStream(Patch.sub([__]));
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should yield just the assertion of interest', function () {
      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(1).strip().equals(Patch.assert([1]).andThen(Patch.assert([2]))))
	.to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);
    });
  });

  describe('for removed general interest', function () {
    var oldM = new Mux.Mux();
    oldM.addStream(Patch.assert([1]).andThen(Patch.assert([2])));
    oldM.addStream(Patch.sub([__]));
    var newM = oldM.shallowCopy();
    var updateStreamResult = newM.updateStream(1, Patch.unsub([__]));
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should yield both retractions', function () {
      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(1).strip().equals(Patch.retract([1]).andThen(Patch.retract([2]))))
	.to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);
    });
  });

  describe('for removed specific interest', function () {
    it('should yield three appropriate events for three intercessions', function () {
      var oldM = new Mux.Mux();
      oldM.addStream(Patch.assert([1]).andThen(Patch.assert([2])));
      oldM.addStream(Patch.sub([__]));
      var newM = oldM.shallowCopy();
      var updateStreamResult = newM.updateStream(1, Patch.unsub([1]));
      var events = Mux.computeEvents(oldM, newM, updateStreamResult);

      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(1).strip().equals(Patch.retract([1]))).to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);

      oldM = newM;
      newM = oldM.shallowCopy();
      events = Mux.computeEvents(oldM, newM, newM.updateStream(1, Patch.unsub([2])));

      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(1).strip().equals(Patch.retract([2])))
	.to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);

      oldM = newM;
      newM = oldM.shallowCopy();
      events = Mux.computeEvents(oldM, newM, newM.updateStream(0, Patch.assert([3])));

      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(1).strip().equals(Patch.assert([3]))).to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);
    });
  });

  describe('for new meta assertion', function () {
    var oldM = new Mux.Mux();
    var newM = oldM.shallowCopy();
    var updateStreamResult = newM.addStream(Patch.assert(Patch.atMeta(1)).andThen(Patch.assert(2)));
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should yield no local events and one meta event', function () {
      expect(events.eventMap.size).to.be(0);
      expect(events.metaEvents.size).to.be(1);
      expect(events.metaEvents.get(0).strip().equals(Patch.assert(1))).to.be(true);
    });
  });

  describe('for adding supply and demand simultaneously', function () {
    var oldM = new Mux.Mux();
    var newM = oldM.shallowCopy();
    var updateStreamResult = newM.addStream(Patch.assert(1).andThen(Patch.sub(1)));
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should send a patch back', function () {
      expect(events.eventMap.size).to.be(1);
      expect(events.metaEvents.size).to.be(1);
      expect(events.eventMap.get(0).strip().equals(Patch.assert(1))).to.be(true);
      expect(events.metaEvents.get(0).equals(Patch.emptyPatch)).to.be(true);
    });
  });

  describe('for a no-op but nonempty patch', function () {
    var oldM = new Mux.Mux();
    var pid1 = oldM.addStream(Patch.assert(["fieldContents", "initial", 7])).pid;
    var pid2 = oldM.addStream(Patch.sub(["fieldContents", __, __])).pid;
    var newM = oldM.shallowCopy();
    var unclampedPatch =
	Patch.retract(["fieldContents", __, __])
	.andThen(Patch.assert(["fieldContents", "initial", 7]));
    var updateStreamResult = newM.updateStream(pid1, unclampedPatch);
    var events = Mux.computeEvents(oldM, newM, updateStreamResult);

    it('should send no patch to the subscriber', function () {
      expect(events.eventMap.size).to.be(0);
    });
  });
});
