// DOM fragment display driver
var Patch = require("./patch.js");
var DemandMatcher = require('./demand-matcher.js').DemandMatcher;
var Struct = require('./struct.js');
var Ack = require('./ack.js').Ack;

var Dataspace_ = require("./dataspace.js");
var Dataspace = Dataspace_.Dataspace;
var __ = Dataspace_.__;
var _$ = Dataspace_._$;

var DOM = Struct.makeConstructor('DOM', ['selector', 'fragmentClass', 'fragmentSpec']);

function spawnDOMDriver(domWrapFunction, jQueryWrapFunction) {
  domWrapFunction = domWrapFunction || DOM;
  var spec = domWrapFunction(_$('selector'), _$('fragmentClass'), _$('fragmentSpec'));
  Dataspace.spawn(
    new DemandMatcher([spec],
		      [Patch.advertise(spec)],
		      {
			onDemandIncrease: function (c) {
			  Dataspace.spawn(new DOMFragment(c.selector,
							  c.fragmentClass,
							  c.fragmentSpec,
							  domWrapFunction,
							  jQueryWrapFunction));
			}
		      }));
}

function DOMFragment(selector, fragmentClass, fragmentSpec, domWrapFunction, jQueryWrapFunction) {
  this.selector = selector;
  this.fragmentClass = fragmentClass;
  this.fragmentSpec = fragmentSpec;
  this.domWrapFunction = domWrapFunction;
  this.jQueryWrapFunction = jQueryWrapFunction;
  this.demandExists = false;
  this.subscriptionEstablished = new Ack();
  this.nodes = this.buildNodes();
}

DOMFragment.prototype.boot = function () {
  var self = this;
  var specification = self.domWrapFunction(self.selector, self.fragmentClass, self.fragmentSpec);

  Dataspace.spawn(new Dataspace(function () {
    Syndicate.JQuery.spawnJQueryDriver(self.selector+" > ."+self.fragmentClass,
				       1,
				       self.jQueryWrapFunction);
    Dataspace.spawn({
      demandExists: false,
      subscriptionEstablished: new Ack(1),
      boot: function () {
	this.subscriptionEstablished.arm();
	return Patch.sub(Patch.advertise(specification), 1);
      },
      handleEvent: function (e) {
	this.subscriptionEstablished.check(e);
	if (e.type === "stateChange") {
	  if (e.patch.hasAdded()) this.demandExists = true;
	  if (e.patch.hasRemoved()) this.demandExists = false;
	}
	if (this.subscriptionEstablished.done && !this.demandExists) {
	  Dataspace.exitDataspace();
	}
      }
    });
  }));

  this.subscriptionEstablished.arm();
  return Patch.sub(specification).andThen(Patch.pub(specification));
};

DOMFragment.prototype.handleEvent = function (e) {
  this.subscriptionEstablished.check(e);
  if (e.type === "stateChange") {
    if (e.patch.hasAdded()) this.demandExists = true;
    if (e.patch.hasRemoved()) this.demandExists = false;
  }
  if (this.subscriptionEstablished.done && !this.demandExists) {
    for (var i = 0; i < this.nodes.length; i++) {
      var n = this.nodes[i];
      n.parentNode.removeChild(n);
    }
    Dataspace.exit();
  }
};

///////////////////////////////////////////////////////////////////////////

DOMFragment.prototype.buildNodes = function () {
  var self = this;
  var nodes = [];
  $(self.selector).each(function (index, domNode) {
    if (typeof self.fragmentSpec !== 'string') {
      throw new Error("DOM fragmentSpec not a string: " + JSON.stringify(self.fragmentSpec));
    }
    var newNodes = $('<div>' + self.fragmentSpec + '</div>')[0].childNodes;
    // This next loop looks SUPER SUSPICIOUS. What is happening is
    // that each time we call domNode.appendChild(n), where n is an
    // element of the NodeList newNodes, the DOM is **removing** n
    // from the NodeList in order to place it in its new parent. So,
    // each call to appendChild shrinks the NodeList by one node until
    // it is finally empty, and its length property yields zero.
    while (newNodes.length) {
      var n = newNodes[0];
      if ('classList' in n) {
        n.classList.add(self.fragmentClass);
      }
      domNode.appendChild(n);
      nodes.push(n);
    }
  });
  return nodes;
};

///////////////////////////////////////////////////////////////////////////

module.exports.spawnDOMDriver = spawnDOMDriver;
module.exports.DOM = DOM;
