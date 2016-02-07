// DOM fragment display driver
var Patch = require("./patch.js");
var DemandMatcher = require('./demand-matcher.js').DemandMatcher;
var Ack = require('./ack.js').Ack;
var Seal = require('./seal.js').Seal;

var Network_ = require("./network.js");
var Network = Network_.Network;
var __ = Network_.__;
var _$ = Network_._$;

function spawnDOMDriver(domWrapFunction, jQueryWrapFunction) {
  domWrapFunction = domWrapFunction || defaultWrapFunction;
  var spec = domWrapFunction(_$('selector'), _$('fragmentClass'), _$('fragmentSpec'));
  Network.spawn(
    new DemandMatcher(spec,
		      Patch.advertise(spec),
		      {
			onDemandIncrease: function (c) {
			  Network.spawn(new DOMFragment(c.selector,
							c.fragmentClass,
							c.fragmentSpec,
							domWrapFunction,
							jQueryWrapFunction));
			}
		      }));
}

function defaultWrapFunction(selector, fragmentClass, fragmentSpec) {
  return ["DOM", selector, fragmentClass, fragmentSpec];
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

  Network.spawn(new Network(function () {
    Syndicate.JQuery.spawnJQueryDriver(self.selector+" > ."+self.fragmentClass,
				       1,
				       self.jQueryWrapFunction);
    Network.spawn({
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
	  Network.exitNetwork();
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
    Network.exit();
  }
};

///////////////////////////////////////////////////////////////////////////

function isAttributes(x) {
  return Array.isArray(x) && ((x.length === 0) || Array.isArray(x[0]));
}

DOMFragment.prototype.interpretSpec = function (spec) {
  // Fragment specs are roughly JSON-equivalents of SXML.
  // spec ::== ["tag", [["attr", "value"], ...], spec, spec, ...]
  //         | ["tag", spec, spec, ...]
  //         | "cdata"
  if (typeof(spec) === "string" || typeof(spec) === "number") {
    return document.createTextNode(spec);
  } else if ($.isArray(spec)) {
    var tagName = spec[0];
    var hasAttrs = isAttributes(spec[1]);
    var attrs = hasAttrs ? spec[1] : {};
    var kidIndex = hasAttrs ? 2 : 1;

    // Wow! Such XSS! Many hacks! So vulnerability! Amaze!
    var n = document.createElement(tagName);
    for (var i = 0; i < attrs.length; i++) {
      n.setAttribute(attrs[i][0], attrs[i][1]);
    }
    for (var i = kidIndex; i < spec.length; i++) {
      n.appendChild(this.interpretSpec(spec[i]));
    }
    return n;
  } else {
    throw new Error("Ill-formed DOM specification");
  }
};

DOMFragment.prototype.buildNodes = function () {
  var self = this;
  var nodes = [];
  $(self.selector).each(function (index, domNode) {
    var n = self.interpretSpec(self.fragmentSpec.sealContents);
    n.classList.add(self.fragmentClass);
    domNode.appendChild(n);
    nodes.push(n);
  });
  return nodes;
};

///////////////////////////////////////////////////////////////////////////

module.exports.spawnDOMDriver = spawnDOMDriver;
module.exports.defaultWrapFunction = defaultWrapFunction;
