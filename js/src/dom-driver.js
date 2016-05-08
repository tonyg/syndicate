// DOM fragment display driver
var Patch = require("./patch.js");
var DemandMatcher = require('./demand-matcher.js').DemandMatcher;
var Route = require('./route.js');
var Ack = require('./ack.js').Ack;
var Seal = require('./seal.js').Seal;

var Dataspace_ = require("./dataspace.js");
var Dataspace = Dataspace_.Dataspace;
var __ = Dataspace_.__;
var _$ = Dataspace_._$;

var DOM = Route.makeStructureConstructor('DOM', ['selector', 'fragmentClass', 'fragmentSpec']);

function spawnDOMDriver(domWrapFunction, jQueryWrapFunction) {
  domWrapFunction = domWrapFunction || DOM;
  var spec = domWrapFunction(_$('selector'), _$('fragmentClass'), _$('fragmentSpec'));
  Dataspace.spawn(
    new DemandMatcher(spec,
		      Patch.advertise(spec), // TODO: are the embedded captures problematic here? If not, why not?
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
    if ('classList' in n) {
      n.classList.add(self.fragmentClass);
    }
    domNode.appendChild(n);
    nodes.push(n);
  });
  return nodes;
};

///////////////////////////////////////////////////////////////////////////

module.exports.spawnDOMDriver = spawnDOMDriver;
module.exports.DOM = DOM;
