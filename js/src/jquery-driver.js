// JQuery event driver
var Patch = require("./patch.js");
var DemandMatcher = require('./demand-matcher.js').DemandMatcher;

var Network_ = require("./network.js");
var Network = Network_.Network;
var __ = Network_.__;
var _$ = Network_._$;

function spawnJQueryDriver(baseSelector, metaLevel, wrapFunction) {
  metaLevel = metaLevel || 0;
  wrapFunction = wrapFunction || defaultWrapFunction;
  Network.spawn(
    new DemandMatcher(Patch.observe(wrapFunction(_$('selector'), _$('eventName'), __)),
		      Patch.advertise(wrapFunction(_$('selector'), _$('eventName'), __)),
		      {
			metaLevel: metaLevel,
			onDemandIncrease: function (c) {
			  Network.spawn(new JQueryEventRouter(baseSelector,
							      c.selector,
							      c.eventName,
							      metaLevel,
							      wrapFunction));
			}
		      }));
}

function defaultWrapFunction(selector, eventName, eventValue) {
  return ["jQuery", selector, eventName, eventValue];
}

function JQueryEventRouter(baseSelector, selector, eventName, metaLevel, wrapFunction) {
  var self = this;
  this.baseSelector = baseSelector || null;
  this.selector = selector;
  this.eventName = eventName;
  this.metaLevel = metaLevel || 0;
  this.wrapFunction = wrapFunction || defaultWrapFunction;
  this.preventDefault = (this.eventName.charAt(0) !== "+");
  this.handler =
    Network.wrap(function (e) {
      Network.send(self.wrapFunction(self.selector, self.eventName, e), self.metaLevel);
      if (self.preventDefault) e.preventDefault();
      return !self.preventDefault;
    });
  this.computeNodes().on(this.preventDefault ? this.eventName : this.eventName.substring(1),
			 this.handler);
}

JQueryEventRouter.prototype.boot = function () {
  return Patch.pub(this.wrapFunction(this.selector, this.eventName, __), this.metaLevel)
    .andThen(Patch.sub(Patch.observe(this.wrapFunction(this.selector, this.eventName, __)),
		       this.metaLevel));
};

JQueryEventRouter.prototype.handleEvent = function (e) {
  if (e.type === "stateChange" && e.patch.hasRemoved()) {
    this.computeNodes().off(this.eventName, this.handler);
    Network.exit();
  }
};

JQueryEventRouter.prototype.computeNodes = function () {
  if (this.baseSelector) {
    return $(this.baseSelector).children(this.selector).addBack(this.selector);
  } else {
    return $(this.selector);
  }
};

function simplifyDOMEvent(e) {
  var keys = [];
  for (var k in e) {
    var v = e[k];
    if (typeof v === 'object') continue;
    if (typeof v === 'function') continue;
    keys.push(k);
  }
  keys.sort();
  var simplified = [];
  for (var i = 0; i < keys.length; i++) {
    simplified.push([keys[i], e[keys[i]]]);
  }
  return simplified;
}

///////////////////////////////////////////////////////////////////////////

module.exports.spawnJQueryDriver = spawnJQueryDriver;
module.exports.simplifyDOMEvent = simplifyDOMEvent;
module.exports.defaultWrapFunction = defaultWrapFunction;
