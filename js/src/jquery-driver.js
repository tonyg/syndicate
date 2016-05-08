// JQuery event driver
var Patch = require("./patch.js");
var DemandMatcher = require('./demand-matcher.js').DemandMatcher;
var Route = require('./route.js');

var Dataspace_ = require("./dataspace.js");
var Dataspace = Dataspace_.Dataspace;
var __ = Dataspace_.__;
var _$ = Dataspace_._$;

var jQueryEvent = Route.makeStructureConstructor('jQueryEvent', ['selector', 'eventName', 'eventValue']);

function spawnJQueryDriver(baseSelector, metaLevel, wrapFunction) {
  metaLevel = metaLevel || 0;
  wrapFunction = wrapFunction || jQueryEvent;
  Dataspace.spawn(
    new DemandMatcher(Patch.observe(wrapFunction(_$('selector'), _$('eventName'), __)),
		      Patch.advertise(wrapFunction(_$('selector'), _$('eventName'), __)),
		      {
			metaLevel: metaLevel,
			onDemandIncrease: function (c) {
			  Dataspace.spawn(new JQueryEventRouter(baseSelector,
							        c.selector,
							        c.eventName,
							        metaLevel,
							        wrapFunction));
			}
		      }));
}

function JQueryEventRouter(baseSelector, selector, eventName, metaLevel, wrapFunction) {
  var self = this;
  this.baseSelector = baseSelector || null;
  this.selector = selector;
  this.eventName = eventName;
  this.metaLevel = metaLevel || 0;
  this.wrapFunction = wrapFunction || jQueryEvent;
  this.preventDefault = (this.eventName.charAt(0) !== "+");
  this.handler =
    Dataspace.wrap(function (e) {
      Dataspace.send(self.wrapFunction(self.selector, self.eventName, e), self.metaLevel);
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
    Dataspace.exit();
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
module.exports.jQueryEvent = jQueryEvent;
