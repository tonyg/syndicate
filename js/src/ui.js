"use strict";
// UI (DOM + event) support for Syndicate
//
// The previous dom-driver.js + jquery-driver.js approach worked kind
// of OK, but started to fall down in a couple of areas: Added UI
// fragments lacked identity, so would sometimes move around the tree
// unexpectedly as they were updated; and there was no convenient
// means of scoping event selectors to within a particular UI
// fragment, despite various attempts at this.
//
// The design of this module aims to take these lessons into account.

var Immutable = require('immutable');
var Patch = require("./patch.js");
var Trie = require("./trie.js");
var DemandMatcher = require('./demand-matcher.js').DemandMatcher;
var Struct = require('./struct.js');
var RandomID = require('./randomid.js');

var Dataspace_ = require("./dataspace.js");
var Dataspace = Dataspace_.Dataspace;
var __ = Dataspace_.__;
var _$ = Dataspace_._$;

///////////////////////////////////////////////////////////////////////////
// Protocol

// Message. Interest in this causes event listeners to be added for
// the given eventType to all nodes matching the given selector *at
// the time of the subscription*. As nodes *from this library* come
// and go, they will have event handlers installed and removed as
// well. WARNING: The simple implementation below currently scans the
// whole document anytime a change is signalled; in future, it may not
// do such a scan.
var globalEvent = Struct.makeConstructor('globalEvent', ['selector', 'eventType', 'event']);

// Message. Like globalEvent, but applies only within the scope of the
// UI fragment identified.
var uiEvent = Struct.makeConstructor('uiEvent', ['fragmentId', 'selector', 'eventType', 'event']);

// Assertion. Causes the setup of DOM nodes corresponding to the given
// HTML fragment, as immediate children of all nodes named by the
// given selector that exist at the time of assertion.
var uiFragment = Struct.makeConstructor('uiFragment', ['fragmentId', 'selector', 'html']);

// Assertion. Asserted by respondent to a given uiFragment.
var uiFragmentExists = Struct.makeConstructor('uiFragmentExists', ['fragmentId']);

///////////////////////////////////////////////////////////////////////////
// ID allocators

var moduleInstance = RandomID.randomId(16, true);

var nextFragmentIdNumber = 0;
function newFragmentId() {
  return 'ui_' + moduleInstance + '_' + (nextFragmentIdNumber++);
}

///////////////////////////////////////////////////////////////////////////

function spawnUIDriver() {
  var globalEventProj = globalEvent(_$('selector'), _$('eventType'), __);
  Dataspace.spawn(
    new DemandMatcher([Patch.observe(globalEventProj)],
                      [Patch.advertise(globalEventProj)],
                      {
                        onDemandIncrease: function (c) {
                          Dataspace.spawn(new GlobalEventSupply(c.selector, c.eventType));
                        }
                      }));

  Dataspace.spawn(
    new DemandMatcher([uiFragment(_$('fragmentId'), __, __)],
                      [uiFragmentExists(_$('fragmentId'))],
                      {
                        onDemandIncrease: function (c) {
                          Dataspace.spawn(new UIFragment(c.fragmentId));
                        }
                      }));
}

///////////////////////////////////////////////////////////////////////////

function GlobalEventSupply(selector, eventType) {
  this.selector = selector;
  this.eventType = eventType;
  this.demandPat = Patch.observe(globalEvent(this.selector, this.eventType, __));
}

GlobalEventSupply.prototype.boot = function () {
  var self = this;
  this.handlerClosure = Dataspace.wrap(function(e) { return self.handleDomEvent(e); });
  this.updateEventListeners(true);

  return Patch.sub(this.demandPat) // track demand
    .andThen(Patch.sub(uiFragmentExists(__))) // track new fragments
    .andThen(Patch.pub(globalEvent(this.selector, this.eventType, __))) // indicate our presence
  ;
};

GlobalEventSupply.prototype.updateEventListeners = function (install) {
  var nodes = document.querySelectorAll(this.selector);
  for (var i = 0; i < nodes.length; i++) {
    var n = nodes[i];
    // addEventListener and removeEventListener are apparently idempotent.
    if (install) {
      n.addEventListener(cleanEventType(this.eventType), this.handlerClosure);
    } else {
      n.removeEventListener(cleanEventType(this.eventType), this.handlerClosure);
    }
  }
};

GlobalEventSupply.prototype.trapexit = function () {
  console.log('GlobalEventSupply trapexit running', this.selector, this.eventType);
  this.updateEventListeners(false);
};

GlobalEventSupply.prototype.handleDomEvent = function (event) {
  Dataspace.send(globalEvent(this.selector, this.eventType, event));
  return dealWithPreventDefault(this.eventType, event);
};

GlobalEventSupply.prototype.handleEvent = function (e) {
  this.updateEventListeners(true);
  // TODO: don't be so crude about this ^. On the one hand, this lets
  // us ignore uiFragmentExists records coming and going; on the other
  // hand, we do potentially a lot of redundant work.
  if (e.type === 'stateChange' && e.patch.project(this.demandPat).hasRemoved()) {
    Dataspace.exit(); // trapexit will uninstall event listeners
  }
};

///////////////////////////////////////////////////////////////////////////

function UIFragment(fragmentId) {
  this.fragmentId = fragmentId;
  this.demandProj = uiFragment(this.fragmentId, _$('selector'), _$('html'));
  this.eventDemandProj =
    Patch.observe(uiEvent(this.fragmentId, _$('selector'), _$('eventType'), __));

  this.currentAnchorNodes = [];
  this.currentSelector = null;
  this.currentHtml = null;

  this.currentEventRegistrations = Immutable.Map();
  // ^ Map from (Map of selector/eventType) to closure.
}

UIFragment.prototype.boot = function () {
  return Patch.sub(Trie.projectionToPattern(this.demandProj)) // track demand
    .andThen(Patch.assert(uiFragmentExists(this.fragmentId))) // assert presence
    .andThen(Patch.sub(Trie.projectionToPattern(this.eventDemandProj)))
    // ^ track demand for fragment-specific events
  ;
};

UIFragment.prototype.trapexit = function () {
  console.log('UIFragment trapexit running', this.fragmentId);
  this.updateContent(null, null);
};

function brandNode(n, fragmentId, brandValue) {
  if ('dataset' in n) {
    // html element nodes etc.
    n.dataset[fragmentId] = brandValue;
  } else {
    // text nodes, svg nodes, etc etc.
    n[fragmentId] = brandValue;
  }
}

function getBrand(n, fragmentId) {
  if ('dataset' in n && n.dataset[fragmentId]) return n.dataset[fragmentId];
  if (n[fragmentId]) return n[fragmentId];
  return null;
}

function findInsertionPoint(n, fragmentId) {
  for (var i = 0; i < n.childNodes.length; i++) {
    var c = n.childNodes[i];
    if (getBrand(c, fragmentId)) return c;
  }
  return null;
}

function htmlToNodes(html) {
  var e = document.createElement('arbitrarycontainer');
  e.innerHTML = html;
  return Array.prototype.slice.call(e.childNodes);
}

UIFragment.prototype.updateContent = function (newSelector, newHtml) {
  var self = this;
  var newBrand = '' + (Date.now());

  var newAnchors = (newSelector !== null)
      ? Array.prototype.slice.call(document.querySelectorAll(newSelector))
      : [];

  newAnchors.forEach(function (anchorNode) {
    var insertionPoint = findInsertionPoint(anchorNode, self.fragmentId);
    htmlToNodes(newHtml).forEach(function (newNode) {
      brandNode(newNode, self.fragmentId, newBrand);
      anchorNode.insertBefore(newNode, insertionPoint);
    });
  });

  self.currentAnchorNodes.forEach(function (anchorNode) {
    var insertionPoint = findInsertionPoint(anchorNode, self.fragmentId);
    while (insertionPoint) {
      var nextNode = insertionPoint.nextSibling;
      var b = getBrand(insertionPoint, self.fragmentId);
      if (!b) break; // we know all our-brand nodes will be adjacent
      if (b !== newBrand) {
        insertionPoint.parentNode.removeChild(insertionPoint);
      }
      insertionPoint = nextNode;
    }
  });

  self.currentAnchorNodes = newAnchors;
  self.currentSelector = newSelector;
  self.currentHtml = newHtml;

  self.currentEventRegistrations.forEach(function (_handlerClosure, key) {
    self.updateEventListeners(key.toObject(), true); // (re)install event listeners
  });
};

UIFragment.prototype.handleEvent = function (e) {
  var self = this;

  if (e.type === 'stateChange') {
    var fragmentChanges = e.patch.projectObjects(self.demandProj);
    fragmentChanges[0].forEach(function (c) { self.updateContent(c.selector, c.html); });
    fragmentChanges[1].forEach(function (c) {
      if (c.selector === self.currentSelector && c.html === self.currentHtml) {
        Dataspace.exit(); // trapexit will remove nodes
      }
    });

    var eventDemand = e.patch.projectObjects(self.eventDemandProj);
    eventDemand[0].forEach(function (c) { self.updateEventListeners(c, true); })
    eventDemand[1].forEach(function (c) { self.updateEventListeners(c, false); })
  }
};

UIFragment.prototype.getEventClosure = function (c) {
  var self = this;
  var key = Immutable.Map(c);
  if (!self.currentEventRegistrations.has(key)) {
    var handlerClosure = Dataspace.wrap(function (e) { return self.handleDomEvent(c, e); });
    self.currentEventRegistrations = self.currentEventRegistrations.set(key, handlerClosure);
  }
  return self.currentEventRegistrations.get(key);
};

UIFragment.prototype.clearEventClosure = function (c) {
  var key = Immutable.Map(c);
  this.currentEventRegistrations = this.currentEventRegistrations.remove(key);
};

UIFragment.prototype.updateEventListeners = function (c, install) {
  var self = this;
  var handlerClosure = self.getEventClosure(c);

  self.currentAnchorNodes.forEach(function (anchorNode) {
    var uiNode = findInsertionPoint(anchorNode, self.fragmentId);
    while (uiNode && getBrand(uiNode, self.fragmentId)) {
      if ('querySelectorAll' in uiNode) {
        var nodes = uiNode.querySelectorAll(c.selector);
        for (var i = 0; i < nodes.length; i++) {
          var n = nodes[i];
          // addEventListener and removeEventListener are apparently idempotent.
          if (install) {
            n.addEventListener(cleanEventType(c.eventType), handlerClosure);
          } else {
            n.removeEventListener(cleanEventType(c.eventType), handlerClosure);
          }
        }
      }
      uiNode = uiNode.nextSibling;
    }
  });

  if (!install) {
    this.clearEventClosure(c);
  }
};

UIFragment.prototype.handleDomEvent = function (c, e) {
  Dataspace.send(uiEvent(this.fragmentId, c.selector, c.eventType, e));
  return dealWithPreventDefault(c.eventType, e);
};

///////////////////////////////////////////////////////////////////////////

function escapeDataAttributeName(s) {
  // Per https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/dataset,
  // the rules seem to be:
  //
  // 1. Must not contain a dash immediately followed by an ASCII lowercase letter
  // 2. Must not contain anything other than:
  //     - letters
  //     - numbers
  //     - dash, dot, colon, underscore
  //
  // I'm not implementing this exactly - I'm escaping some things that
  // don't absolutely need escaping, because it's simpler and I don't
  // yet need to undo this transformation.

  var result = '';
  for (var i = 0; i < s.length; i++) {
    var c = s[i];
    if (c >= 'a' && c <= 'z') { result = result + c; continue; }
    if (c >= 'A' && c <= 'Z') { result = result + c; continue; }
    if (c >= '0' && c <= '9') { result = result + c; continue; }
    if (c === '.' || c === ':') { result = result + c; continue; }

    c = c.charCodeAt(0);
    result = result + '_' + c + '_';
  }
  return result;
}

function dealWithPreventDefault(eventType, event) {
  var shouldPreventDefault = eventType.charAt(0) !== '+';
  if (shouldPreventDefault) event.preventDefault();
  return !shouldPreventDefault;
}

function cleanEventType(eventType) {
  return (eventType.charAt(0) === '+') ? eventType.slice(1) : eventType;
}

///////////////////////////////////////////////////////////////////////////

function Anchor(explicitFragmentId) {
  this.fragmentId =
    (typeof explicitFragmentId === 'undefined') ? newFragmentId() : explicitFragmentId;
  this.htmlPattern = uiFragment(this.fragmentId, __, __);
  this.eventPattern = uiEvent(this.fragmentId, __, __, __);
}

Anchor.prototype.context = function (/* ... */) {
  var extn = Array.prototype.slice.call(arguments).map(escapeDataAttributeName).join('__');
  return new Anchor(this.fragmentId + '__' + extn);
};

Anchor.prototype.html = function (selector, html) {
  return uiFragment(this.fragmentId, selector, html);
};

Anchor.prototype.event = function (selector, eventType, event) {
  return uiEvent(this.fragmentId, selector, eventType, event);
};

///////////////////////////////////////////////////////////////////////////

module.exports.newFragmentId = newFragmentId;
module.exports.spawnUIDriver = spawnUIDriver;
module.exports.Anchor = Anchor;
module.exports.globalEvent = globalEvent;
module.exports.uiEvent = uiEvent;
module.exports.uiFragment = uiFragment;
module.exports.uiFragmentExists = uiFragmentExists;
