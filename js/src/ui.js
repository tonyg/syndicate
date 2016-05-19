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
var globalEvent = Struct.makeConstructor('global-event', ['selector', 'eventType', 'event']);

// Message. As globalEvent, but instead of using a selector to choose
// target DOM nodes, attaches an event handler to the browser "window"
// object itself.
var windowEvent = Struct.makeConstructor('window-event', ['eventType', 'event']);

// Message. Like globalEvent, but applies only within the scope of the
// UI fragment identified.
var uiEvent = Struct.makeConstructor('ui-event', ['fragmentId', 'selector', 'eventType', 'event']);

// Assertion. Causes the setup of DOM nodes corresponding to the given
// HTML fragment, as immediate children of all nodes named by the
// given selector that exist at the time of assertion. The orderBy
// field should be null, a string, or a number. Fragments are ordered
// primarily by orderBy, and secondarily by fragmentId.
var uiFragment = Struct.makeConstructor('ui-fragment',
                                        ['fragmentId', 'selector', 'html', 'orderBy']);

// Assertion. Asserted by respondent to a given uiFragment.
var uiFragmentExists = Struct.makeConstructor('ui-fragment-exists', ['fragmentId']);

// Assertion. Causes the setup of DOM attributes on all nodes named by
// the given selector that exist at the time of assertion.
//
// NOTE: Attribute "class" is a special case: it treats the value of
// the attribute as a (string encoding of a) set. The given value is
// split on whitespace, and each piece is added to the set of things
// already present. (See the implementation for details.)
var uiAttribute = Struct.makeConstructor('ui-attribute', ['selector', 'attribute', 'value']);

// Assertion. Similar to uiAttribute, but for properties of DOM nodes.
var uiProperty = Struct.makeConstructor('ui-property', ['selector', 'property', 'value']);

// Messages.
// NOTE: These do not treat "class" specially!
var setAttribute = Struct.makeConstructor('set-ui-attribute', ['selector', 'attribute', 'value']);
var removeAttribute = Struct.makeConstructor('remove-ui-attribute', ['selector', 'attribute']);
var setProperty = Struct.makeConstructor('set-ui-property', ['selector', 'property', 'value']);
var removeProperty = Struct.makeConstructor('remove-ui-property', ['selector', 'property']);

// Assertion. Current "location hash" -- the "#/path/part" fragment at
// the end of window.location.
var locationHash = Struct.makeConstructor('locationHash', ['value']);

// Message. Causes window.location to be updated to have the given new
// "location hash" value.
var setLocationHash = Struct.makeConstructor('setLocationHash', ['value']);

///////////////////////////////////////////////////////////////////////////
// ID allocators

var moduleInstance = RandomID.randomId(16, true);

var nextFragmentIdNumber = 0;
function newFragmentId() {
  return 'ui_' + moduleInstance + '_' + (nextFragmentIdNumber++);
}

///////////////////////////////////////////////////////////////////////////

function spawnUIDriver(options) {
  options = options || {};

  var globalEventProj = globalEvent(_$('selector'), _$('eventType'), __);
  Dataspace.spawn(
    new DemandMatcher([Patch.observe(globalEventProj)],
                      [Patch.advertise(globalEventProj)],
                      function (c) {
                        Dataspace.spawn(new GlobalEventSupply(c.selector, c.eventType));
                      }, { name: 'globalEventSupervisor' }));

  var windowEventProj = windowEvent(_$('eventType'), __);
  Dataspace.spawn(
    new DemandMatcher([Patch.observe(windowEventProj)],
                      [Patch.advertise(windowEventProj)],
                      function (c) {
                        Dataspace.spawn(new WindowEventSupply(c.eventType));
                      }, { name: 'windowEventSupervisor' }));

  Dataspace.spawn(
    new DemandMatcher([uiFragment(_$('fragmentId'), __, __, __)],
                      [uiFragmentExists(_$('fragmentId'))],
                      function (c) {
                        Dataspace.spawn(new UIFragment(c.fragmentId));
                      }, { name: 'uiFragmentSupervisor' }));

  Dataspace.spawn(
    new DemandMatcher([uiAttribute(_$('selector'), _$('attribute'), _$('value'))],
                      [Patch.advertise(uiAttribute(_$('selector'), _$('attribute'), _$('value')))],
                      function (c) {
                        Dataspace.spawn(new UIAttribute(
                          c.selector, c.attribute, c.value, 'attribute'));
                      }, { name: 'uiAttributeSupervisor' }));

  Dataspace.spawn(
    new DemandMatcher([uiProperty(_$('selector'), _$('property'), _$('value'))],
                      [Patch.advertise(uiProperty(_$('selector'), _$('property'), _$('value')))],
                      function (c) {
                        Dataspace.spawn(new UIAttribute(
                          c.selector, c.property, c.value, 'property'));
                      }, { name: 'uiPropertySupervisor' }));

  Dataspace.spawn(new AttributeUpdater());
  Dataspace.spawn(new LocationHashTracker(options.defaultLocationHash || '/'));
}

///////////////////////////////////////////////////////////////////////////

function GlobalEventSupply(selector, eventType) {
  this.selector = selector;
  this.eventType = eventType;
  this.demandPat = Patch.observe(globalEvent(this.selector, this.eventType, __));
  this.name = ['globalEvent', selector, eventType];
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
  selectorMatch(document, this.selector).forEach(
    eventUpdater(cleanEventType(this.eventType), this.handlerClosure, install));
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

function WindowEventSupply(eventType) {
  this.eventType = eventType;
  this.demandPat = Patch.observe(windowEvent(this.eventType, __));
  this.name = ['windowEvent', eventType];
}

WindowEventSupply.prototype.boot = function () {
  var self = this;
  this.handlerClosure = Dataspace.wrap(function(e) { return self.handleDomEvent(e); });
  this.updateEventListeners(true);

  return Patch.sub(this.demandPat) // track demand
    .andThen(Patch.pub(windowEvent(this.eventType, __))) // indicate our presence
  ;
};

WindowEventSupply.prototype.updateEventListeners = function (install) {
  if (install) {
    window.addEventListener(cleanEventType(this.eventType), this.handlerClosure);
  } else {
    window.removeEventListener(cleanEventType(this.eventType), this.handlerClosure);
  }
};

WindowEventSupply.prototype.trapexit = function () {
  console.log('WindowEventSupply trapexit running', this.eventType);
  this.updateEventListeners(false);
};

WindowEventSupply.prototype.handleDomEvent = function (event) {
  Dataspace.send(windowEvent(this.eventType, event));
  return dealWithPreventDefault(this.eventType, event);
};

WindowEventSupply.prototype.handleEvent = function (e) {
  if (e.type === 'stateChange' && e.patch.project(this.demandPat).hasRemoved()) {
    Dataspace.exit(); // trapexit will uninstall event listeners
  }
};

///////////////////////////////////////////////////////////////////////////

function UIFragment(fragmentId) {
  this.fragmentId = fragmentId;
  this.demandProj = uiFragment(this.fragmentId, _$('selector'), _$('html'), _$('orderBy'));
  this.eventDemandProj =
    Patch.observe(uiEvent(this.fragmentId, _$('selector'), _$('eventType'), __));

  this.currentAnchorNodes = [];
  this.currentSelector = null;
  this.currentHtml = null;
  this.currentOrderBy = null;

  this.currentEventRegistrations = Immutable.Map();
  // ^ Map from (Map of selector/eventType) to closure.

  this.name = ['uiFragment', fragmentId];
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
  this.updateContent(null, null, null);
};

var SYNDICATE_SORT_KEY = '__syndicate_sort_key';

function setSortKey(n, orderBy, fragmentId) {
  var v = JSON.stringify([orderBy, fragmentId]);
  if ('dataset' in n) {
    // html element nodes etc.
    n.dataset[SYNDICATE_SORT_KEY] = v;
  } else {
    // text nodes, svg nodes, etc etc.
    n[SYNDICATE_SORT_KEY] = v;
  }
}

function getSortKey(n) {
  if ('dataset' in n && n.dataset[SYNDICATE_SORT_KEY]) {
    return JSON.parse(n.dataset[SYNDICATE_SORT_KEY]);
  }
  if (n[SYNDICATE_SORT_KEY]) {
    return JSON.parse(n[SYNDICATE_SORT_KEY]);
  }
  return null;
}

function hasSortKey(n, orderBy, fragmentId) {
  var v = getSortKey(n);
  if (!v) return false;
  if (v[0] !== orderBy) return false;
  if (v[1] !== fragmentId) return false;
  return true;
}

function firstChildNodeIndex_withSortKey(n) {
  for (var i = 0; i < n.childNodes.length; i++) {
    if (getSortKey(n.childNodes[i])) return i;
  }
  return n.childNodes.length;
}

// If *no* nodes have a sort key, returns a value that yields an empty
// range in conjunction with firstChildNodeIndex_withSortKey.
function lastChildNodeIndex_withSortKey(n) {
  for (var i = n.childNodes.length - 1; i >= 0; i--) {
    if (getSortKey(n.childNodes[i])) return i;
  }
  return n.childNodes.length - 1;
}

function isGreaterThan(a, b) {
  if (typeof a > typeof b) return true;
  if (typeof a < typeof b) return false;
  return a > b;
}

function findInsertionPoint(n, orderBy, fragmentId) {
  var lo = firstChildNodeIndex_withSortKey(n);
  var hi = lastChildNodeIndex_withSortKey(n) + 1;
  // lo <= hi, and [lo, hi) have sort keys.

  while (lo < hi) { // when lo === hi, there's nothing more to examine.
    var probe = (lo + hi) >> 1;
    var probeSortKey = getSortKey(n.childNodes[probe]);

    if ((isGreaterThan(probeSortKey[0], orderBy))
        || ((probeSortKey[0] === orderBy) && (probeSortKey[1] > fragmentId)))
    {
      hi = probe;
    } else {
      lo = probe + 1;
    }
  }

  // lo === hi now.
  if (lo < n.childNodes.length) {
    return n.childNodes[lo];
  } else {
    return null;
  }
}

UIFragment.prototype.removeNodes = function () {
  var self = this;

  self.currentAnchorNodes.forEach(function (anchorNode) {
    var insertionPoint = findInsertionPoint(anchorNode, self.currentOrderBy, self.fragmentId);
    while (1) {
      var n = insertionPoint ? insertionPoint.previousSibling : anchorNode.lastChild;
      if (!(n && hasSortKey(n, self.currentOrderBy, self.fragmentId))) break;
      n.parentNode.removeChild(n); // auto-updates previousSibling/lastChild
    }
  });
};

function htmlToNodes(parent, html) {
  var e = parent.cloneNode(false);
  e.innerHTML = html;
  return Array.prototype.slice.call(e.childNodes);
}

function configureNode(n) {
  // Runs post-insertion configuration of nodes.
  // TODO: review this design.
  selectorMatch(n, '.-syndicate-focus').forEach(function (n) {
    if ('focus' in n && 'setSelectionRange' in n) {
      n.focus();
      n.setSelectionRange(n.value.length, n.value.length);
    }
  });
}

UIFragment.prototype.updateContent = function (newSelector, newHtml, newOrderBy) {
  var self = this;

  self.removeNodes();

  var newAnchors = (newSelector !== null) ? selectorMatch(document, newSelector) : [];

  newAnchors.forEach(function (anchorNode) {
    var insertionPoint = findInsertionPoint(anchorNode, newOrderBy, self.fragmentId);
    htmlToNodes(anchorNode, newHtml).forEach(function (newNode) {
      setSortKey(newNode, newOrderBy, self.fragmentId);
      anchorNode.insertBefore(newNode, insertionPoint);
      configureNode(newNode);
    });
  });

  self.currentAnchorNodes = newAnchors;
  self.currentSelector = newSelector;
  self.currentHtml = newHtml;
  self.currentOrderBy = newOrderBy;

  self.currentEventRegistrations.forEach(function (_handlerClosure, key) {
    self.updateEventListeners(key.toObject(), true); // (re)install event listeners
  });
};

UIFragment.prototype.handleEvent = function (e) {
  var self = this;

  if (e.type === 'stateChange') {
    var fragmentChanges = e.patch.projectObjects(self.demandProj);
    fragmentChanges[0].forEach(function (c) { self.updateContent(c.selector, c.html, c.orderBy); });
    fragmentChanges[1].forEach(function (c) {
      if (c.selector === self.currentSelector
          && c.html === self.currentHtml
          && c.orderBy === self.currentOrderBy)
      {
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
    var insertionPoint = findInsertionPoint(anchorNode, self.currentOrderBy, self.fragmentId);
    while (1) {
      var uiNode = insertionPoint ? insertionPoint.previousSibling : anchorNode.lastChild;
      if (!(uiNode && hasSortKey(uiNode, self.currentOrderBy, self.fragmentId))) break;
      if ('querySelectorAll' in uiNode) {
        selectorMatch(uiNode, c.selector).forEach(
          eventUpdater(cleanEventType(c.eventType), handlerClosure, install));
      }
      insertionPoint = uiNode;
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

function UIAttribute(selector, key, value, kind) {
  if (['attribute', 'property'].indexOf(kind) === -1) {
    throw new Error("UIAttribute: kind must be 'attribute' or 'property'; got " + kind);
  }

  this.selector = selector;
  this.key = key;
  this.value = value;
  this.kind = kind;

  this.savedValues = [];
  // ^ Array of {node: DOMNode, value: (U Null String)},
  //   when attribute !== 'class' or kind !== 'attribute'.
  // ^ Array of {node: DOMNode},
  //   when attribute === 'class' and kind === 'attribute'.

  this.name = ['uiAttribute', selector, key, value, kind];
}

UIAttribute.prototype.boot = function () {
  var a = ((this.kind === 'attribute') ? uiAttribute : uiProperty)(this.selector, this.key, this.value);
  this.install();
  return Patch.sub(a).andThen(Patch.pub(a));
};

UIAttribute.prototype.trapexit = function () {
  console.log('UIAttribute trapexit running', this.selector, this.key, this.value, this.kind);
  this.restoreSavedValues();
};

function splitClassValue(v) {
  v = (v || '').trim();
  return v ? v.split(/ +/) : [];
}

UIAttribute.prototype.install = function () {
  var self = this;
  selectorMatch(document, self.selector).forEach(function (node) {
    switch (self.kind) {
      case 'attribute':
        if (self.key === 'class') {
          // Deliberately maintains duplicates, so we don't interfere
          // with potential other UIAttribute instances on the same
          // objects for the same attribute. See also
          // restoreSavedValues.
          var existing = splitClassValue(node.getAttribute('class'));
          var toAdd = splitClassValue(self.value);
          self.savedValues.push({node: node});
          node.setAttribute('class', existing.concat(toAdd).join(' '));
        } else {
          self.savedValues.push({node: node, value: node.getAttribute(self.key)});
          node.setAttribute(self.key, self.value);
        }
        break;
      case 'property':
        self.savedValues.push({node: node, value: node[self.key]});
        node[self.key] = self.value;
        break;
    }
  });
};

UIAttribute.prototype.restoreSavedValues = function () {
  var self = this;
  self.savedValues.forEach(function (entry) {
    switch (self.kind) {
      case 'attribute':
        if (self.key === 'class') {
          var existing = splitClassValue(entry.node.getAttribute('class'));
          var toRemove = splitClassValue(self.value);
          toRemove.forEach(function (v) {
            var i = existing.indexOf(v);
            if (i !== -1) { existing.splice(i, 1); }
          });
          if (existing.length === 0) {
            entry.node.removeAttribute('class');
          } else {
            entry.node.setAttribute('class', existing.join(' '));
          }
        } else {
          if (entry.value === null) {
            entry.node.removeAttribute(self.key);
          } else {
            entry.node.setAttribute(self.key, entry.value);
          }
        }
        break;
      case 'property':
        if (typeof entry.value === 'undefined') {
          delete entry.node[self.key];
        } else {
          entry.node[self.key] = entry.value;
        }
        break;
    }
  });
  self.savedValues = [];
};

UIAttribute.prototype.handleEvent = function (e) {
  if (e.type === 'stateChange' && e.patch.hasRemoved()) {
    Dataspace.exit(); // trapexit will restore attributes
  }
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

  if (typeof s !== 'string') {
    s = JSON.stringify(s);
  }

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

function selectorMatch(n, selector) {
  if (n && typeof n === 'object' && 'querySelectorAll' in n) {
    return Array.prototype.slice.call(n.querySelectorAll(selector));
  } else {
    return [];
  }
}

function eventUpdater(eventType, handlerClosure, install) {
  return function (n) {
    // addEventListener and removeEventListener are idempotent.
    if (install) {
      n.addEventListener(eventType, handlerClosure);
    } else {
      n.removeEventListener(eventType, handlerClosure);
    }
  };
}

///////////////////////////////////////////////////////////////////////////

function Anchor(explicitFragmentId) {
  this.fragmentId =
    (typeof explicitFragmentId === 'undefined') ? newFragmentId() : explicitFragmentId;
  this.htmlPattern = uiFragment(this.fragmentId, __, __, __);
  this.eventPattern = uiEvent(this.fragmentId, __, __, __);
}

Anchor.prototype.context = function (/* ... */) {
  var extn = Array.prototype.slice.call(arguments).map(escapeDataAttributeName).join('__');
  return new Anchor(this.fragmentId + '__' + extn);
};

Anchor.prototype.html = function (selector, html, orderBy) {
  return uiFragment(this.fragmentId,
                    selector,
                    html,
                    typeof orderBy === 'undefined' ? null : orderBy);
};

Anchor.prototype.event = function (selector, eventType, event) {
  return uiEvent(this.fragmentId, selector, eventType, event);
};

///////////////////////////////////////////////////////////////////////////

function LocationHashTracker(defaultLocationHash) {
  this.defaultLocationHash = defaultLocationHash;
  this.hashValue = null;
  this.name = 'LocationHashTracker';
}

LocationHashTracker.prototype.boot = function () {
  var self = this;
  this.loadHash();
  this.handlerClosure = Dataspace.wrap(function (e) { self.handleDomEvent(e); });
  window.addEventListener('hashchange', this.handlerClosure);

  return Patch.assert(locationHash(this.hashValue))
    .andThen(Patch.sub(setLocationHash(__)));
};

LocationHashTracker.prototype.trapexit = function () {
  window.removeEventListener('hashchange', this.handlerClosure);
};

LocationHashTracker.prototype.loadHash = function () {
  this.hashValue = window.location.hash;
  if (this.hashValue.length && this.hashValue[0] === '#') {
    this.hashValue = this.hashValue.slice(1);
  }
  if (!this.hashValue) {
    this.hashValue = this.defaultLocationHash;
  }
};

LocationHashTracker.prototype.handleDomEvent = function (e) {
  this.loadHash();
  Dataspace.stateChange(Patch.retract(locationHash(__))
                        .andThen(Patch.assert(locationHash(this.hashValue))));
};

LocationHashTracker.prototype.handleEvent = function (e) {
  if (e.type === 'message' && setLocationHash.isClassOf(e.message)) {
    window.location.hash = e.message[0];
  }
};

///////////////////////////////////////////////////////////////////////////

function AttributeUpdater() {
  this.name = 'AttributeUpdater';
}

AttributeUpdater.prototype.boot = function () {
  return Patch.sub(setAttribute(__, __, __))
    .andThen(Patch.sub(removeAttribute(__, __)))
    .andThen(Patch.sub(setProperty(__, __, __)))
    .andThen(Patch.sub(removeProperty(__, __)))
  ;
};

AttributeUpdater.prototype.handleEvent = function (e) {
  if (e.type === 'message') {
    var f = false;
    if (setAttribute.isClassOf(e.message)) {
      f = function (n, k) { n.setAttribute(k, e.message[2]); };
    } else if (removeAttribute.isClassOf(e.message)) {
      f = function (n, k) { n.removeAttribute(k); };
    } else if (setProperty.isClassOf(e.message)) {
      f = function (n, k) { n[k] = e.message[2]; };
    } else if (removeProperty.isClassOf(e.message)) {
      f = function (n, k) { delete n[k]; };
    }
    if (f) {
      selectorMatch(document, e.message[0]).forEach(function (n) {
        f(n, e.message[1]);
      });
    }
  }
};

///////////////////////////////////////////////////////////////////////////

module.exports.newFragmentId = newFragmentId;
module.exports.spawnUIDriver = spawnUIDriver;
module.exports.Anchor = Anchor;
module.exports.globalEvent = globalEvent;
module.exports.windowEvent = windowEvent;
module.exports.uiEvent = uiEvent;
module.exports.uiFragment = uiFragment;
module.exports.uiFragmentExists = uiFragmentExists;
module.exports.uiAttribute = uiAttribute;
module.exports.uiProperty = uiProperty;
module.exports.setAttribute = setAttribute;
module.exports.removeAttribute = removeAttribute;
module.exports.setProperty = setProperty;
module.exports.removeProperty = removeProperty;
module.exports.locationHash = locationHash;
module.exports.setLocationHash = setLocationHash;
