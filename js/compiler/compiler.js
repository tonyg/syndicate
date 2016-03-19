// Compile ES5+Syndicate to plain ES5.

'use strict';

var fs = require('fs');
var path = require('path');

var ohm = require('ohm-js');
var ES5 = require('ohm-js/examples/ecmascript/es5.js');

var grammarSource = fs.readFileSync(path.join(__dirname, 'syndicate.ohm')).toString();
var grammar = ohm.grammar(grammarSource, { ES5: ES5.grammar });
var semantics = grammar.extendSemantics(ES5.semantics);

var gensym_start = Math.floor(new Date() * 1);
var gensym_counter = 0;
function gensym(label) {
  return '_' + (label || 'g') + gensym_start + '_' + (gensym_counter++);
}

var forEachChild = (function () {
  function flattenIterNodes(nodes, acc) {
    for (var i = 0; i < nodes.length; ++i) {
      if (nodes[i].isIteration()) {
        flattenIterNodes(nodes[i].children, acc);
      } else {
        acc.push(nodes[i]);
      }
    }
  }

  function compareByInterval(node, otherNode) {
    return node.interval.startIdx - otherNode.interval.startIdx;
  }

  function forEachChild(children, f) {
    var nodes = [];
    flattenIterNodes(children, nodes);
    nodes.sort(compareByInterval).forEach(f);
  }

  return forEachChild;
})();

function buildActor(constructorES5, block) {
  return 'Syndicate.Actor.spawnActor(new '+constructorES5+', '+
    'function() ' + block.asES5 + ');';
}

function buildFacet(facetBlock, transitionBlock) {
  return 'Syndicate.Actor.createFacet()' +
    (facetBlock ? facetBlock.asES5 : '') +
    (transitionBlock ? transitionBlock.asES5 : '') +
    '.completeBuild();';
}

function buildOnEvent(isTerminal, eventType, subscription, projection, bindings, body) {
  return '\n.onEvent(' + isTerminal + ', ' + JSON.stringify(eventType) + ', ' +
    subscription + ', ' + projection +
    ', (function(' + bindings.join(', ') + ') ' + body + '))';
}

var modifiedSourceActions = {
  ActorStatement_noConstructor: function(_actor, block) {
    return buildActor('Object()', block);
  },
  ActorStatement_withConstructor: function(_actor, ctorExp, block) {
    return buildActor(ctorExp.asES5, block);
  },

  NetworkStatement_ground: function(_ground, _network, block) {
    return 'new Syndicate.Ground(function () ' + block.asES5 + ').startStepping();';
  },
  NetworkStatement_normal: function(_network, block) {
    return 'Syndicate.Network.spawn(new Network(function () ' + block.asES5 + '));';
  },

  ActorFacetStatement_state: function(_state, facetBlock, _until, transitionBlock) {
    return buildFacet(facetBlock, transitionBlock);
  },
  ActorFacetStatement_until: function(_until, transitionBlock) {
    return buildFacet(null, transitionBlock);
  },
  ActorFacetStatement_forever: function(_forever, facetBlock) {
    return buildFacet(facetBlock, null);
  },

  AssertionTypeDeclarationStatement: function(_assertion,
                                              _type,
                                              typeName,
                                              _leftParen,
                                              formalsRaw,
                                              _rightParen,
                                              _maybeEquals,
                                              maybeLabel,
                                              _maybeSc)
  {
    var formals = formalsRaw.asSyndicateStructureArguments;
    var label = maybeLabel.numChildren === 1
        ? maybeLabel.children[0].interval.contents
        : JSON.stringify(typeName.interval.contents);
    var fragments = [];
    fragments.push(
      'var ' + typeName.asES5 + ' = (function() {',
      '  var $SyndicateMeta$ = {',
      '    label: ' + label + ',',
      '    arguments: ' + JSON.stringify(formals),
      '  };',
      '  return function ' + typeName.asES5 + '(' + formalsRaw.asES5 + ') {',
      '    return {');
    formals.forEach(function(f) {
      fragments.push('      ' + JSON.stringify(f) + ': ' + f + ',');
    });
    fragments.push(
      '      "$SyndicateMeta$": $SyndicateMeta$',
      '    };',
      '  };',
      '})();');
    return fragments.join('\n');
  },

  SendMessageStatement: function(_colons, expr, sc) {
    return 'Syndicate.Network.send(' + expr.asES5 + ')' + sc.interval.contents;
  },

  FacetBlock: function(_leftParen, init, situations, done, _rightParen) {
    return (init ? init.asES5 : '') + situations.asES5.join('') + (done ? done.asES5 : '');
  },
  FacetStateTransitionBlock: function(_leftParen, transitions, _rightParen) {
    return transitions.asES5;
  },

  FacetInitBlock: function(_init, block) {
    return '\n.addInitBlock((function() ' + block.asES5 + '))';
  },
  FacetDoneBlock: function(_done, block) {
    return '\n.addDoneBlock((function() ' + block.asES5 + '))';
  },

  FacetSituation_assert: function(_assert, expr, _sc) {
    return '\n.addAssertion(' + buildSubscription([expr], 'assert', 'pattern') + ')';
  },
  FacetSituation_event: function(_on, eventPattern, block) {
    return buildOnEvent(false,
                        eventPattern.eventType,
                        eventPattern.subscription,
                        eventPattern.projection,
                        eventPattern.bindings,
                        block.asES5);
  },
  FacetSituation_during: function(_during, pattern, facetBlock) {
    return buildOnEvent(false,
                        'asserted',
                        pattern.subscription,
                        pattern.projection,
                        pattern.bindings,
                        '{ Syndicate.Actor.createFacet()' +
                        facetBlock.asES5 +
                        buildOnEvent(true,
                                     'retracted',
                                     pattern.instantiatedSubscription,
                                     'null',
                                     [],
                                     '{}') +
                        '.completeBuild(); }');
  },

  FacetStateTransition_withContinuation: function(_case, eventPattern, block) {
    return buildOnEvent(true,
                        eventPattern.eventType,
                        eventPattern.subscription,
                        eventPattern.projection,
                        eventPattern.bindings,
                        block.asES5);
  },
  FacetStateTransition_noContinuation: function(_case, eventPattern, _sc) {
    return buildOnEvent(true,
                        eventPattern.eventType,
                        eventPattern.subscription,
                        eventPattern.projection,
                        eventPattern.bindings,
                        '');
  }
};

semantics.extendAttribute('modifiedSource', modifiedSourceActions);

semantics.addAttribute('asSyndicateStructureArguments', {
  FormalParameterList: function(formals) {
    return formals.asIteration().asSyndicateStructureArguments;
  },
  identifier: function(_name) {
    return this.interval.contents;
  }
});

semantics.addAttribute('eventType', {
  FacetEventPattern_messageEvent: function(_kw, _pattern) { return 'message'; },
  FacetEventPattern_assertedEvent: function(_kw, _pattern) { return 'asserted'; },
  FacetEventPattern_retractedEvent: function(_kw, _pattern) { return 'retracted'; }
});

function buildSubscription(children, patchMethod, mode) {
  var fragments = [];
  fragments.push('(function() { var _ = Syndicate.__; return ');
  if (patchMethod) {
    fragments.push('Syndicate.Patch.' + patchMethod + '(');
  } else {
    fragments.push('{ assertion: ');
  }
  children.forEach(function (c) { c.buildSubscription(fragments, mode); });
  if (patchMethod) {
    fragments.push(', ');
  } else {
    fragments.push(', metalevel: ');
  }
  children.forEach(function (c) { fragments.push(c.metalevel) });
  if (patchMethod) {
    fragments.push(')');
  } else {
    fragments.push(' }');
  }
  fragments.push('; })');
  return fragments.join('');
}

semantics.addAttribute('subscription', {
  _default: function(children) {
    return buildSubscription(children, 'sub', 'pattern');
  }
});

semantics.addAttribute('instantiatedSubscription', {
  _default: function(children) {
    return buildSubscription(children, 'sub', 'instantiated');
  }
});

semantics.addAttribute('projection', {
  _default: function(children) {
    return buildSubscription(children, null, 'projection');
  }
});

semantics.addAttribute('metalevel', {
  FacetEventPattern_messageEvent: function(_kw, p) { return p.metalevel; },
  FacetEventPattern_assertedEvent: function(_kw, p) { return p.metalevel; },
  FacetEventPattern_retractedEvent: function(_kw, p) { return p.metalevel; },

  FacetPattern_withMetalevel: function(_expr, _kw, metalevel) {
    return metalevel.interval.contents;
  },
  FacetPattern_noMetalevel: function(_expr) {
    return 0;
  }
});

semantics.addOperation('buildSubscription(acc,mode)', {
  FacetEventPattern_messageEvent: function(_kw, pattern) {
    pattern.buildSubscription(this.args.acc, this.args.mode);
  },
  FacetEventPattern_assertedEvent: function(_kw, pattern) {
    pattern.buildSubscription(this.args.acc, this.args.mode);
  },
  FacetEventPattern_retractedEvent: function(_kw, pattern) {
    pattern.buildSubscription(this.args.acc, this.args.mode);
  },

  FacetPattern: function (v) {
    v.children[0].buildSubscription(this.args.acc, this.args.mode); // both branches!
  },

  identifier: function(_name) {
    var i = this.interval.contents;
    if (i[0] === '$') {
      switch (this.args.mode) {
      case 'pattern': this.args.acc.push('_'); break;
      case 'instantiated': this.args.acc.push(i.slice(1)); break;
      case 'projection': this.args.acc.push('(Syndicate._$(' + JSON.stringify(i.slice(1)) + '))'); break;
      default: throw new Error('Unexpected buildSubscription mode ' + this.args.mode);
      }
    } else {
      this.args.acc.push(i);
    }
  },
  _terminal: function() {
    this.args.acc.push(this.interval.contents);
  },
  _nonterminal: function(children) {
    var self = this;
    forEachChild(children, function (c) {
      c.buildSubscription(self.args.acc, self.args.mode);
    });
  }
});

semantics.addAttribute('bindings', {
  _default: function(children) {
    var result = [];
    this.pushBindings(result);
    return result;
  }
});

semantics.addOperation('pushBindings(accumulator)', {
  identifier: function(_name) {
    var i = this.interval.contents;
    if (i[0] === '$') {
      this.args.accumulator.push(i.slice(1));
    }
  },
  _terminal: function () {},
  _nonterminal: function(children) {
    var self = this;
    children.forEach(function (c) { c.pushBindings(self.args.accumulator); });
  }
})

function compileSyndicateSource(inputSource, onError) {
  var parseResult = grammar.match(inputSource);
  if (parseResult.failed()) {
    if (onError) {
      return onError(parseResult.message, parseResult);
    } else {
      console.error(parseResult.message);
      return false;
    }
  } else {
    return '"use strict";\n' + semantics(parseResult).asES5;
  }
}

//---------------------------------------------------------------------------

module.exports.grammar = grammar;
module.exports.semantics = semantics;
module.exports.compileSyndicateSource = compileSyndicateSource;
