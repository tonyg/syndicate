// Compile ES5+Syndicate to plain ES5.

'use strict';

var fs = require('fs');
var path = require('path');

var ohm = require('ohm-js');
var ES5 = require('./es5.js');

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

function buildActor(constructorES5, nameExpOpt, block) {
  var nameExpStr;
  if (nameExpOpt.numChildren === 1) {
    nameExpStr = ', ' + nameExpOpt.asES5;
  } else {
    nameExpStr = '';
  }
  return 'Syndicate.Actor.spawnActor(new '+constructorES5+', '+
    'function() ' + block.asES5 + nameExpStr + ');';
}

function buildFacet(facetBlock, transitionBlock) {
  return '(function () { ' + (facetBlock ? facetBlock.facetVarDecls : '') +
    '\nSyndicate.Actor.createFacet()' +
    (facetBlock ? facetBlock.asES5 : '') +
    (transitionBlock ? transitionBlock.asES5 : '') +
    '.completeBuild(); })();';
}

function buildOnEvent(isTerminal, eventType, subscription, projection, bindings, body) {
  return '\n.onEvent(' + isTerminal + ', ' + JSON.stringify(eventType) + ', ' +
    subscription + ', ' + projection +
    ', (function(' + bindings.join(', ') + ') ' + body + '))';
}

function buildCaseEvent(eventPattern, body) {
  if (eventPattern.eventType === 'risingEdge') {
    return buildOnEvent(true,
                        eventPattern.eventType,
                        'function() { return (' + eventPattern.asES5 + '); }',
                        'null',
                        [],
                        body);
  } else {
    return buildOnEvent(true,
                        eventPattern.eventType,
                        eventPattern.subscription,
                        eventPattern.projection,
                        eventPattern.bindings,
                        body);
  }
}

var modifiedSourceActions = {
  ActorStatement_noConstructor: function(_actor, _namedOpt, nameExpOpt, block) {
    return buildActor('Object()', nameExpOpt, block);
  },
  ActorStatement_withConstructor: function(_actor, ctorExp, _namedOpt, nameExpOpt, block) {
    return buildActor(ctorExp.asES5, nameExpOpt, block);
  },

  DataspaceStatement_ground: function(_ground, _dataspace, maybeId, block) {
    var code = 'new Syndicate.Ground(function () ' + block.asES5 + ').startStepping();';
    if (maybeId.numChildren === 1) {
      return 'var ' + maybeId.children[0].interval.contents + ' = ' + code;
    } else {
      return code;
    }
  },
  DataspaceStatement_normal: function(_dataspace, block) {
    return 'Syndicate.Dataspace.spawn(new Dataspace(function () ' + block.asES5 + '));';
  },

  ActorFacetStatement_state: function(_state, facetBlock, _until, transitionBlock) {
    return buildFacet(facetBlock, transitionBlock);
  },
  ActorFacetStatement_until: function(_react, _until, transitionBlock) {
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
    return 'var ' + typeName.asES5 + ' = Syndicate.Struct.makeConstructor(' +
      label + ', ' + JSON.stringify(formals) + ');';
  },

  SendMessageStatement: function(_colons, expr, sc) {
    return 'Syndicate.Dataspace.send(' + expr.asES5 + ')' + sc.interval.contents;
  },

  FacetBlock: function(_leftParen, _varStmts, init, situations, done, _rightParen) {
    return (init ? init.asES5 : '') + situations.asES5.join('') + (done ? done.asES5 : '');
  },
  FacetStateTransitionBlock: function(_leftParen, transitions, _rightParen) {
    return transitions.asES5.join('');
  },

  FacetInitBlock: function(_init, block) {
    return '\n.addInitBlock((function() ' + block.asES5 + '))';
  },
  FacetDoneBlock: function(_done, block) {
    return '\n.addDoneBlock((function() ' + block.asES5 + '))';
  },

  FacetSituation_assert: function(_assert, expr, whenClause, _sc) {
    return '\n.addAssertion(' + buildSubscription([expr], 'assert', 'pattern', whenClause, null) + ')';
  },
  FacetSituation_event: function(_on, eventPattern, block) {
    return buildOnEvent(false,
                        eventPattern.eventType,
                        eventPattern.subscription,
                        eventPattern.projection,
                        eventPattern.bindings,
                        block.asES5);
  },
  FacetSituation_onEvent: function (_on, _event, id, block) {
    return '\n.addOnEventHandler((function(' + id.asES5 + ') ' + block.asES5 + '))';
  },
  FacetSituation_during: function(_during, pattern, facetBlock) {
    var cachedAssertionVar = gensym('cachedAssertion');
    return buildOnEvent(false,
                        'asserted',
                        pattern.subscription,
                        pattern.projection,
                        pattern.bindings,
                        '{ ' + facetBlock.facetVarDecls +
                        '\nvar '+cachedAssertionVar+' = '+pattern.instantiatedAssertion+';'+
                        '\nSyndicate.Actor.createFacet()' +
                        facetBlock.asES5 +
                        buildOnEvent(true,
                                     'retracted',
                                     pattern.instantiatedSubscription(cachedAssertionVar),
                                     pattern.instantiatedProjection(cachedAssertionVar),
                                     [],
                                     '{}') +
                        '.completeBuild(); }');
  },

  AssertWhenClause: function(_when, _lparen, expr, _rparen) {
    return expr.asES5;
  },

  FacetStateTransition_withContinuation: function(_case, eventPattern, block) {
    return buildCaseEvent(eventPattern, block.asES5);
  },
  FacetStateTransition_noContinuation: function(_case, eventPattern, _sc) {
    return buildCaseEvent(eventPattern, '{}');
  }
};

semantics.extendAttribute('modifiedSource', modifiedSourceActions);

semantics.addAttribute('facetVarDecls', {
  FacetBlock: function (_leftParen, varDecls, _init, _situations, _done, _rightParen) {
    return varDecls.asES5.join(' ');
  }
});

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
  FacetEventPattern_retractedEvent: function(_kw, _pattern) { return 'retracted'; },

  FacetTransitionEventPattern_facetEvent: function (pattern) { return pattern.eventType; },
  FacetTransitionEventPattern_risingEdge: function (_lp, expr, _rp) { return 'risingEdge'; }
});

function buildSubscription(children, patchMethod, mode, whenClause, cachedAssertionVar) {
  var fragments = [];
  var hasWhenClause = (whenClause && (whenClause.numChildren === 1));
  fragments.push('(function() { var _ = Syndicate.__; return ');
  if (hasWhenClause) {
    fragments.push('(' + whenClause.asES5 + ') ? ');
  }
  if (patchMethod) {
    fragments.push('Syndicate.Patch.' + patchMethod + '(');
  } else {
    fragments.push('{ assertion: ');
  }
  if (cachedAssertionVar) {
    fragments.push(cachedAssertionVar);
  } else {
    children.forEach(function (c) { fragments.push(c.buildSubscription(mode)); });
  }
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
  if (hasWhenClause) {
    fragments.push(' : Syndicate.Patch.emptyPatch');
  }
  fragments.push('; })');
  return fragments.join('');
}

semantics.addAttribute('subscription', {
  _default: function(children) {
    return buildSubscription(children, 'sub', 'pattern', null, null);
  }
});

semantics.addAttribute('instantiatedAssertion', {
  _default: function(children) {
    var fragments = [];
    fragments.push('(function() { var _ = Syndicate.__; return ');
    children.forEach(function (c) { fragments.push(c.buildSubscription('instantiated')); });
    fragments.push('; })()');
    return fragments.join('');
  }
});

semantics.addOperation('instantiatedSubscription(cachedAssertionVar)', {
  _default: function(children) {
    return buildSubscription(children, 'sub', 'instantiated', null, this.args.cachedAssertionVar);
  }
});

semantics.addOperation('instantiatedProjection(cachedAssertionVar)', {
  _default: function(children) {
    return buildSubscription(children, null, 'instantiated', null, this.args.cachedAssertionVar);
  }
});

semantics.addAttribute('projection', {
  _default: function(children) {
    return buildSubscription(children, null, 'projection', null, null);
  }
});

semantics.addAttribute('metalevel', {
  FacetEventPattern_messageEvent: function(_kw, p) { return p.metalevel; },
  FacetEventPattern_assertedEvent: function(_kw, p) { return p.metalevel; },
  FacetEventPattern_retractedEvent: function(_kw, p) { return p.metalevel; },

  FacetTransitionEventPattern_facetEvent: function (pattern) { return pattern.metalevel; },

  FacetPattern_withMetalevel: function(_expr, _kw, metalevel) {
    return metalevel.interval.contents;
  },
  FacetPattern_noMetalevel: function(_expr) {
    return 0;
  }
});

semantics.addOperation('buildSubscription(mode)', {
  FacetEventPattern_messageEvent: function(_kw, pattern) {
    return pattern.buildSubscription(this.args.mode);
  },
  FacetEventPattern_assertedEvent: function(_kw, pattern) {
    return pattern.buildSubscription(this.args.mode);
  },
  FacetEventPattern_retractedEvent: function(_kw, pattern) {
    return pattern.buildSubscription(this.args.mode);
  },

  FacetTransitionEventPattern_facetEvent: function (pattern) {
    return pattern.buildSubscription(this.args.mode);
  },

  FacetPattern: function (v) {
    return v.children[0].buildSubscription(this.args.mode); // both branches!
  },

  AssignmentExpression_assignment: function (lhsExpr, _assignmentOperator, rhsExpr) {
    var i = lhsExpr.interval.contents;
    if (i[0] === '$' && i.length > 1) {
      switch (this.args.mode) {
        case 'pattern': return rhsExpr.buildSubscription(this.args.mode);
        case 'instantiated': return lhsExpr.buildSubscription(this.args.mode);
        case 'projection':
          return '(Syndicate._$(' + JSON.stringify(i.slice(1)) + ',' +
            rhsExpr.buildSubscription(this.args.mode) +
            '))';
        default: throw new Error('Unexpected buildSubscription mode ' + this.args.mode);
      }
    } else {
      return lhsExpr.buildSubscription(this.args.mode) +
        _assignmentOperator.buildSubscription(this.args.mode) +
        rhsExpr.buildSubscription(this.args.mode);
    }
  },

  identifier: function(_name) {
    var i = this.interval.contents;
    if (i[0] === '$' && i.length > 1) {
      switch (this.args.mode) {
        case 'pattern': return '_';
        case 'instantiated': return i.slice(1);
        case 'projection': return '(Syndicate._$(' + JSON.stringify(i.slice(1)) + '))';
        default: throw new Error('Unexpected buildSubscription mode ' + this.args.mode);
      }
    } else {
      return i;
    }
  },
  _terminal: function() {
    return undefined;
  },
  _nonterminal: function(children) {
    var self = this;
    return ES5.translateNonterminalCode(children,
                                        function(n) {
                                          return n.buildSubscription(self.args.mode);
                                        }) || this.interval.contents;
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
    if (i[0] === '$' && i.length > 1) {
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
