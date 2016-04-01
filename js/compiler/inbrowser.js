'use strict';

var compiler = require('./compiler.js');

function getUrlContent(url) {
  var req = new XMLHttpRequest();
  req.open('GET', url, false);
  try {
    req.send();
    if (req.status === 0 || req.status === 200) {
      return req.responseText;
    }
  } catch (e) {
    console.error("Error while loading " + url, e);
  }
  return false;
}

function translateSyndicateScripts() {
  var scriptNodes = document.querySelectorAll('script[type="text/syndicate-js"]');
  var allSources = [];
  for (var i = 0; i < scriptNodes.length; i++) {
    var n = scriptNodes[i];
    var srcUrl = n.getAttribute('src');
    allSources.push(srcUrl ? getUrlContent(srcUrl) : n.innerHTML);
  }
  var allSourceText = allSources.join('\n;\n');

  var output = compiler.compileSyndicateSource(allSourceText);
  var f = new Function(output);
  f();
}

document.addEventListener('DOMContentLoaded', translateSyndicateScripts, false);

//---------------------------------------------------------------------------

module.exports = compiler;
