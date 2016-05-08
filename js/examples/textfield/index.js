///////////////////////////////////////////////////////////////////////////
// GUI

var Dataspace = Syndicate.Dataspace;
var Trie = Syndicate.Trie;
var Patch = Syndicate.Patch;
var __ = Syndicate.__;
var _$ = Syndicate._$;

var jQueryEvent = Syndicate.JQuery.jQueryEvent;
var fieldContents = Syndicate.Struct.makeStructureConstructor('fieldContents', ['text', 'pos']);
var highlight = Syndicate.Struct.makeStructureConstructor('highlight', ['state']);
var fieldCommand = Syndicate.Struct.makeStructureConstructor('fieldCommand', ['detail']);

function escapeText(text) {
  text = text.replace(/&/g, '&amp;');
  text = text.replace(/</g, '&lt;');
  text = text.replace(/>/g, '&gt;');
  text = text.replace(/ /g, '&nbsp;');
  return text;
}

function piece(text, pos, lo, hi, cls) {
  return "<span class='"+cls+"'>"+
    ((pos >= lo && pos < hi)
     ? (escapeText(text.substring(lo, pos)) +
	"<span class='cursor'></span>" +
	escapeText(text.substring(pos, hi)))
     : escapeText(text.substring(lo, hi)))
    + "</span>";
}

function spawnGui() {
  Dataspace.spawn({
    field: { text: '', pos: 0 },
    highlight: { state: false },

    boot: function () {
      return Patch.sub(jQueryEvent("#inputRow", "+keypress", __))
	.andThen(Patch.sub(fieldContents.pattern))
	.andThen(Patch.sub(highlight.pattern));
    },

    fieldContentsProjection: fieldContents(_$("text"), _$("pos")),
    highlightProjection: highlight(_$("state")),
    handleEvent: function (e) {
      var self = this;
      switch (e.type) {
      case "message":
	var event = e.message.eventValue;
	var keycode = event.keyCode;
	var character = String.fromCharCode(event.charCode);
	if (keycode === 37 /* left */) {
	  Dataspace.send(fieldCommand("cursorLeft"));
	} else if (keycode === 39 /* right */) {
	  Dataspace.send(fieldCommand("cursorRight"));
	} else if (keycode === 9 /* tab */) {
	  // ignore
	} else if (keycode === 8 /* backspace */) {
	  Dataspace.send(fieldCommand("backspace"));
	} else if (character) {
	  Dataspace.send(fieldCommand(["insert", character]));
	}
	break;
      case "stateChange":
	Trie.projectObjects(e.patch.added, this.fieldContentsProjection).forEach(function (c) {
	  self.field = c;
	});
	Trie.projectObjects(e.patch.added, this.highlightProjection).forEach(function (c) {
	  self.highlight = c;
	});
	this.updateDisplay();
	break;
      }
    },

    updateDisplay: function () {
      var text = this.field ? this.field.text : "";
      var pos = this.field ? this.field.pos : 0;
      var highlight = this.highlight ? this.highlight.state : false;
      var hLeft = highlight ? highlight[0] : 0;
      var hRight = highlight ? highlight[1] : 0;
      $("#fieldContents")[0].innerHTML = highlight
	? piece(text, pos, 0, hLeft, "normal") +
	piece(text, pos, hLeft, hRight, "highlight") +
	piece(text, pos, hRight, text.length + 1, "normal")
	: piece(text, pos, 0, text.length + 1, "normal");
    }
  });
}

///////////////////////////////////////////////////////////////////////////
// Textfield Model

function spawnModel() {
  var initialContents = "initial";
  Dataspace.spawn({
    fieldValue: initialContents,
    cursorPos: initialContents.length, /* positions address gaps between characters */

    boot: function () {
      this.publishState();
      return Patch.sub(fieldCommand.pattern);
    },

    handleEvent: function (e) {
      if (e.type === "message" && fieldCommand.isClassOf(e.message)) {
	var command = e.message.detail;
	if (command === "cursorLeft") {
	  this.cursorPos--;
	  if (this.cursorPos < 0)
	    this.cursorPos = 0;
	} else if (command === "cursorRight") {
	  this.cursorPos++;
	  if (this.cursorPos > this.fieldValue.length)
	    this.cursorPos = this.fieldValue.length;
	} else if (command === "backspace" && this.cursorPos > 0) {
	  this.fieldValue =
	    this.fieldValue.substring(0, this.cursorPos - 1) +
	    this.fieldValue.substring(this.cursorPos);
	  this.cursorPos--;
	} else if (command.constructor === Array && command[0] === "insert") {
	  var newText = command[1];
	  this.fieldValue =
	    this.fieldValue.substring(0, this.cursorPos) +
	    newText +
	    this.fieldValue.substring(this.cursorPos);
	  this.cursorPos += newText.length;
	}
	this.publishState();
      }
    },

    publishState: function () {
      Dataspace.stateChange(
	Patch.retract(fieldContents.pattern)
	  .andThen(Patch.assert(fieldContents(this.fieldValue, this.cursorPos))));
    }
  });
}

///////////////////////////////////////////////////////////////////////////
// Search engine

function spawnSearch() {
  Dataspace.spawn({
    fieldValue: "",
    highlight: false,

    boot: function () {
      this.publishState();
      return Patch.sub(jQueryEvent("#searchBox", "input", __))
	.andThen(Patch.sub(fieldContents.pattern));
    },

    fieldContentsProjection: fieldContents(_$("text"), _$("pos")),
    handleEvent: function (e) {
      var self = this;
      if (jQueryEvent.isClassOf(e.message)) {
	this.search();
      }
      if (e.type === "stateChange") {
	Trie.projectObjects(e.patch.added, this.fieldContentsProjection).forEach(function (c) {
	  self.fieldValue = c.text;
	});
	this.search();
      }
    },

    publishState: function () {
      Dataspace.stateChange(
	Patch.retract(highlight.pattern)
	  .andThen(Patch.assert(highlight(this.highlight))));
    },

    search: function () {
      var searchtext = $("#searchBox")[0].value;
      var oldHighlight = this.highlight;
      if (searchtext) {
	var pos = this.fieldValue.indexOf(searchtext);
	this.highlight = (pos !== -1) && [pos, pos + searchtext.length];
      } else {
	this.highlight = false;
      }
      if (JSON.stringify(oldHighlight) !== JSON.stringify(this.highlight)) {
	this.publishState();
      }
    }
  });
}

///////////////////////////////////////////////////////////////////////////
// Main

var G;
$(document).ready(function () {
  G = new Syndicate.Ground(function () {
    Syndicate.JQuery.spawnJQueryDriver();
    Syndicate.DOM.spawnDOMDriver();

    spawnGui();
    spawnModel();
    spawnSearch();
  });

  G.dataspace.onStateChange = function (mux, patch) {
    $("#spy-holder").text(Syndicate.prettyTrie(mux.routingTable));
  };

  G.startStepping();
});
