///////////////////////////////////////////////////////////////////////////
// GUI

var Network = Syndicate.Network;
var Route = Syndicate.Route;
var Patch = Syndicate.Patch;
var __ = Syndicate.__;
var _$ = Syndicate._$;

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
  Network.spawn({
    field: { text: '', pos: 0 },
    highlight: { state: false },

    boot: function () {
      return Patch.sub(["jQuery", "#inputRow", "+keypress", __])
	.andThen(Patch.sub(["fieldContents", __, __]))
	.andThen(Patch.sub(["highlight", __]));
    },

    fieldContentsProjection: Route.compileProjection(["fieldContents", _$("text"), _$("pos")]),
    highlightProjection: Route.compileProjection(["highlight", _$("state")]),
    handleEvent: function (e) {
      var self = this;
      switch (e.type) {
      case "message":
	var event = e.message[3];
	var keycode = event.keyCode;
	var character = String.fromCharCode(event.charCode);
	if (keycode === 37 /* left */) {
	  Network.send(["fieldCommand", "cursorLeft"]);
	} else if (keycode === 39 /* right */) {
	  Network.send(["fieldCommand", "cursorRight"]);
	} else if (keycode === 9 /* tab */) {
	  // ignore
	} else if (keycode === 8 /* backspace */) {
	  Network.send(["fieldCommand", "backspace"]);
	} else if (character) {
	  Network.send(["fieldCommand", ["insert", character]]);
	}
	break;
      case "stateChange":
	Route.projectObjects(e.patch.added, this.fieldContentsProjection).forEach(function (c) {
	  self.field = c;
	});
	Route.projectObjects(e.patch.added, this.highlightProjection).forEach(function (c) {
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
      var hLeft = highlight ? highlight.get(0) : 0;
      var hRight = highlight ? highlight.get(1) : 0;
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
  Network.spawn({
    fieldContents: initialContents,
    cursorPos: initialContents.length, /* positions address gaps between characters */

    boot: function () {
      this.publishState();
      return Patch.sub(["fieldCommand", __]);
    },

    handleEvent: function (e) {
      if (e.type === "message" && e.message[0] === "fieldCommand") {
	var command = e.message[1];
	if (command === "cursorLeft") {
	  this.cursorPos--;
	  if (this.cursorPos < 0)
	    this.cursorPos = 0;
	} else if (command === "cursorRight") {
	  this.cursorPos++;
	  if (this.cursorPos > this.fieldContents.length)
	    this.cursorPos = this.fieldContents.length;
	} else if (command === "backspace" && this.cursorPos > 0) {
	  this.fieldContents =
	    this.fieldContents.substring(0, this.cursorPos - 1) +
	    this.fieldContents.substring(this.cursorPos);
	  this.cursorPos--;
	} else if (command.constructor === Array && command[0] === "insert") {
	  var newText = command[1];
	  this.fieldContents =
	    this.fieldContents.substring(0, this.cursorPos) +
	    newText +
	    this.fieldContents.substring(this.cursorPos);
	  this.cursorPos += newText.length;
	}
	this.publishState();
      }
    },

    publishState: function () {
      Network.stateChange(
	Patch.retract(["fieldContents", __, __])
	  .andThen(Patch.assert(["fieldContents", this.fieldContents, this.cursorPos])));
    }
  });
}

///////////////////////////////////////////////////////////////////////////
// Search engine

function spawnSearch() {
  Network.spawn({
    fieldContents: "",
    highlight: false,

    boot: function () {
      this.publishState();
      return Patch.sub(["jQuery", "#searchBox", "input", __])
	.andThen(Patch.sub(["fieldContents", __, __]));
    },

    fieldContentsProjection: Route.compileProjection(["fieldContents", _$("text"), _$("pos")]),
    handleEvent: function (e) {
      var self = this;
      if (e.type === "message" && e.message[0] === "jQuery") {
	this.search();
      }
      if (e.type === "stateChange") {
	Route.projectObjects(e.patch.added, this.fieldContentsProjection).forEach(function (c) {
	  self.fieldContents = c.text;
	});
	this.search();
      }
    },

    publishState: function () {
      Network.stateChange(
	Patch.retract(["highlight", __])
	  .andThen(Patch.assert(["highlight", this.highlight])));
    },

    search: function () {
      var searchtext = $("#searchBox")[0].value;
      var oldHighlight = this.highlight;
      if (searchtext) {
	var pos = this.fieldContents.indexOf(searchtext);
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

  G.network.onStateChange = function (mux, patch) {
    $("#spy-holder").text(Syndicate.prettyTrie(mux.routingTable));
  };

  G.startStepping();
});
