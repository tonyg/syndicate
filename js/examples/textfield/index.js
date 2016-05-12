///////////////////////////////////////////////////////////////////////////
// GUI

var Dataspace = Syndicate.Dataspace;
var Trie = Syndicate.Trie;
var Patch = Syndicate.Patch;
var __ = Syndicate.__;
var _$ = Syndicate._$;

var globalEvent = Syndicate.UI.globalEvent;
var fieldContents = Syndicate.Struct.makeConstructor('fieldContents', ['text', 'pos']);
var highlight = Syndicate.Struct.makeConstructor('highlight', ['state']);
var fieldCommand = Syndicate.Struct.makeConstructor('fieldCommand', ['detail']);

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
      return Patch.sub(globalEvent("#inputRow", "keypress", __))
        .andThen(Patch.sub(globalEvent("#inputRow", "+keydown", __)))
	.andThen(Patch.sub(fieldContents.pattern))
	.andThen(Patch.sub(highlight.pattern));
    },

    fieldContentsProjection: fieldContents(_$("text"), _$("pos")),
    highlightProjection: highlight(_$("state")),
    handleEvent: function (e) {
      var self = this;
      switch (e.type) {
        case "message":
	  var event = e.message[2];
          switch (event.type) {
            case "keydown":
              switch (event.keyCode) {
                case 37 /* left */: Dataspace.send(fieldCommand("cursorLeft")); break;
                case 39 /* right */: Dataspace.send(fieldCommand("cursorRight")); break;
                case 9 /* tab */: /* ignore */ break;
                case 8 /* backspace */:
                  event.preventDefault(); // that this works here is a minor miracle
	          Dataspace.send(fieldCommand("backspace"));
                  break;
                default: break;
              }
              break;
            case "keypress":
	      var character = String.fromCharCode(event.charCode);
              if (event.charCode && character) {
	        Dataspace.send(fieldCommand(["insert", character]));
	      }
              break;
            default:
              break;
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
      document.getElementById("fieldContents").innerHTML = highlight
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
	var command = e.message[0];
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
      return Patch.sub(globalEvent("#searchBox", "input", __))
	.andThen(Patch.sub(fieldContents.pattern));
    },

    fieldContentsProjection: fieldContents(_$("text"), _$("pos")),
    handleEvent: function (e) {
      var self = this;
      if (globalEvent.isClassOf(e.message)) {
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
      var searchtext = document.getElementById("searchBox").value;
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
document.addEventListener('DOMContentLoaded', function () {
  G = new Syndicate.Ground(function () {
    Syndicate.UI.spawnUIDriver();

    spawnGui();
    spawnModel();
    spawnSearch();
  });

  G.dataspace.setOnStateChange(function (mux, patch) {
    document.getElementById("spy-holder").innerText = Syndicate.prettyTrie(mux.routingTable);
  });

  G.startStepping();
});
