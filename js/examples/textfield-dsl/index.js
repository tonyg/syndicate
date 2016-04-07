///////////////////////////////////////////////////////////////////////////
// GUI

assertion type jQuery(selector, eventType, event);
assertion type fieldCommand(detail);
assertion type fieldContents(text, pos);
assertion type highlight(state);

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
  actor {
    this.text = '';
    this.pos = 0;
    this.highlightState = false;

    this.updateDisplay = function () {
      var text = this.text;
      var pos = this.pos;
      var highlight = this.highlightState;
      var hLeft = highlight ? highlight.get(0) : 0;
      var hRight = highlight ? highlight.get(1) : 0;
      $("#fieldContents")[0].innerHTML = highlight
	? piece(text, pos, 0, hLeft, "normal") +
	piece(text, pos, hLeft, hRight, "highlight") +
	piece(text, pos, hRight, text.length + 1, "normal")
	: piece(text, pos, 0, text.length + 1, "normal");
    };

    forever {
      on message jQuery("#inputRow", "+keypress", $event) {
        var keycode = event.keyCode;
        var character = String.fromCharCode(event.charCode);
        if (keycode === 37 /* left */) {
          :: fieldCommand("cursorLeft");
        } else if (keycode === 39 /* right */) {
	  :: fieldCommand("cursorRight");
        } else if (keycode === 9 /* tab */) {
	  // ignore
        } else if (keycode === 8 /* backspace */) {
	  :: fieldCommand("backspace");
        } else if (character) {
	  :: fieldCommand(["insert", character]);
        }
      }

      on asserted fieldContents($text, $pos) {
        this.text = text;
        this.pos = pos;
        this.updateDisplay();
      }

      on asserted highlight($state) {
        this.highlightState = state;
        this.updateDisplay();
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Textfield Model

function spawnModel() {
  actor {
    this.fieldContents = "initial";
    this.cursorPos = this.fieldContents.length; /* positions address gaps between characters */

    forever {
      assert fieldContents(this.fieldContents, this.cursorPos);

      on message fieldCommand("cursorLeft") {
	this.cursorPos--;
	if (this.cursorPos < 0)
	  this.cursorPos = 0;
      }

      on message fieldCommand("cursorRight") {
	this.cursorPos++;
	if (this.cursorPos > this.fieldContents.length)
	  this.cursorPos = this.fieldContents.length;
      }

      on message fieldCommand("backspace") {
        if (this.cursorPos > 0) {
	  this.fieldContents =
	    this.fieldContents.substring(0, this.cursorPos - 1) +
	    this.fieldContents.substring(this.cursorPos);
	  this.cursorPos--;
        }
      }

      on message fieldCommand(["insert", $newText]) {
	this.fieldContents =
	  this.fieldContents.substring(0, this.cursorPos) +
	  newText +
	  this.fieldContents.substring(this.cursorPos);
	this.cursorPos += newText.length;
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Search engine

function spawnSearch() {
  actor {
    this.fieldContents = "";
    this.highlight = false;

    this.search = function () {
      var searchtext = $("#searchBox")[0].value;
      if (searchtext) {
	var pos = this.fieldContents.indexOf(searchtext);
	this.highlight = (pos !== -1) && [pos, pos + searchtext.length];
      } else {
	this.highlight = false;
      }
    };

    forever {
      assert highlight(this.highlight);

      on message jQuery("#searchBox", "input", $event) {
        this.search();
      }

      on asserted fieldContents($text, _) {
        this.fieldContents = text;
        this.search();
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Main

$(document).ready(function () {
  ground dataspace G {
    Syndicate.JQuery.spawnJQueryDriver();
    Syndicate.DOM.spawnDOMDriver();

    spawnGui();
    spawnModel();
    spawnSearch();
  }

  G.dataspace.onStateChange = function (mux, patch) {
    $("#spy-holder").text(Syndicate.prettyTrie(mux.routingTable));
  };
});
