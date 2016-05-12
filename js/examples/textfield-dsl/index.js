///////////////////////////////////////////////////////////////////////////
// GUI

var globalEvent = Syndicate.UI.globalEvent;
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
      var hLeft = highlight ? highlight[0] : 0;
      var hRight = highlight ? highlight[1] : 0;
      document.getElementById("fieldContents").innerHTML = highlight
	? piece(text, pos, 0, hLeft, "normal") +
	piece(text, pos, hLeft, hRight, "highlight") +
	piece(text, pos, hRight, text.length + 1, "normal")
	: piece(text, pos, 0, text.length + 1, "normal");
    };

    react {
      on message globalEvent("#inputRow", "+keydown", $event) {
        switch (event.keyCode) {
          case 37 /* left  */: :: fieldCommand("cursorLeft"); break;
          case 39 /* right */: :: fieldCommand("cursorRight"); break;
          case 9 /* tab */: /* ignore */ break;
          case 8 /* backspace */:
            event.preventDefault(); // that this works here is a minor miracle
	    :: fieldCommand("backspace");
            break;
          default: break;
        }
      }

      on message globalEvent("#inputRow", "+keypress", $event) {
        var character = String.fromCharCode(event.charCode);
        if (event.charCode && character) {
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
    this.fieldValue = "initial";
    this.cursorPos = this.fieldValue.length; /* positions address gaps between characters */

    react {
      assert fieldContents(this.fieldValue, this.cursorPos);

      on message fieldCommand("cursorLeft") {
	this.cursorPos--;
	if (this.cursorPos < 0)
	  this.cursorPos = 0;
      }

      on message fieldCommand("cursorRight") {
	this.cursorPos++;
	if (this.cursorPos > this.fieldValue.length)
	  this.cursorPos = this.fieldValue.length;
      }

      on message fieldCommand("backspace") {
        if (this.cursorPos > 0) {
	  this.fieldValue =
	    this.fieldValue.substring(0, this.cursorPos - 1) +
	    this.fieldValue.substring(this.cursorPos);
	  this.cursorPos--;
        }
      }

      on message fieldCommand(["insert", $newText]) {
	this.fieldValue =
	  this.fieldValue.substring(0, this.cursorPos) +
	  newText +
	  this.fieldValue.substring(this.cursorPos);
	this.cursorPos += newText.length;
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Search engine

function spawnSearch() {
  actor {
    this.fieldValue = "";
    this.highlight = false;

    this.search = function () {
      var searchtext = document.getElementById("searchBox").value;
      if (searchtext) {
	var pos = this.fieldValue.indexOf(searchtext);
	this.highlight = (pos !== -1) && [pos, pos + searchtext.length];
      } else {
	this.highlight = false;
      }
    };

    react {
      assert highlight(this.highlight);

      on message globalEvent("#searchBox", "input", $event) {
        this.search();
      }

      on asserted fieldContents($text, _) {
        this.fieldValue = text;
        this.search();
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Main

ground dataspace G {
  Syndicate.UI.spawnUIDriver();

  spawnGui();
  spawnModel();
  spawnSearch();
}

G.dataspace.setOnStateChange(function (mux, patch) {
  document.getElementById("spy-holder").innerText = Syndicate.prettyTrie(mux.routingTable);
});
