assertion type present(name, status);
assertion type says(who, message);

var DOM = Syndicate.DOM.DOM;
var jQueryEvent = Syndicate.JQuery.jQueryEvent;

///////////////////////////////////////////////////////////////////////////
// Application

function spawnChatApp() {
  $("#chat_form").submit(function (e) { e.preventDefault(); return false; });
  $("#nym_form").submit(function (e) { e.preventDefault(); return false; });
  if (!($("#nym").val())) { $("#nym").val("nym" + Math.floor(Math.random() * 65536)); }

  actor {
    forever {
      on asserted jQueryInput('#nym',    $v) { this.nym = v; }
      on asserted jQueryInput('#status', $v) { this.status = v; }

      on asserted brokerConnected($url) { outputState('connected to ' + url); }
      on retracted brokerConnected($url) { outputState('disconnected from ' + url); }

      during jQueryInput('#wsurl', $url) {
        assert brokerConnection(url);

        on message Syndicate.WakeDetector.wakeEvent() {
          :: forceBrokerDisconnect(url);
        }

        assert toBroker(url, present(this.nym, this.status));
        during fromBroker(url, present($who, $status)) {
          assert DOM('#nymlist', 'present-nym', Syndicate.seal(
            ["li",
             ["span", [["class", "nym"]], who],
             ["span", [["class", "nym_status"]], status]]));
        }

        on message jQueryEvent('#send_chat', 'click', _) {
          var inp = $("#chat_input");
          var utterance = inp.val();
          inp.val("");
          if (utterance) :: toBroker(url, says(this.nym, utterance));
        }

        on message fromBroker(url, says($who, $what)) {
          outputUtterance(who, what);
        }
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Adding items to the transcript panel (plain Javascript/jQuery)

function outputItem(item) {
  var stamp = $("<span/>").text((new Date()).toGMTString()).addClass("timestamp");
  var item = $("<div/>").append([stamp].concat(item));
  var o = $("#chat_output");
  o.append(item);
  o[0].scrollTop = o[0].scrollHeight;
  return item;
}

function outputState(state) {
  outputItem([$("<span/>").text(state).addClass(state).addClass("state")])
    .addClass("state_" + state);
}

function outputUtterance(who, what) {
  outputItem([$("<span/>").text(who).addClass("nym"),
	      $("<span/>").text(what).addClass("utterance")]).addClass("utterance");
}

///////////////////////////////////////////////////////////////////////////
// Input control value monitoring

assertion type jQueryInput(selector, value);

function spawnInputChangeMonitor() {
  actor {
    forever {
      on asserted Syndicate.observe(jQueryInput($selector, _)) {
        actor {
          this.value = $(selector).val();
          state {
            assert jQueryInput(selector, this.value);
            on message jQueryEvent(selector, 'change', $e) {
              this.value = e.target.value;
            }
          } until {
            case retracted Syndicate.observe(jQueryInput(selector, _));
          }
        }
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
    Syndicate.WakeDetector.spawnWakeDetector();
    spawnBrokerClientDriver();
    spawnInputChangeMonitor();
    spawnChatApp();
  }

  // G.dataspace.setOnStateChange(function (mux, patch) {
  //   $("#ds-state").text(Syndicate.prettyTrie(mux.routingTable));
  // });
});
