assertion type switchState(on);
assertion type powerDraw(watts);
assertion type time(now);
assertion type remoteClick();
assertion type tvAlert(text);
assertion type switchAction(on);
assertion type componentPresent(name);

var DOM = Syndicate.DOM.DOM;
var jQueryEvent = Syndicate.JQuery.jQueryEvent;

///////////////////////////////////////////////////////////////////////////
// TV

function spawnTV() {
  actor {
    forever {
      during tvAlert($text) {
        assert DOM('#tv', 'alert', Syndicate.seal(["li", text]));
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Remote control and listener

function spawnRemoteControl() {
  actor {
    forever {
      assert componentPresent('remote control');
      on message jQueryEvent('#remote-control', 'click', _) {
        :: remoteClick();
      }
    }
  }
}

function spawnRemoteListener() {
  actor {
    this.stoveIsOn = false;
    // In principle, we should start up in "power undefined" state and
    // count clicks we get in that state; when we then learn the real
    // state, if we've been clicked, turn it off. We don't do this
    // here, for simplicity.

    forever {
      on asserted powerDraw($watts) {
        this.stoveIsOn = watts > 0;
      }

      on message remoteClick() {
        if (this.stoveIsOn) {
          :: switchAction(false);
        }
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Stove switch and power draw monitor

function spawnStoveSwitch() {
  actor {
    this.powerOn = false;
    state {
      assert componentPresent('stove switch');
      assert switchState(this.powerOn);

      assert DOM('#stove-switch', 'switch-state',
                 Syndicate.seal(["img", [["src",
                                          "img/stove-coil-element-" +
                                          (this.powerOn ? "hot" : "cold") + ".jpg"]]]));

      on message jQueryEvent('#stove-switch-on', 'click', _) { this.powerOn = true; }
      on message jQueryEvent('#stove-switch-off', 'click', _) { this.powerOn = false; }

      on message switchAction($newState) {
        this.powerOn = newState;
      }
    } until {
      case message jQueryEvent('#kill-stove-switch', 'click', _);
    }
  }
}

function spawnPowerDrawMonitor() {
  actor {
    this.watts = 0;
    state {
      assert componentPresent('power draw monitor');
      assert powerDraw(this.watts);

      assert DOM('#power-draw-meter', 'power-draw',
                 Syndicate.seal(["p", "Power draw: ",
                                 ["span", [["class", "power-meter-display"]],
                                  this.watts + " W"]]));

      on asserted switchState($on) {
        this.watts = on ? 1500 : 0;
      }
    } until {
      case message jQueryEvent('#kill-power-draw-monitor', 'click', _);
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Clock and "timeout listener"

function spawnClock() {
  actor {
    setInterval(Syndicate.Dataspace.wrap(function () {
      :: time(+(new Date()));
    }), 200);
    forever {
      assert componentPresent('real time clock');
    }
  }
}

function spawnTimeoutListener() {
  var message = tvAlert('Stove on too long?');
  actor {
    this.mostRecentTime = 0;
    this.powerOnTime = null;

    forever {
      on asserted powerDraw($watts) {
        this.powerOnTime = (watts > 0) ? this.mostRecentTime : null;
      }
      on message time($now) {
        this.mostRecentTime = now;
        if (this.powerOnTime !== null && this.mostRecentTime - this.powerOnTime > 3000) {
          Syndicate.Dataspace.stateChange(Syndicate.assert(message));
        } else {
          Syndicate.Dataspace.stateChange(Syndicate.retract(message));
        }
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Failure monitor

function spawnFailureMonitor() {
  function messageFor(who) {
    return tvAlert('FAILURE: ' + who);
  }

  actor {
    forever {
      on asserted componentPresent($who) {
        Syndicate.Dataspace.stateChange(Syndicate.retract(messageFor(who)));
      }
      on retracted componentPresent($who) {
        Syndicate.Dataspace.stateChange(Syndicate.assert(messageFor(who)));
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Chaos Monkey

function spawnChaosMonkey() {
  actor {
    forever {
      on message jQueryEvent('#spawn-power-draw-monitor', 'click', _) {
        spawnPowerDrawMonitor();
      }
      on message jQueryEvent('#spawn-stove-switch', 'click', _) {
        spawnStoveSwitch();
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

    spawnTV();
    spawnRemoteControl();
    spawnRemoteListener();
    spawnStoveSwitch();
    spawnPowerDrawMonitor();
    spawnClock();
    spawnTimeoutListener();

    spawnFailureMonitor();

    spawnChaosMonkey();
  }

  G.dataspace.onStateChange = function (mux, patch) {
    $("#ds-state").text(Syndicate.prettyTrie(mux.routingTable));
  };
});
