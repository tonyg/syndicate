assertion type switchState(on);
assertion type powerDraw(watts);
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
    react {
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
    react {
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

    react {
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
    react {
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
    react {
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
// Timeout listener

function spawnTimeoutListener() {
  actor {
    react {
      during powerDraw($watts) {
        do {
          if (watts > 0) {
            var powerOnTime = Date.now();
            react {
              on asserted Syndicate.Timer.timeLaterThan(powerOnTime + 3000) {
                react { assert tvAlert('Stove on too long?'); }
              }
            }
          }
        }
      }
    }
  }
}

// function spawnTimeoutListener() {
//   actor {
//     react {
//       on asserted powerDraw($watts) {
//         if (watts > 0) {
//           var powerOnTime = Date.now();
//           react {
//             on asserted Syndicate.Timer.timeLaterThan(powerOnTime + 3000) {
//               react { assert tvAlert('Stove on too long?'); }
//             }
//           } until {
//             case asserted powerDraw(0); // alt: on retracted powerDraw(watts);
//           }
//         }
//       }
//     }
//   }
// }

// function spawnTimeoutListener() {
//   actor {
//     this.mostRecentTime = 0;
//     this.powerOnTime = null;
//     react {
//       on asserted powerDraw($watts) {
//         this.powerOnTime = (watts > 0) ? this.mostRecentTime : null;
//       }
//       on message Syndicate.Timer.periodicTick(200) {
//         this.mostRecentTime = Date.now();
//       }
//       assert tvAlert('Stove on too long?')
//         when (this.powerOnTime !== null && this.mostRecentTime - this.powerOnTime > 3000);
//     }
//   }
// }

///////////////////////////////////////////////////////////////////////////
// Failure monitor

function spawnFailureMonitor() {
  actor {
    react {
      on retracted componentPresent($who) {
        react {
          assert tvAlert('FAILURE: ' + who);
        } until {
          case asserted componentPresent(who);
        }
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Chaos Monkey

function spawnChaosMonkey() {
  actor {
    react {
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
    Syndicate.Timer.spawnTimerDriver();

    spawnTV();
    spawnRemoteControl();
    spawnRemoteListener();
    spawnStoveSwitch();
    spawnPowerDrawMonitor();
    spawnTimeoutListener();

    spawnFailureMonitor();

    spawnChaosMonkey();
  }

  G.dataspace.setOnStateChange(function (mux, patch) {
    $("#ds-state").text(Syndicate.prettyTrie(mux.routingTable));
  });
});
