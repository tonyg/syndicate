assertion type switchState(on);
assertion type powerDraw(watts);
assertion type remoteClick();
assertion type tvAlert(text);
assertion type switchAction(on);
assertion type componentPresent(name);

///////////////////////////////////////////////////////////////////////////
// TV

function spawnTV() {
  actor {
    var ui = new Syndicate.UI.Anchor();
    react {
      during tvAlert($text) {
        assert ui.context(text).html('#tv', Mustache.render($('#alert_template').html(), { text: text }));
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
      on message Syndicate.UI.globalEvent('#remote-control', 'click', _) {
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
    field this.powerOn = false;
    this.ui = new Syndicate.UI.Anchor();
    react {
      assert componentPresent('stove switch');
      assert switchState(this.powerOn);

      assert this.ui.html('#stove-switch',
                          Mustache.render($('#stove_element_template').html(),
                                          { imgurl: ("img/stove-coil-element-" +
                                                     (this.powerOn ? "hot" : "cold") + ".jpg") }));

      on message Syndicate.UI.globalEvent('#stove-switch-on', 'click', _) { this.powerOn = true; }
      on message Syndicate.UI.globalEvent('#stove-switch-off', 'click', _) { this.powerOn = false; }

      on message switchAction($newState) {
        this.powerOn = newState;
      }
    } until {
      case message Syndicate.UI.globalEvent('#kill-stove-switch', 'click', _);
    }
  }
}

function spawnPowerDrawMonitor() {
  actor {
    field this.watts = 0;
    this.ui = new Syndicate.UI.Anchor();
    react {
      assert componentPresent('power draw monitor');
      assert powerDraw(this.watts);

      assert this.ui.html('#power-draw-meter',
                          Mustache.render($('#power_draw_template').html(), { watts: this.watts }));

      on asserted switchState($on) {
        this.watts = on ? 1500 : 0;
      }
    } until {
      case message Syndicate.UI.globalEvent('#kill-power-draw-monitor', 'click', _);
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
              on asserted Syndicate.Timer.timeLaterThan(powerOnTime + 10000) {
                $("img.flames").show();
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
    monitorComponent('power draw monitor',
                     '#spawn-power-draw-monitor',
                     '#kill-power-draw-monitor',
                     spawnPowerDrawMonitor);
    monitorComponent('stove switch',
                     '#spawn-stove-switch',
                     '#kill-stove-switch',
                     spawnStoveSwitch);
  }

  function monitorComponent(name, spawnButtonSelector, killButtonSelector, spawnFunction) {
    var jSpawnButtons = $(spawnButtonSelector);
    var jKillButtons = $(killButtonSelector);
    react {
      during componentPresent(name) {
        do {
          jSpawnButtons.prop('disabled', true);
          jKillButtons.prop('disabled', false);
        }
        finally {
          jSpawnButtons.prop('disabled', false);
          jKillButtons.prop('disabled', true);
        }
      }
      on message Syndicate.UI.globalEvent(spawnButtonSelector, 'click', _) {
        spawnFunction();
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////
// Main

$(document).ready(function () {
  ground dataspace G {
    Syndicate.UI.spawnUIDriver();
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
