assertion type point(color, x, y);

var brokerConnection = Syndicate.Broker.brokerConnection;
var toBroker = Syndicate.Broker.toBroker;
var fromBroker = Syndicate.Broker.fromBroker;

ground dataspace G {
  Syndicate.UI.spawnUIDriver();
  Syndicate.Timer.spawnTimerDriver();
  Syndicate.Broker.spawnBrokerClientDriver();

  actor {
    var ui = new Syndicate.UI.Anchor();
    var color = tinycolor('hsl ' + (Math.random() * 360 | 0) + ' 100% 50%').toHexString();
    var x = 0;
    var y = 0;
    var publishedX = x;
    var publishedY = y;

    function clamp(v) {
      var limit = 9.8;
      return Math.max(Math.min(v, limit), -limit);
    }

    var wsurl = 'wss://demo-broker.syndicate-lang.org:8443/';
    react {
      assert brokerConnection(wsurl);

      assert Syndicate.UI.uiAttribute('rect#my_color', 'fill', color);

      assert toBroker(wsurl, point(color, publishedX, publishedY));
      on message Syndicate.Timer.periodicTick(100) {
        publishedX = x;
        publishedY = y;
      }

      on message Syndicate.UI.windowEvent('deviceorientation', $e) {
        var scale = 0.5;
        x = clamp(e.gamma * scale);
        y = clamp((e.beta - 40) * scale);
      }

      during fromBroker(wsurl, point($oc, $ox, $oy)) {
        assert ui.context(oc)
          .html('#container',
                Mustache.render(document.getElementById('circle-template').innerHTML, {
                  color: oc,
                  x: ox,
                  y: oy
                }));
      }
    }
  }
}

// G.dataspace.setOnStateChange(function (mux, patch) {
//   document.getElementById("ds-state").innerText = Syndicate.prettyTrie(mux.routingTable);
// });
