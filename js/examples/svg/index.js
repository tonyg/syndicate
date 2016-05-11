var DOM = Syndicate.DOM.DOM;
var jQueryEvent = Syndicate.JQuery.jQueryEvent;

assertion type time(value);

$(document).ready(function () {
  ground dataspace G {
    Syndicate.JQuery.spawnJQueryDriver();
    Syndicate.DOM.spawnDOMDriver();

    actor {
      setInterval(Syndicate.Dataspace.wrap(function () {
        :: time(+(new Date()));
      }), 1000);

      react {
        on message time($now) {
          this.angle = (((now / 1000) % 60) / 60) * 2 * Math.PI;
          this.handX = 50 + 40 * Math.cos(this.angle);
          this.handY = 50 + 40 * Math.sin(this.angle);
        }
        assert DOM('#clock', 'clock',
                   '<svg width="300px" viewBox="0 0 100 100">'+
                   '<circle fill="#0B79CE" r=45 cx=50 cy=50/>'+
                   '<line stroke="#023963" x1=50 y1=50 x2='+this.handX+' y2='+this.handY+' />'+
                   '</svg>')
          when (typeof this.angle === 'number');
      }
    }
  }
});
