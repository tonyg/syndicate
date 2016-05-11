var DOM = Syndicate.DOM.DOM;

$(document).ready(function () {
  ground dataspace G {
    Syndicate.DOM.spawnDOMDriver();
    Syndicate.Timer.spawnTimerDriver();

    actor {
      react {
        assert DOM('#clock', 'clock',
                   '<svg width="300px" viewBox="0 0 100 100">'+
                   '<circle fill="#0B79CE" r=45 cx=50 cy=50/>'+
                   '<line stroke="#023963" x1=50 y1=50 x2='+this.handX+' y2='+this.handY+' />'+
                   '</svg>')
          when (typeof this.angle === 'number');

        on message Syndicate.Timer.periodicTick(1000) {
          this.angle = ((((Date.now() / 1000) % 60) / 60) - 0.25) * 2 * Math.PI;
          this.handX = 50 + 40 * Math.cos(this.angle);
          this.handY = 50 + 40 * Math.sin(this.angle);
        }
      }
    }
  }
});
