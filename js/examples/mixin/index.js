ground dataspace G {
  message type shiftClicked(fragmentId);

  Syndicate.UI.spawnUIDriver();

  spawn {
    var uiRoot = new Syndicate.UI.Anchor();

    assert uiRoot.html('#place', '<svg id="svgroot" width="100%" height="100%"/>');

    spawn {
      var ui = new Syndicate.UI.Anchor();
      assert ui.html('#svgroot',
                     '<rect id="underlay" x="0" y="0" width="100%" height="100%" fill="grey"/>',
                     -1);
      on message ui.event('.', 'click', $e) {
        var svg = document.getElementById('svgroot');
        var pt = svg.createSVGPoint();
        pt.x = e.clientX;
        pt.y = e.clientY;
        pt = pt.matrixTransform(svg.getScreenCTM().inverse());
        spawnRectangle(pt.x, pt.y);
      }
    }

    spawn {
      field this.x = 50;
      field this.y = 50;
      var ui = new Syndicate.UI.Anchor();
      assert ui.html('#svgroot',
                     '<circle fill="green" r=45 cx="'+this.x+'" cy="'+this.y+'"/>',
                     0);
      draggableMixin(this, ui);
    }

    ///////////////////////////////////////////////////////////////////////////

    function spawnRectangle(x0, y0) {
      var length = 90;
      spawn {
        field this.x = x0 - length / 2;
        field this.y = y0 - length / 2;
        var ui = new Syndicate.UI.Anchor();
        assert ui.html('#svgroot',
                       '<rect fill="yellow" stroke="black" stroke-width="3" width="90" height="90"'+
                       ' x="'+this.x+'" y="'+this.y+'"/>',
                       0);
        draggableMixin(this, ui);
        on message ui.event('.', 'mousedown', $e) {
          if (e.shiftKey) { :: shiftClicked(ui.fragmentId); }
        }
        stop on message shiftClicked(ui.fragmentId);
      }
    }

    function draggableMixin(obj, ui) {
      idle();

      function idle() {
        react {
          stop on message ui.event('.', 'mousedown', $e) {
            dragging(e.clientX - obj.x, e.clientY - obj.y);
          }
        }
      }

      function dragging(dx, dy) {
        react {
          on message uiRoot.event('.', 'mousemove', $e) {
            obj.x = e.clientX - dx;
            obj.y = e.clientY - dy;
          }
          stop on message uiRoot.event('.', 'mouseup', _) {
            idle();
          }
        }
      }
    }
  }
}
