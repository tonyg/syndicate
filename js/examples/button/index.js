var DOM = Syndicate.DOM.DOM;
var jQueryEvent = Syndicate.JQuery.jQueryEvent;

$(document).ready(function() {
  ground dataspace {
    Syndicate.DOM.spawnDOMDriver();
    Syndicate.JQuery.spawnJQueryDriver();

    actor {
      this.counter = 0;
      react {
        assert DOM('#button-label', '', Syndicate.seal(this.counter));
        on message jQueryEvent('#counter', 'click', _) {
          this.counter++;
        }
      }
    }
  }
});
