assertion type DOM(containerSelector, fragmentClass, spec);
assertion type jQuery(selector, eventType, event);

$(document).ready(function() {
  ground network {
    Syndicate.DOM.spawnDOMDriver();
    Syndicate.JQuery.spawnJQueryDriver();

    actor {
      this.counter = 0;
      forever {
        assert DOM('#button-label', '', Syndicate.seal(this.counter));
        on message jQuery('#counter', 'click', _) {
          this.counter++;
        }
      }
    }
  }
});
