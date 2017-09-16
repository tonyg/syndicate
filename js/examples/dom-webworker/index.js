document.addEventListener('DOMContentLoaded', function () {
  ground dataspace G {
    Syndicate.UI.spawnUIDriver();

    spawn {
      var ui = new Syndicate.UI.Anchor();
      during Syndicate.observe('bump_count') { // wait for the worker to boot and start listening
        assert ui.html('#clicker-holder',
                       '<button><span style="font-style: italic">Click me!</span></button>');
      }
      on message Syndicate.UI.globalEvent('#clicker-holder > button', 'click', _) {
        :: 'bump_count';
      }
    }

    Syndicate.Dataspace.spawn(new Syndicate.Worker('worker.expanded.js'));
  }

  G.dataspace.setOnStateChange(function (mux, patch) {
    document.getElementById('spy-holder').innerText = Syndicate.prettyTrie(mux.routingTable);
  });
});
