assertion type page(name, order);
assertion type currentPage(name);

function spawnNavBar(defaultPage) {
  spawn {
    field this.currentPage = defaultPage;
    assert currentPage(this.currentPage);
    on message Syndicate.UI.globalEvent('.nav-btn', 'click', $e) {
      // INVARIANT: the id of each nav bar button is of the form 'nav-btn-Name',
      // where Name is the name of the page associated with that button
      var newPage = e.target.id.match(/nav-btn-(.*)/)[1];
      this.currentPage = newPage;
    }
  }
}

// It seems conceivable that a transition between pages (currentPage changing)
// could see a 'glitch' where both the previous and the next page are asserting
// '#page-contents'.
//
// This does not seem to manifest with the current scheduling. There's also the
// possiblity that it *is* happening and my feeble human perception cannot
// detect it. Or, perhaps, there is something I am missing and there is some
// reason the glitch does not happen.
function spawnPage(pg) {
  spawn {
    this.ui = new Syndicate.UI.Anchor();
    this.ctx = this.ui.context(pg.name);

    assert this.ctx.html('#nav-bar', Mustache.render($('#nav-bar-btn').html(), {name: pg.name}), pg.order);

    during currentPage(pg.name) {
        assert this.ui.html('#page-contents', pg.contents);
      }
  }
}

var homePage = {
  name: "Home",
  order: 1,
  contents: Mustache.render($('#home-contents').html())
};

var computersPage = {
  name: "Computers",
  order: 2,
  contents: Mustache.render($('#computers-contents').html())
};

var televisionsPage = {
  name: "Televisions",
  order: 3,
  contents: Mustache.render($('#televisions-contents').html())
};

var stereosPage = {
  name: "Stereos",
  order: 4,
  contents: Mustache.render($('#stereos-contents').html())
};

var pages = [homePage, computersPage, televisionsPage, stereosPage];
var defaultPage = homePage.name;

$(document).ready(function () {
  ground dataspace G {
    Syndicate.UI.spawnUIDriver();
    spawnNavBar(defaultPage);
    pages.forEach(spawnPage);
  }
  // debugging
  G.dataspace.setOnStateChange(function (mux, patch) {
    $("#ds-state").text(Syndicate.prettyTrie(mux.routingTable));
  });
});
