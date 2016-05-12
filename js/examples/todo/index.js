assertion type todo(id, title, completed);
message type deleteTodo(id);
assertion type show(completed);
assertion type currentLocationHash(hash);

/*
  To Do (ho ho ho)
  spec is at: https://github.com/tastejs/todomvc/blob/master/app-spec.md

  - file layout?
  - README
  - pattern the HTML more explicitly on the given template, keep changes to a minimum
  - code style https://github.com/tastejs/todomvc/blob/master/contributing.md#code-style

  - no todos: make sure #main and #footer are hidden
  - mark all as complete/incomplete; make sure it is only ever checked when all the todos are checked
  - count of active todos; pluralize correctly; format correctly
  - hide "clear completed" button when no completed todos exist
  - persist to localStorage; use correct keys and name.

  - routing: spec requires that filtering be done "on a model level";
    we, by using "hidden" class, are kind of partly doing it on a view
    level. We could either continue to do this, or switch to a proper
    model level approach, but then we'd lose stability of ordering!
 */

var ESCAPE_KEY_CODE = 27;
var ENTER_KEY_CODE = 13;

var nextId = 0;
function addTodo(title) {
  title = title.trim();
  if (!title) return;

  actor {
    this.id = nextId++;
    this.ui = new Syndicate.UI.Anchor();
    this.title = title;
    this.completed = false;
    this.editing = false;
    this.visible = false;

    react {
      assert todo(this.id, this.title, this.completed);

      during show(this.completed) {
        do      { this.visible = true;  }
        finally { this.visible = false; }
      }

      assert this.ui.html('#todo-list',
                          Mustache.render($(this.editing
                                            ? '#todo-list-item-edit-template'
                                            : '#todo-list-item-view-template').html(),
                                          {
                                            completed_class: this.completed ? "completed" : "",
                                            hidden_class: this.visible ? "" : "hidden",
                                            id: this.id,
                                            checked: this.completed ? "checked" : "",
                                            title: this.title
                                          }));

      on message this.ui.event('.toggle', 'change', $e) {
        this.completed = e.target.checked;
      }

      on message this.ui.event('.destroy', 'click', _) {
        :: deleteTodo(this.id);
      }

      on message this.ui.event('label', 'dblclick', _) {
        var self = this;
        this.editing = true;
        focusMe(); // TODO this is gross
        function focusMe() {
          setTimeout(function () {
            var q = 'li[data-id="'+self.id+'"] input.edit';
            var n = document.querySelector(q);
            if (!n) { return focusMe(); }
            n.focus();
            n.setSelectionRange(n.value.length, n.value.length);
          }, 0);
        }
      }

      on message this.ui.event('input.edit', 'keyup', $e) {
        if (e.keyCode === ESCAPE_KEY_CODE || e.keyCode === ENTER_KEY_CODE) {
          this.editing = false;
        }
      }
      on message this.ui.event('input.edit', 'blur', $e) {
        this.editing = false;
      }
      on message this.ui.event('input.edit', 'change', $e) {
        this.title = e.target.value.trim();
        this.editing = false;
        if (!this.title) :: deleteTodo(this.id);
      }
    } until {
      case message deleteTodo(this.id);
    }
  }
}

ground dataspace G {
  Syndicate.UI.spawnUIDriver();

  actor {
    react {
      on message Syndicate.UI.globalEvent('.new-todo', 'change', $e) {
        addTodo(e.target.value);
        e.target.value = "";
      }
    }
  }

  actor {
    this.hash = hashFrom(document.location.toString());
    function hashFrom(u) {
      var i = u.indexOf('#');
      return (i !== -1) ? u.slice(i + 1) : '/';
    }
    react {
      assert currentLocationHash(this.hash);
      on message Syndicate.UI.windowEvent('hashchange', $e) {
        this.hash = hashFrom(e.newURL);
      }
    }
  }

  actor {
    react {
      on asserted currentLocationHash($hash) {
        // TODO this is a bit icky too
        var ns = document.querySelectorAll('ul.filters > li > a');
        for (var i = 0; i < ns.length; i++) { ns[i].className = ''; }
        var n = document.querySelector('ul.filters > li > a[href="#'+hash+'"]');
        n.className = 'selected';
      }

      during currentLocationHash('/') {
        assert show(true);
        assert show(false);
      }
      during currentLocationHash('/active') {
        assert show(false);
      }
      during currentLocationHash('/completed') {
        assert show(true);
      }
    }
  }

  addTodo('Buy milk');
  addTodo('Buy bread');
  addTodo('Finish PhD');
}

G.dataspace.setOnStateChange(function (mux, patch) {
  $("#ds-state").text(Syndicate.prettyTrie(mux.routingTable));
});
