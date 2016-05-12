assertion type todo(id, task, completed);
message type deleteTodo(id);
assertion type show(completed);
assertion type currentLocationHash(hash);

var nextId = 0;
function addTodo(task) {
  actor {
    this.id = nextId++;
    this.ui = new Syndicate.UI.Anchor();
    this.task = task;
    this.completed = false;
    this.editing = false;
    this.visible = false;

    react {
      assert todo(this.id, this.task, this.completed);
      during show(this.completed) {
        do {
          this.visible = true;
          console.log('shown', this.id, this.task, this.visible);
        }
        finally {
          this.visible = false;
          console.log('hidden', this.id, this.task, this.visible);
        }
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
                                            task: this.task
                                          }));
      on message this.ui.event('.toggle', 'change', $e) {
        this.completed = e.target.checked;
      }
      on message this.ui.event('.destroy', 'click', _) {
        console.log('destroy clicked');
        :: deleteTodo(this.id);
      }
      on message this.ui.event('label', 'dblclick', _) {
        this.editing = true;
      }
      on message this.ui.event('input.edit', 'change', $e) {
        this.task = e.target.value;
        this.editing = false;
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
      during currentLocationHash('/') {
        do { console.log('set hash to /'); }
        assert show(true);
        assert show(false);
      }
      during currentLocationHash('/active') {
        do { console.log('set hash to /active'); }
        assert show(false);
      }
      during currentLocationHash('/completed') {
        do { console.log('set hash to /completed'); }
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
