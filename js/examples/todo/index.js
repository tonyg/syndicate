assertion type todo(id, title, completed);
assertion type show(completed);
assertion type activeTodoCount(n);
assertion type completedTodoCount(n);
assertion type totalTodoCount(n);

message type deleteTodo(id);
message type clearCompletedTodos();

/*
  To Do (ho ho ho)
  spec is at: https://github.com/tastejs/todomvc/blob/master/app-spec.md

  - file layout?
  - README
  - pattern the HTML more explicitly on the given template, keep changes to a minimum
  - code style https://github.com/tastejs/todomvc/blob/master/contributing.md#code-style

  - mark all as complete/incomplete; make sure it is only ever checked when all the todos are checked
  - persist to localStorage; use correct keys and name.

  - routing: spec requires that filtering be done "on a model level";
    we, by using "hidden" class, are kind of partly doing it on a view
    level. We could either continue to do this, or switch to a proper
    model level approach, but then we'd lose stability of ordering!

  - BUG: doesn't hide an item if in "Active" state and you click on the checkbox
 */

var ESCAPE_KEY_CODE = 27;
var ENTER_KEY_CODE = 13;

function getTemplate(id) {
  return document.getElementById(id).innerHTML;
}

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

      assert this.ui.html('.todo-list',
                          Mustache.render(getTemplate(this.editing
                                                      ? 'todo-list-item-edit-template'
                                                      : 'todo-list-item-view-template'),
                                          {
                                            completed_class: this.completed ? "completed" : "",
                                            hidden_class: this.visible ? "" : "hidden",
                                            id: this.id,
                                            checked: this.completed ? "checked" : "",
                                            title: this.title
                                          }),
                         this.id);

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

      on message clearCompletedTodos() {
        if (this.completed) :: deleteTodo(this.id);
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
    var completedCount = 0;
    var activeCount = 0;
    react {
      on asserted  todo(_, _, $completed) { if (completed) completedCount++; else activeCount++; }
      on retracted todo(_, _, $completed) { if (completed) completedCount--; else activeCount--; }
      assert activeTodoCount(activeCount);
      assert completedTodoCount(completedCount);
      assert totalTodoCount(activeCount + completedCount);
    }
  }

  actor {
    var ui = new Syndicate.UI.Anchor();
    react {
      during activeTodoCount($count) {
        assert ui.context('count').html('.todo-count strong', '' + count);
        assert ui.context('plural').html('.todo-count span.s', 's') when (count !== 1);
      }
      during completedTodoCount(0) {
        assert Syndicate.UI.uiAttribute('button.clear-completed', 'class', 'hidden');
      }
      during totalTodoCount(0) {
        assert Syndicate.UI.uiAttribute('section.main', 'class', 'hidden');
        assert Syndicate.UI.uiAttribute('footer.footer', 'class', 'hidden');
      }
      on message Syndicate.UI.globalEvent('button.clear-completed', 'click', _) {
        :: clearCompletedTodos();
      }
    }
  }

  actor {
    react {
      during Syndicate.UI.locationHash($hash) {
        assert Syndicate.UI.uiAttribute('ul.filters > li > a[href="#'+hash+'"]',
                                        'class', 'selected');
      }

      during Syndicate.UI.locationHash('/') {
        assert show(true);
        assert show(false);
      }
      during Syndicate.UI.locationHash('/active') {
        assert show(false);
      }
      during Syndicate.UI.locationHash('/completed') {
        assert show(true);
      }
    }
  }

  addTodo('Buy milk');
  addTodo('Buy bread');
  addTodo('Finish PhD');
}

// G.dataspace.setOnStateChange(function (mux, patch) {
//   document.getElementById("ds-state").innerText = Syndicate.prettyTrie(mux.routingTable);
// });
