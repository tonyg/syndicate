/*
  To Do (ho ho ho)
  spec is at: https://github.com/tastejs/todomvc/blob/master/app-spec.md

  - file layout?
  - README
  - pattern the HTML more explicitly on the given template, keep changes to a minimum
  - code style https://github.com/tastejs/todomvc/blob/master/contributing.md#code-style

  - persist to localStorage; use correct keys and name.
  - BUG: transitions don't happen because the nodes are being replaced rather than edited.
 */

assertion type todoExists(id);
assertion type todo(id, title, completed);

message type createTodo(title);
message type setTitle(id, title);
message type setCompleted(id, completed);
message type deleteTodo(id);
message type clearCompletedTodos();
message type setAllCompleted(completed);

// Derived model state
assertion type activeTodoCount(n);
assertion type completedTodoCount(n);
assertion type totalTodoCount(n);
assertion type allCompleted();

// View state
assertion type show(completed);

//////////////////////////////////////////////////////////////////////////

function todoListItemModel(initialId, initialTitle, initialCompleted) {
  actor {
    this.id = initialId;
    this.title = initialTitle;
    this.completed = initialCompleted;

    react {
      assert todoExists(this.id);
      assert todo(this.id, this.title, this.completed);

      on message setCompleted(this.id, $v) { this.completed = v; }
      on message setAllCompleted($v)       { this.completed = v; }

      on message setTitle(this.id, $v)     { this.title = v;     }

      on message clearCompletedTodos() {
        if (this.completed) :: deleteTodo(this.id);
      }
    } until {
      case message deleteTodo(this.id);
    }
  }
}

///////////////////////////////////////////////////////////////////////////

var ESCAPE_KEY_CODE = 27;
var ENTER_KEY_CODE = 13;

function getTemplate(id) {
  return document.getElementById(id).innerHTML;
}

function todoListItemView(id) {
  actor {
    this.ui = new Syndicate.UI.Anchor();
    this.editing = false;
    react {
      during todo(id, $title, $completed) {
        during show(completed) {
          assert this.ui.html('.todo-list',
                              Mustache.render(getTemplate(this.editing
                                                          ? 'todo-list-item-edit-template'
                                                          : 'todo-list-item-view-template'),
                                              {
                                                id: id,
                                                title: title,
                                                completed_class: completed ? "completed" : "",
                                                checked: completed ? "checked" : "",
                                              }),
                              id);
        }
      }

      on message this.ui.event('.toggle', 'change', $e) {
        :: setCompleted(id, e.target.checked);
      }

      on message this.ui.event('.destroy', 'click', _) {
        :: deleteTodo(id);
      }

      on message this.ui.event('label', 'dblclick', _) {
        this.editing = true;
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
        var newTitle = e.target.value.trim();
        :: (newTitle ? setTitle(id, newTitle) : deleteTodo(id));
        this.editing = false;
      }
    } until {
      case retracted todoExists(id);
    }
  }
}

///////////////////////////////////////////////////////////////////////////

ground dataspace G {
  Syndicate.UI.spawnUIDriver();

  actor {
    react {
      on message Syndicate.UI.globalEvent('.new-todo', 'change', $e) {
        var newTitle = e.target.value.trim();
        if (newTitle) :: createTodo(newTitle);
        e.target.value = "";
      }
    }
  }

  actor {
    this.ui = new Syndicate.UI.Anchor();

    react {
      during activeTodoCount($count) {
        assert this.ui.context('count').html('.todo-count strong', '' + count);
        assert this.ui.context('plural').html('.todo-count span.s', 's') when (count !== 1);
      }

      during totalTodoCount(0) {
        assert Syndicate.UI.uiAttribute('section.main', 'class', 'hidden');
        assert Syndicate.UI.uiAttribute('footer.footer', 'class', 'hidden');
      }

      during completedTodoCount(0) {
        assert Syndicate.UI.uiAttribute('button.clear-completed', 'class', 'hidden');
      }
      on message Syndicate.UI.globalEvent('button.clear-completed', 'click', _) {
        :: clearCompletedTodos();
      }

      during allCompleted() {
        do      { :: Syndicate.UI.setProperty('.toggle-all', 'checked', true); }
        finally { :: Syndicate.UI.setProperty('.toggle-all', 'checked', false); }
      }
      on message Syndicate.UI.globalEvent('.toggle-all', 'change', $e) {
        :: setAllCompleted(e.target.checked);
      }

      on asserted todoExists($id) {
        todoListItemView(id);
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
      assert allCompleted() when (completedCount > 0 && activeCount === 0);
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

  actor {
    var db;

    if ('todos-syndicate' in localStorage) {
      db = JSON.parse(localStorage['todos-syndicate']);
      for (var i in db.todos) {
        var t = db.todos[i];
        todoListItemModel(t.id, t.title, t.completed);
      }
    } else {
      db = {nextId: 0, todos: {}};
      react until {
        case asserted Syndicate.observe(createTodo(_)) {
          :: createTodo('Buy milk');
          :: createTodo('Buy bread');
          :: createTodo('Finish PhD');
        }
      }
    }

    react {
      on message createTodo($title) {
        todoListItemModel(db.nextId++, title, false);
      }

      during todo($id, $title, $completed) {
        do {
          db.todos[id] = {id: id, title: title, completed: completed};
          localStorage['todos-syndicate'] = JSON.stringify(db);
        }
        finally {
          delete db.todos[id];
          localStorage['todos-syndicate'] = JSON.stringify(db);
        }
      }
    }
  }
}

// G.dataspace.setOnStateChange(function (mux, patch) {
//   document.getElementById("ds-state").innerText = Syndicate.prettyTrie(mux.routingTable);
// });
