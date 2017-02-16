/*
  todomvc spec is at: https://github.com/tastejs/todomvc/blob/master/app-spec.md
  - BUG: transitions don't happen because the nodes are being replaced rather than edited.
 */

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
  spawn {
    field this.id = initialId;
    field this.title = initialTitle;
    field this.completed = initialCompleted;

    stop on message deleteTodo(this.id);

    assert todo(this.id, this.title, this.completed);

    on message setCompleted(this.id, $v) { this.completed = v; }
    on message setAllCompleted($v)       { this.completed = v; }

    on message setTitle(this.id, $v)     { this.title = v;     }

    on message clearCompletedTodos() {
      if (this.completed) :: deleteTodo(this.id);
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
  spawn {
    stop on retracted todo(id, _, _);

    this.ui = new Syndicate.UI.Anchor();
    field this.editing = false;

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
  }
}

///////////////////////////////////////////////////////////////////////////

ground dataspace G {
  Syndicate.UI.spawnUIDriver();

  spawn {
    on message Syndicate.UI.globalEvent('.new-todo', 'change', $e) {
      var newTitle = e.target.value.trim();
      if (newTitle) :: createTodo(newTitle);
      e.target.value = "";
    }
  }

  spawn {
    this.ui = new Syndicate.UI.Anchor();

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
      on start { :: Syndicate.UI.setProperty('.toggle-all', 'checked', true); }
      on stop  { :: Syndicate.UI.setProperty('.toggle-all', 'checked', false); }
    }
    on message Syndicate.UI.globalEvent('.toggle-all', 'change', $e) {
      :: setAllCompleted(e.target.checked);
    }

    on asserted todo($id, _, _) {
      todoListItemView(id);
    }
  }

  spawn {
    field this.completedCount = 0;
    field this.activeCount = 0;
    on asserted  todo($id, _, $c) { if (c) this.completedCount++; else this.activeCount++; }
    on retracted todo($id, _, $c) { if (c) this.completedCount--; else this.activeCount--; }
    assert activeTodoCount(this.activeCount);
    assert completedTodoCount(this.completedCount);
    assert totalTodoCount(this.activeCount + this.completedCount);
    assert allCompleted() when (this.completedCount > 0 && this.activeCount === 0);
  }

  spawn {
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

  spawn {
    var db;

    if ('todos-syndicate' in localStorage) {
      db = JSON.parse(localStorage['todos-syndicate']);
      for (var i in db.todos) {
        var t = db.todos[i];
        todoListItemModel(t.id, t.title, t.completed);
      }
    } else {
      db = {nextId: 0, todos: {}};
      on start {
        react {
          stop on asserted Syndicate.observe(createTodo(_)) {
            :: createTodo('Buy milk');
            :: createTodo('Buy bread');
            :: createTodo('Finish PhD');
          }
        }
      }
    }

    on message createTodo($title) {
      todoListItemModel(db.nextId++, title, false);
    }

    during todo($id, _, _) {
      during todo(id, $title, $completed) {
        on start {
          db.todos[id] = {id: id, title: title, completed: completed};
          localStorage['todos-syndicate'] = JSON.stringify(db);
        }
      }
      on stop {
        delete db.todos[id];
        localStorage['todos-syndicate'] = JSON.stringify(db);
      }
    }
  }
}
