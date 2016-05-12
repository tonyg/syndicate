

assertion type todo(id, task, completed, assignee);
message type deleteTodo(id);

var nextId = 0;
function addTodo(task) {
  actor {
    this.id = nextId++;
    this.ui = new Syndicate.UI.Anchor();
    this.task = task;
    this.completed = false;
    this.assignee = null;
    react {
      assert todo(this.id, this.task, this.completed, this.assignee);
      assert this.ui.html('#todo-list',
                     Mustache.render($('#todo-list-item-template').html(), {
                       id: this.id,
                       checked: this.completed ? "checked" : "",
                       task: this.task
                 }));
      on message this.ui.event('.toggle', 'click', $e) {
        console.log('toggle clicked');
        this.completed = e.target.value;
      }
      on message this.ui.event('.destroy', 'click', _) {
        console.log('destroy clicked');
        :: deleteTodo(this.id);
      }
    } until {
      case message deleteTodo(this.id);
    }
  }
}

$(document).ready(function () {
  ground dataspace G {
    Syndicate.UI.spawnUIDriver();

    addTodo('Buy milk');
    addTodo('Buy bread');
    addTodo('Finish PhD');
  }

  G.dataspace.setOnStateChange(function (mux, patch) {
    $("#ds-state").text(Syndicate.prettyTrie(mux.routingTable));
  });
});
