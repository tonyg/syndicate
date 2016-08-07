assertion type person(id, firstName, lastName, address, age);
message type setSortColumn(number);

function newRow(id, firstName, lastName, address, age) {
  actor named ('model' + id) {
    react {
      assert person(id, firstName, lastName, address, age);
    }
  }
}

function spawnModel() {
  newRow(1, 'Keith', 'Example', '94 Main St.', 44);
  newRow(2, 'Karen', 'Fakeperson', '5504 Long Dr.', 34);
  newRow(3, 'Angus', 'McFictional', '2B Pioneer Heights', 39);
  newRow(4, 'Sue', 'Donnem', '1 Infinite Loop', 104);
  newRow(5, 'Boaty', 'McBoatface', 'Arctic Ocean', 1);
}

function spawnView() {
  actor named 'view' {
    var ui = new Syndicate.UI.Anchor();
    field this.orderColumn = 2;

    function cell(text) {
      // Should escape text in a real application.
      return '<td>' + text + '</td>';
    }

    react {
      on message setSortColumn($c) { this.orderColumn = c; }

      during person($id, $firstName, $lastName, $address, $age) {
        assert ui.context(id)
          .html('table#the-table tbody',
                '<tr>' + [id, firstName, lastName, address, age].map(cell).join('') + '</tr>',
                [id, firstName, lastName, address, age][this.orderColumn]);
      }
    }
  }
}

function spawnController() {
  actor named 'controller' {
    react {
      on message Syndicate.UI.globalEvent('table#the-table th', 'click', $e) {
        :: setSortColumn(JSON.parse(e.target.dataset.column));
      }
    }
  }
}

ground dataspace G {
  Syndicate.UI.spawnUIDriver();

  spawnModel();
  spawnView();
  spawnController();
}
