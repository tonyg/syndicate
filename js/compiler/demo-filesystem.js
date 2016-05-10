// bin/syndicatec compiler/demo-filesystem.js | node

var Syndicate = require('./src/main.js');

assertion type file(name, content) = "file";
assertion type saveFile(name, content) = "save";
assertion type deleteFile(name) = "delete";

ground dataspace {
  ///////////////////////////////////////////////////////////////////////////
  // The file system actor

  actor {
    this.files = {};
    react {
      during Syndicate.observe(file($name, _)) {
        do {
          console.log("At least one reader exists for:", name);
        }
        assert file(name, this.files[name]);
        finally {
          console.log("No remaining readers exist for:", name);
        }
      }
      on message saveFile($name, $newcontent) {
        this.files[name] = newcontent;
      }
      on message deleteFile($name) {
        delete this.files[name];
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // A simple demo client of the file system

  actor {
    react {
      on asserted file("hello.txt", $content) {
        console.log("hello.txt has content", JSON.stringify(content));
      }
    } until {
      case asserted file("hello.txt", "quit demo") {
        console.log("The hello.txt file contained 'quit demo', so we will quit");
      }
    }

    react until {
      case asserted Syndicate.observe(saveFile(_, _)) {
        :: saveFile("hello.txt", "a");
        :: deleteFile("hello.txt");
        :: saveFile("hello.txt", "c");
        :: saveFile("hello.txt", "quit demo");
        :: saveFile("hello.txt", "final contents");
        actor {
          react until {
            case asserted file("hello.txt", $content) {
              console.log("second observer sees that hello.txt content is",
                          JSON.stringify(content));
            }
          }
        }
      }
    }
  }
}
