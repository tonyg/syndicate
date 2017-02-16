// bin/syndicatec compiler/demo-filesystem.js | node

// Good output:
//
// At least one reader exists for: hello.txt
// hello.txt has content undefined
// hello.txt has content "a"
// hello.txt has content undefined
// hello.txt has content "c"
// hello.txt has content "quit demo"
// The hello.txt file contained 'quit demo', so we will quit
// second observer sees that hello.txt content is "final contents"
// No remaining readers exist for: hello.txt

var Syndicate = require('./src/main.js');

assertion type file(name, content) = "file";
message type saveFile(name, content) = "save";
message type deleteFile(name) = "delete";

ground dataspace {
  ///////////////////////////////////////////////////////////////////////////
  // The file system actor

  spawn {
    this.files = {};
    during Syndicate.observe(file($name, _)) {
      on start {
        console.log("At least one reader exists for:", name);
      }
      assert file(name, field this.files[name]);
      on stop {
        console.log("No remaining readers exist for:", name);
      }
    }
    on message saveFile($name, $newcontent) {
      field this.files[name] = newcontent;
    }
    on message deleteFile($name) {
      delete field this.files[name];
    }
  }

  ///////////////////////////////////////////////////////////////////////////
  // A simple demo client of the file system

  spawn {
    on asserted file("hello.txt", $content) {
      console.log("hello.txt has content", JSON.stringify(content));
    }

    stop on asserted file("hello.txt", "quit demo") {
      console.log("The hello.txt file contained 'quit demo', so we will quit");
    }
  }

  spawn {
    stop on asserted Syndicate.observe(saveFile(_, _)) {
      :: saveFile("hello.txt", "a");
      :: deleteFile("hello.txt");
      :: saveFile("hello.txt", "c");
      :: saveFile("hello.txt", "quit demo");
      :: saveFile("hello.txt", "final contents");
      spawn {
        stop on asserted file("hello.txt", $content) {
          console.log("second observer sees that hello.txt content is",
                      JSON.stringify(content));
        }
      }
    }
  }
}
