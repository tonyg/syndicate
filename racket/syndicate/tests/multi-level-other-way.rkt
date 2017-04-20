#lang syndicate/test

(dataspace (spawn (on (asserted (inbound "gday"))
                      (send! (outbound "good things")))))

(spawn (assert "gday"))

(trace (message "good things"))