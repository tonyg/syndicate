/* Web Worker interface */
var Ground = require("./ground.js").Ground;
var Util = require("./util.js");
var Codec = require("./codec.js");

var Dataspace = require("./dataspace.js").Dataspace;

var BuiltinWorker = typeof window !== 'undefined' && window.Worker;

///////////////////////////////////////////////////////////////////////////

function Worker(scriptUrl) {
  this.scriptUrl = scriptUrl;
  this.w = new BuiltinWorker(scriptUrl);
}

Worker.prototype.boot = function () {
  this.w.onmessage = Dataspace.wrap(function (e) {
    console.log("Received from worker", JSON.stringify(e.data));
    Dataspace.enqueueAction(Codec.decodeAction(e.data));
  });
};

Worker.prototype.handleEvent = function (e) {
  console.log("Sending to worker", JSON.stringify(Codec.encodeEvent(e)));
  this.w.postMessage(Codec.encodeEvent(e));
};

///////////////////////////////////////////////////////////////////////////

function WorkerGround(bootFn) {
  var self = this;
  Ground.call(this, bootFn);
  onmessage = function (e) {
    console.log("Received from main page", JSON.stringify(e.data));
    self.dataspace.handleEvent(Codec.decodeEvent(e.data));
    self.startStepping();
  };
}

WorkerGround.prototype = Util.extend({}, Ground.prototype);

WorkerGround.prototype.enqueueAction = function (pid, action) {
  console.log("Sending to main page", JSON.stringify(Codec.encodeAction(action)));
  postMessage(Codec.encodeAction(action));
  console.log("Sent to main page");
};

///////////////////////////////////////////////////////////////////////////

module.exports.Worker = Worker;
module.exports.WorkerGround = WorkerGround;
