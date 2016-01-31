var Reflect = require("./reflect.js");

module.exports.extend = function (what, _with) {
  for (var prop in _with) {
    if (_with.hasOwnProperty(prop)) {
      what[prop] = _with[prop];
    }
  }
  return what;
};

module.exports.kwApply = function (f, thisArg, args) {
  var formals = Reflect.formalParameters(f);
  var actuals = []
  for (var i = 0; i < formals.length; i++) {
    var formal = formals[i];
    if (!(formal in args)) {
      throw new Error("Function parameter '"+formal+"' not present in args");
    }
    actuals.push(args[formal]);
  }
  return f.apply(thisArg, actuals);
};
