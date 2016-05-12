var randomId;

if ((typeof window !== 'undefined') &&
    (typeof window.crypto !== 'undefined') &&
    (typeof window.crypto.getRandomValues !== 'undefined')) {
  randomId = function (byteCount, hexOutput) {
    var buf = new Uint8Array(byteCount);
    window.crypto.getRandomValues(buf);
    if (hexOutput) {
      var encoded = [];
      for (var i = 0; i < buf.length; i++) {
	encoded.push("0123456789abcdef"[(buf[i] >> 4) & 15]);
	encoded.push("0123456789abcdef"[buf[i] & 15]);
      }
      return encoded.join('');
    } else {
      return btoa(String.fromCharCode.apply(null, buf)).replace(/=/g,'');
    }
  };
} else {
  var crypto;
  try {
    crypto = require('crypto');
  } catch (e) {}
  if ((typeof crypto !== 'undefined') &&
      (typeof crypto.randomBytes !== 'undefined')) {
    randomId = function (byteCount, hexOutput) {
      if (hexOutput) {
        return crypto.randomBytes(byteCount).hexSlice().replace(/=/g,'');
      } else {
        return crypto.randomBytes(byteCount).base64Slice().replace(/=/g,'');
      }
    };
  } else {
    console.warn('No suitable implementation for RandomID.randomId available.');
  }
}

module.exports.randomId = randomId;
