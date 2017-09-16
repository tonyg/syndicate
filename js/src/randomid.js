var randomId;

function browserCryptoObject(crypto) {
  if (typeof crypto.getRandomValues === 'undefined') return false;
  randomId = function (byteCount, hexOutput) {
    var buf = new Uint8Array(byteCount);
    crypto.getRandomValues(buf);
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
  return true;
}

if ((typeof window !== 'undefined') &&
    (typeof window.crypto !== 'undefined') &&
    browserCryptoObject(window.crypto)) {
  // We are in the main page, and window.crypto is available, and
  // browserCryptoObject has installed a suitable randomId. Do
  // nothing.
} else if ((typeof self !== 'undefined') &&
           (typeof self.crypto !== 'undefined') &&
           browserCryptoObject(self.crypto)) {
  // We are in a web worker, and self.crypto is available, and
  // browserCryptoObject has installed a suitable randomId. Do
  // nothing.
} else {
  // See if we're in node.js.

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
