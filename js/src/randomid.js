var randomId;

if ((typeof window !== 'undefined') &&
    (typeof window.crypto !== 'undefined') &&
    (typeof window.crypto.getRandomValues !== 'undefined')) {
  randomId = function (byteCount) {
    var buf = new Uint8Array(byteCount);
    window.crypto.getRandomValues(buf);
    return btoa(String.fromCharCode.apply(null, buf)).replace(/=/g,'');
  };
} else if ((typeof crypto !== 'undefined') &&
	   (typeof crypto.randomBytes !== 'undefined')) {
  randomId = function (byteCount) {
    return crypto.randomBytes(byteCount).base64Slice().replace(/=/g,'');
  };
} else {
  console.warn('No suitable implementation for RandomID.randomId available.');
}

module.exports.randomId = randomId;
