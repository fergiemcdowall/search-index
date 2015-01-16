var searchIndexLogger = require('../logger/searchIndexLogger');

exports.getDoc = function (reverseIndex, docID, callback) {
  reverseIndex.get('DOCUMENT~' + docID + '~', function (err, value) {
    if (err) {
      return callback(err, {});
    }
    else {
      return callback(false, value);
    }
  });
}


//returns every artifact placed in index for a given document
exports.getDocArtifacts = function (reverseIndex, reverseIndexMultiply, docID, callback) {

  //'DELETE-DOCUMENT~' + id + '~' + fieldKey
  //'VECTOR~' + fieldKey + '~' + docID + '~'
  //'VECTOR~*fielded~' + docID + '~',
  //'DOCUMENT~' + docID + '~',

  var res = {};
  var docFields = [];
  var keys = [];

  reverseIndex.createReadStream({
    valueEncoding: 'json',
    start: 'DELETE-DOCUMENT~' + docID + '~',
    end: 'DELETE-DOCUMENT~' + docID + '~~'
  }).on('data', function (data) {
    res[data.key] = data.value;
    var thisField = data.key.split('~')[2];
    docFields.push(thisField);
    keys.push('VECTOR~' + thisField + '~' + docID + '~');
  }).on('error', function (err) {
    callback(err, false);
  }).on('end', function () {
    reverseIndexMultiply.get(keys, function(err, data) {
      for (k in data) {
        if (data[k]) res[k] = data[k];
      }
      callback(null, res);
    })
  })
}
