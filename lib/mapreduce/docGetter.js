//returns every artifact placed in index for a given document

exports.getDoc = function (reverseIndex, reverseIndexMultiply, docID, callback) {

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
    console.log('Oh my!', err)
  }).on('end', function () {
    keys.push('VECTOR~*fielded~' + docID + '~');
    keys.push('DOCUMENT~' + docID + '~');
    reverseIndexMultiply.get(keys, function(err, data) {
      for (k in data) {
        if (data[k]) res[k] = data[k];
      }
      callback(res);
    })
  })
}
