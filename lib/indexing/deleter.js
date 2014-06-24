//deletes all references to a document from the search index

var docGetter = require('../mapreduce/docGetter.js');


//recalibrate index after deleting all artifacts (remove or decrement TF entries)
function incrementallyCalibrateForDeletion(indexesMultiply, keys, callback) {
  var deleteKeys = [];
  var decrementKeys = {};
  indexesMultiply.get(keys, function(err, data) {
    for (var k in data) {
      if (data[k] > 1)
        decrementKeys[k] = (parseInt(data[k]) - 1);
      else
        deleteKeys.push(k);
    }
    indexesMultiply.put(decrementKeys, function (err, data) {
      indexesMultiply.del(deleteKeys, function (err, value) { 
        callback('[success] index recalibrated')
      })
    })
  })
}


exports.deleteDoc = function(docID, indexes, indexesMultiply, callback) {
  //use docGetter to find out which artifacts must be removed
  docGetter.getDoc(indexes, indexesMultiply, docID, function(docArtifacts) {
    //remove all artifacts
    var deleteKeys = [];
    deleteKeys.push(Object.keys(docArtifacts));
    deleteKeys.push('VECTOR~*fielded~' + 747 + '~');
    deleteKeys.push('DOCUMENT~' + 747 + '~');
    for (var k in docArtifacts) {
      if (k.split('~')[0] == 'DELETE-DOCUMENT')
        deleteKeys = deleteKeys.concat(docArtifacts[k]);
    }
    var calibrationKeys = Object.keys(docArtifacts['VECTOR~*~' + docID + '~'].vector)
    var formattedCalibrationKeys = [];
    for (var i = 0; i < calibrationKeys.length; i++) {
      formattedCalibrationKeys.push('TF~' + calibrationKeys[i] + '~~~*');
    }
    indexes.del(deleteKeys, function (err, data) {
      incrementallyCalibrateForDeletion(indexesMultiply, formattedCalibrationKeys, function(msg) {
        callback(msg);
      })
    })
  });
}
