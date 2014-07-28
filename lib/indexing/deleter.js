//deletes all references to a document from the search index

var docGetter = require('../mapreduce/docGetter.js');
var indexPeek = require('./indexPeek.js');

//recalibrate index after deleting all artifacts (remove or decrement TF entries)
function incrementallyCalibrateForDeletion(docID, indexesMultiply, keys, callback) {
  var deleteKeys = [];
  var decrementKeys = {};
  indexesMultiply.get(keys, function(err, data) {
    for (var k in data) {
      if (data[k] == null) {/*ghost key- do nothing*/}
      else if (data[k].length > 1) {
        data[k].splice([docID], 1);
        decrementKeys[k] = data[k];
      }
      else {deleteKeys.push(k)};
    }
    indexesMultiply.put(decrementKeys, function (err, data) {
      indexesMultiply.del(deleteKeys, function (err, value) { 
        callback('[success] index recalibrated')
      })
    })
  })
}

function deleteArtifacts(docID, indexes, indexesMultiply, callback) {
  //use docGetter to find out which artifacts must be removed
  docGetter.getDoc(indexes, indexesMultiply, docID, function(docArtifacts) {
//    console.log(docArtifacts);
    //remove all artifacts
    var deleteKeys = [];
    deleteKeys.push(Object.keys(docArtifacts));
    deleteKeys.push('VECTOR~*fielded~' + docID + '~');
    deleteKeys.push('DOCUMENT~' + docID + '~');
    for (var k in docArtifacts) {
      if (k.split('~')[0] == 'DELETE-DOCUMENT')
        deleteKeys = deleteKeys.concat(docArtifacts[k]);
    }
    var calibrationKeys = [];
    //this is wrong- it is only deleting the * field. Put in loop that deletes all fields
    if (docArtifacts['VECTOR~*~' + docID + '~'])
      calibrationKeys = Object.keys(docArtifacts['VECTOR~*~' + docID + '~'].vector)
    var formattedCalibrationKeys = [];
    for (var i = 0; i < calibrationKeys.length; i++) {
      formattedCalibrationKeys.push('TF~*~' + calibrationKeys[i] + '~~');
    }
    indexes.del(deleteKeys, function (err, data) {
      incrementallyCalibrateForDeletion(docID, indexesMultiply, formattedCalibrationKeys, function(msg) {
        callback(msg);
      })
    })
  });  
}

exports.doesDocExist = function(docID, indexes, callback) {
  var testKey = 'DOCUMENT~' + docID + '~';
  indexPeek.indexValue(testKey, indexes, function(msg){
//    console.log(msg);
    if (msg == '[warning] key not found') return callback(false);
    return callback(true);
  });
}

exports.deleteDoc = function(docID, indexes, indexesMultiply, callback) {
  this.doesDocExist(docID, indexes, function(docExists){
    if (docExists) {
//      console.log('doc exists, deleting');
      deleteArtifacts(docID, indexes, indexesMultiply, function(msg) {
        return callback('[information] deleting ' + docID);
      });
    }
    else {
//      console.log('doc not here so not deleting');
      return callback('[information] ' + docID 
                      + ' cannot be deleted because it is not in index');
    }
  });
}
