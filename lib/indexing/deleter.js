//deletes all references to a document from the search index

var docGetter = require('../mapreduce/docGetter.js');
var indexPeek = require('./indexPeek.js');

function decrementRIArray(docID, indexesMultiply, indexes, keys, callback) {
  var decrementKeys = [];
  indexesMultiply.get(keys, function(err, data) {
    for (var k in data) {
      var batchOperation = {};
      if (data[k] == null) {/*ghost key- do nothing*/}
      for (var j = 0; j < data[k].length; j++) {
        if (data[k][j][1] == docID) {
          if (data[k].length > 1) {
            data[k].splice(j, 1);
            batchOperation['type'] = 'put';
            batchOperation['key'] = k;
            batchOperation['value'] = data[k];
          }
          else {
            batchOperation['type'] = 'del';
            batchOperation['key'] = k;
          }
        }
      }
      decrementKeys.push(batchOperation);
    }
    indexes.batch(decrementKeys, function (err) {
      callback('[success] index recalibrated');
    })
  })
}

//recalibrate index after deleting all artifacts (remove or decrement TF entries)
function decrementTFArray(docID, indexesMultiply, indexes, keys, callback) {
  function removeA(arr) {
    var what, a = arguments, L = a.length, ax;
    while (L > 1 && arr.length) {
      what = a[--L];
      while ((ax= arr.indexOf(what)) !== -1) {
        arr.splice(ax, 1);
      }
    }
    return arr;
  }
  
  indexesMultiply.get(keys, function(err, data) {
    var decrementKeys = [];
    for (var k in data) {
      var batchOperation = {};
      if (data[k] == null) {/*ghost key- do nothing*/}
      else if (data[k].length > 1) {
        batchOperation['type'] = 'put';
        batchOperation['key'] = k;
        //Making sure the docID is handled as a string
        batchOperation['value'] = removeA(data[k], docID + '');
      }
      else {
        batchOperation['type'] = 'del';
        batchOperation['key'] = k;
      }
      decrementKeys.push(batchOperation);
    }
    indexes.batch(decrementKeys, function (err) {
      if (err) searchIndexLogger.error(err)
      else callback('index recalibrated');
    })
  })
}


function deleteArtifacts(docID, indexes, indexesMultiply, callback) {
  //use docGetter to find out which artifacts must be removed
  docGetter.getDoc(indexes, indexesMultiply, docID, function(err, docArtifacts) {
    //remove all artifacts
    var deleteKeys = Object.keys(docArtifacts);
    var deleteIDsFromTheseArrayKeys = [];
    for (var k in docArtifacts) {
      deleteIDsFromTheseArrayKeys = deleteIDsFromTheseArrayKeys.concat(docArtifacts[k]);
    }
    var deleteIDsFromTheseTFArrayKeys = [];
    for (var i = 0; i < deleteIDsFromTheseArrayKeys.length; i++)
      deleteIDsFromTheseTFArrayKeys[i] = 'TF~' + deleteIDsFromTheseArrayKeys[i];
    var deleteIDsFromTheseRIArrayKeys = [];
    for (var i = 0; i < deleteIDsFromTheseArrayKeys.length; i++)
      deleteIDsFromTheseRIArrayKeys[i] = 'RI~' + deleteIDsFromTheseArrayKeys[i];
    decrementTFArray(docID, indexesMultiply, indexes, deleteIDsFromTheseTFArrayKeys, function(err){
      decrementRIArray(docID, indexesMultiply, indexes, deleteIDsFromTheseRIArrayKeys, function(err){
        indexesMultiply.del(deleteKeys, function (err, data) {
          // database no longer has 'foo', 'boom' or 'whoa' ('wha' was never there)
          callback(false);
        })
      });
    });
  });
}

exports.doesDocExist = function(docID, indexes, callback) {
  var testKey = 'DOCUMENT~' + docID + '~';
  indexPeek.indexValue(testKey, indexes, function(msg){
    if (msg == '[warning] key not found') return callback(false);
    return callback(true);
  });
}

exports.deleteDoc = function(docID, indexes, indexesMultiply, callback) {
  this.doesDocExist(docID, indexes, function(docExists){
    if (docExists) {
      deleteArtifacts(docID, indexes, indexesMultiply, function(msg) {
        return callback(true);
      });
    }
    else {
      return callback({'notFound': docID + ' cannot be deleted because it is not in index'});
    }
  });
}
