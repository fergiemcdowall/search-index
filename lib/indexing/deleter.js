//deletes all references to a document from the search index

var docGetter = require('../mapreduce/docGetter.js');

exports.deleteDoc = function(docID, reverseIndex, reverseIndexMultiply, callback) {

  //use docGetter to find out which artifacts must be removed
  docGetter.getDoc(reverseIndex, reverseIndexMultiply, docID, function(docArtifacts) {
    var deleteKeys = [];
    for (k in docArtifacts) {
      deleteKeys.push(k);
      if (k.split('~')[0] == 'DELETE-DOCUMENT')
        deleteKeys = deleteKeys.concat(docArtifacts[k]);
    }
    reverseIndex.del(deleteKeys, function (err, data) {
      callback('done');
    })
  });

}
