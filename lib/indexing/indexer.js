var TfIdf = require('natural').TfIdf;
var _ = require('lodash');
var async = require('async');
var calibrater = require('./calibrater.js');
var deleter = require('./deleter.js');
var fs = require('fs');
var hash = require('object-hash');
var winston = require('winston');
var indexingLogger = new (winston.Logger)({
  transports: [
    new (winston.transports.Console)({ level: 'info' }),
  ]
});

function reduceTF(tf) {

  return _(tf)
    .flatten(function (item, key) {
      return _.map(item, function (token) {
        return {
          token: token[0],
          KeyID: key,
          ReverseIndex: [token[1], key]
        }
      })
    })
    .groupBy('token')
    .reduce(function (memo, item, key) {
      memo.reducedTF['TF~' + key] = _.pluck(item, 'KeyID');
      memo.reducedTFSortOnTF['RI~' + key] = _.pluck(item, 'ReverseIndex');
      return memo;
    }, {
      reducedTF: {},
      reducedTFSortOnTF: {}
    });

}


//TODO: clean up confusion between filters and factets
function indexDoc(reverseIndex, docID, doc, facets, callback) {
  //use key if found, if no key is found set filename to be key.
  var fieldBatch = [],
    id = docID,
    facetValues = {},
    fieldKey,
    highestFrequencyCount,
    k,
    deleteKeys,
    facetIndexKey,
    l,
    thisFacetValue,
    m,
    tokenKey,
    docDeleteIndexKey;

  var facety = [];
  for (var i = 0; i < facets.length; i++) {
    //doc has some facet values
    if (doc[facets[i]]) {
      //loop though this facet field
      for (var j = 0; j < doc[facets[i]].length; j++) {
        facety.push(facets[i] + '~' + doc[facets[i]][j]);
      }
    }
  }

  var compositeField = '';
  for (fieldKey in doc) {
    if (Object.prototype.toString.call(doc[fieldKey]) === '[object Array]') {
      if (facets.indexOf(fieldKey) != -1) {
        facetValues[fieldKey] = doc[fieldKey];
      }
      //add the individual metadata tokens to the composite field,
      //such that the document is retrievable for each metadata token
      for (var i = 0; i < doc[fieldKey].length; i++)
        compositeField += ' ' + doc[fieldKey][i] + ' ';
    }
    //throw away fields that have null value
    else if (doc[fieldKey] == null) {
      delete doc[fieldKey];
      indexingLogger.debug(docID + ': ' + fieldKey + ' field is null, SKIPPING')
    }
    //only index fields that are strings or numbers
    else if (!(((typeof doc[fieldKey]) == 'string')
      || ((typeof doc[fieldKey]) == 'number'))) {
      //      if (fieldKey == 'randomNumber') console.log(typeof doc[fieldKey]);
      delete doc[fieldKey];
      indexingLogger.debug(docID + ': ' + fieldKey
      + ' field not string or array, SKIPPING')
    }
    else {
      //field is OK- add it to composite
      compositeField +=  ' ' + doc[fieldKey] + ' ';
    }
  }

  doc['*'] = compositeField;
  var fieldedVector = {};

  var tfValues = [];

  for (fieldKey in doc) {
    var reverseIndexValue = {};
    reverseIndexValue['filters'] = facety;
    var tfmap = {};
    tfidf = new TfIdf();
    tfidf.addDocument(doc[fieldKey] + '', fieldKey + '~' + id);
    var docVector = tfidf.documents[tfidf.documents.length - 1];
    
    //wildcard char
    docVector['*'] = 1;

    highestFrequencyCount = 0;
    for (k in docVector) {
      if (docVector[k] > highestFrequencyCount) highestFrequencyCount = docVector[k];
    }
    deleteKeys = [];

    var docVecLength = Object.keys(docVector).length - 2;


    //work out facet keys for bloom filter
    for (var i = 0; i < facets.length; i++) {
      //doc has some facet values
      if (doc[facets[i]]) {
        //loop though this facet field
        for (var j = 0; j < doc[facets[i]].length; j++) {
          for (var k in docVector) {
            if (k != '__key') {
              var bloomEntry = fieldKey + '~' + k + '~' + facets[i] + '~'
                + doc[facets[i]][j];
              var tfValue = (docVector[k] / highestFrequencyCount / docVecLength).toFixed(10);
              tfValues.push([bloomEntry, tfValue]);
              deleteKeys.push(bloomEntry);
            }
          }
        }
      }
    }
    //no facets
    for (var k in docVector) {
      if (k != '__key') {
        var bloomEntry = fieldKey + '~' + k + '~~';
        var tfValue = (docVector[k] / highestFrequencyCount / docVecLength).toFixed(10);
        tfValues.push([bloomEntry, tfValue]);
        deleteKeys.push(bloomEntry);
      }
    }

    //dump references so that docs can be deleted
    docDeleteIndexKey = 'DELETE-DOCUMENT~' + id + '~' + fieldKey;
    //    deleteKeys.push(docDeleteIndexKey);
    fieldBatch.push({
      type: 'put',
      key: docDeleteIndexKey,
      value: deleteKeys
    });
  }
  //generate fielded document vector for weighting

  //document
  fieldBatch.push({
    type: 'put',
    key: 'DOCUMENT~' + docID + '~',
    value: JSON.stringify(doc)
  });

  //put key-values into database
  reverseIndex.batch(fieldBatch, function (err) {
    if (err) {
      indexingLogger.error('Ooops!', err);
      return callback(err);
    }

    var msg = {};
    msg['status'] = 'Indexed doc ' + docID;
    msg['tfValues'] = tfValues;
    return callback(null, msg);
  });
}

exports.addDocToIndex = function (indexes, indexesMultiply, batch, batchName, filters, callback) {
  var tf = {};
  async.each(batch,
    function (doc, callback) {
      async.waterfall([
          //checkDoc
          function (callback) {
            if (Object.prototype.toString.call(doc) != '[object Object]') {
              indexingLogger.error('malformed document: check document formatting at batch index');
              return callback(new Error('Malformed document'));
            }
            callback(null);
          },
          //deleteDoc
          function (callback) {
            var docID = '';
            if (doc['id']) {
              docID = doc['id'];
              deleter.deleteDoc(docID, indexes, indexesMultiply, function (err, msgg) {
                callback(null, docID);
              });
            }
            else {
              docID = Date.now() + '-' + Math.random() * 10000000000000000 + '-' + hash(doc);
              callback(null, docID);
            }
          },
          //IndexDoc
          function (docID) {
            indexDoc(indexes, docID, doc, filters, function (err, msg) {
              indexingLogger.info(msg.status);
              tf[docID] = msg.tfValues;
              callback(null);
            });
          }

        ],
        callback);
    }, function (err) {
      if (err) {
        return callback(err);
      }
      indexingLogger.info('starting calibration');
      calibrater.incrementallyCalibrate(indexes, indexesMultiply, reduceTF(tf), function (err) {
        indexingLogger.info('indexed batch: ' + batchName);
        callback(null);
      });
    }
  );
};
