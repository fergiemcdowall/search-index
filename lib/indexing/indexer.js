var TfIdf = require('natural').TfIdf;
var bf = require('bloomfilter');
var calibrater = require('./calibrater.js');
var fs = require('fs');
var deleter = require('./deleter.js');
var logger = require('../logger');

function reduceTF(tf) {
  var TFSets = {};
  TFSets['reducedTF'] = {};
  TFSets['reducedTFSortOnTF'] = {};
  for (var k in tf) {
    var thisID = String(k);
    var theseTokens = tf[k];
    for (var i = 0; i < theseTokens.length; i++) {
      var thisTFKeyIDSort = 'TF~' + theseTokens[i][0];
      var thisTFKeyReverseIndex = 'RI~' + theseTokens[i][0];
      if (!TFSets['reducedTF'][thisTFKeyIDSort]) {
        TFSets['reducedTF'][thisTFKeyIDSort] = [];
        TFSets['reducedTFSortOnTF'][thisTFKeyReverseIndex] = [];
      }
      //make this a sorted insert to allow faster intersection calculation
      TFSets['reducedTF'][thisTFKeyIDSort].push(thisID);
      TFSets['reducedTFSortOnTF'][thisTFKeyReverseIndex].push([theseTokens[i][1], thisID]);
    }
  }
  return TFSets;
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
    if( Object.prototype.toString.call(doc[fieldKey]) === '[object Array]' ) {
      if (facets.indexOf(fieldKey) != -1) {
        facetValues[fieldKey] = doc[fieldKey];
      }
    }
    //throw away fields that have null value
    else if (doc[fieldKey] == null) {
      delete doc[fieldKey];
      logger.warn(docID + ': ' + fieldKey + ' field is null, SKIPPING')
    }
    //only index fields that are strings
    else if ((typeof doc[fieldKey]) != 'string') {
      delete doc[fieldKey];
      logger.warn(docID + ': ' + fieldKey
                  + ' field not string or array, SKIPPING')
    }
    else {
      //field is OK- add it to composite
      compositeField += doc[fieldKey] + ' ';
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
    tfidf.addDocument(doc[fieldKey], fieldKey + '~' + id);
    var docVector = tfidf.documents[tfidf.documents.length - 1];
    highestFrequencyCount = 0;
    for (k in docVector) {
      if (docVector[k] > highestFrequencyCount) highestFrequencyCount = docVector[k];
//      if (fieldKey == '*') tfValues.push(k);
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
            if (k != '__key'){
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
      value: deleteKeys});
  }
  //generate fielded document vector for weighting

  //document
  fieldBatch.push({
    type: 'put',
    key: 'DOCUMENT~' + docID + '~',
    value: JSON.stringify(doc)});

  //put key-values into database
  reverseIndex.batch(fieldBatch, function (err) {
//    console.log(tfValues);
    var msg = {};
    msg['status'] = 'Indexed doc ' + docID;
    msg['tfValues'] = tfValues;
    callback(msg);
    if (err) return logger.error('Ooops!', err);
    return;
  });
}

exports.addDocToIndex = function(indexes, indexesMultiply, batch, batchName, filters, callback) {
  var i,
      docIDs,
      indexDocs;
  try {
    //index documemts in sequence so as not to block server for reads
    docIDs = Object.keys(batch);
    //this batchs tf values
    tf = {};
    indexDocs = function (i) {
      if (i < docIDs.length) {
        deleter.deleteDoc(docIDs[i], indexes, indexesMultiply, function(msgg) {
          indexDoc(indexes, docIDs[i], batch[docIDs[i]], filters, function(msg) {
            logger.info(msg.status);
            tf[docIDs[i]] = msg.tfValues;
            indexDocs(++i);
          });
        });
      }
      else {
        logger.info('starting calibration');
        calibrater.incrementallyCalibrate(indexesMultiply, reduceTF(tf), function(msg) {
          logger.info(msg);
          logger.success('indexed batch: ' + batchName);
          callback(false);
        });
      }
    };
    indexDocs(0);
  } catch(err) {
    callback('Error parsing batch:' + batchName);
  }  
};
