var TfIdf = require('natural').TfIdf;
var bf = require('bloomfilter');
var calibrater = require('./calibrater.js');
var fs = require('fs');
var deleter = require('./deleter.js');
var logger = require('../logger');


function reduceTF(tf) { 
  var reducedTF = {};
  for (var k in tf) {
    var thisID = String(k);
    var theseTokens = tf[k];
    for (var i = 0; i < theseTokens.length; i++) {
      var thisTFKey = theseTokens[i];
      if (!reducedTF[thisTFKey])
        reducedTF[thisTFKey] = [];
      //make this a sorted insert to allow faster intersection calculation
      reducedTF[thisTFKey].push(thisID);
    }
  }
  return reducedTF;
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
      //field is OK- add it to forage.composite
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



    //generate bloom filter
    var p = 0.1; //bloomErrorMargin
    var n = (Object.keys(docVector).length) //numberOfItemsInBloomFilter
    var bloomBits = Math.ceil((n * Math.log(p)) / Math.log(1.0 / (Math.pow(2.0, Math.log(2.0)))));
    var bloomHashFunctions = Math.round(Math.log(2.0) * bloomBits / n);
    var bloom = new bf.BloomFilter(bloomBits, bloomHashFunctions);
    //work out facet keys for bloom filter
    for (var i = 0; i < facets.length; i++) {
      //doc has some facet values
      if (doc[facets[i]]) {
        //loop though this facet field
        for (var j = 0; j < doc[facets[i]].length; j++) {
          for (var k in docVector) {
            if (k != '__key'){
              var bloomEntry = 'TF~' + k + '~' + facets[i] + '~'
                + doc[facets[i]][j] + '~' + fieldKey;
              tfValues.push(bloomEntry);
              bloom.add(bloomEntry);
            }
          }
        }
      }
    }
    //no facets
    for (var k in docVector) {
      if (k != '__key') {
        var bloomEntry = 'TF~' + k + '~~~' + fieldKey;
        tfValues.push(bloomEntry);
        bloom.add(bloomEntry);
      }
    }
    var bloomArray = [].slice.call(bloom.buckets)
    reverseIndexValue['bloom'] = JSON.stringify(bloomArray);

    //wildcard token
    docVector['*'] = 1;

    var docVecLength = Object.keys(docVector).length - 2;
    for (k in docVector) {
      if (k != '__key') {
        //no faceting
        facetIndexKey = ['~'];
        for (l = 0; l < facets.length; l++) {
          if (doc[facets[l]]) {
            thisFacetValue = doc[facets[l]];
            for (m = 0; m < thisFacetValue.length; m++) {
              facetIndexKey.push(facets[l] + '~' + thisFacetValue[m]);
            }
          } 
        }
        for (l = 0; l < facetIndexKey.length; l++) {
          //augmented term frequency
          var tf = (docVector[k] / highestFrequencyCount / docVecLength).toFixed(10);
          //since levelDB sorts fastest from low to high, it is best
          //to give significant keys (those with highest tf) a lower
          //score. An inverted TF is therefore used for sorting.
          var sortKey = (1 - tf).toFixed(10);
          //design key
          var tokenKey = 'REVERSEINDEX~'
            + k + '~'
            + facetIndexKey[l] + '~'
            + fieldKey + '~'
            + sortKey + '~'
//            + tf + '~'
            + id;
          //make a tf vector for the whole document
          //if (fieldKey == 'forage.composite')
          tfmap[k] = tf;
          fieldBatch.push({
            type: 'put',
            key: tokenKey,
            value: reverseIndexValue});
          deleteKeys.push(tokenKey);
         // logger.debug('tokenKey', tokenKey);
        }
      }
    }
    //dump references so that docs can be deleted
    docDeleteIndexKey = 'DELETE-DOCUMENT~' + id + '~' + fieldKey;
    deleteKeys.push(docDeleteIndexKey);
    fieldBatch.push({
      type: 'put',
      key: docDeleteIndexKey,
      value: deleteKeys});
    var vectorValue = {};
    vectorValue['vector'] = tfmap;
    vectorValue['facetValues'] = facetValues;
    fieldBatch.push({
      type: 'put',
      key: 'VECTOR~' + fieldKey + '~' + docID + '~',
      value: vectorValue});
    fieldedVector[fieldKey] = vectorValue;
  }
  //generate fielded document vector for weighting

  fieldBatch.push({
    type: 'put',
    key: 'VECTOR~*fielded~' + docID + '~',
    value: fieldedVector});
  //document
  fieldBatch.push({
    type: 'put',
    key: 'DOCUMENT~' + docID + '~',
    value: JSON.stringify(doc)});

  //put key-values into database
  reverseIndex.batch(fieldBatch, function (err) {
//    logger.debug(tfValues);
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
        calibrater.incrementallyCalibrate(indexesMultiply, reduceTF(tf), function(msg) {
          logger.info(msg);
          logger.success('indexed batch: ' + batchName);
          callback('[success] indexed batch: ' + batchName);
        });
      }
    };
    indexDocs(0);
  } catch(err) {
    callback('Error parsing batch:' + batchName);
  }  
};
