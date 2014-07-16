var TfIdf = require('natural').TfIdf;
var bf = require('bloomfilter');
var calibrater = require('./calibrater.js');
var fs = require('fs');
var deleter = require('./deleter.js');
var logger = require('../logger');

function reduceTF(tf) { 
  var scores = Array.prototype.slice.call(tf)
    .reduce(function(last, now) {
      var index = last[0].indexOf(now);
      if (index === -1) {
        last[0].push(now);
        last[1].push(1);
      } else {
        last[1][index] += 1;
      }
      return last;
    }, [[], []])
    .reduce(function(last, now, index, context) {
      var zip = {};
      last.forEach(function(word, i) {
        zip['TF~' + word + '~~~*'] = context[1][i];
//        zip.push([word + '~~~*', context[1][i]])
      });
      return zip;
    });
  return scores;
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
      logger.log('[indexing warning] '.yellow + docID.yellow + ': '.yellow
                  + fieldKey.yellow + ' field is null, SKIPPING'.yellow)
    }
    //only index fields that are strings
    else if ((typeof doc[fieldKey]) != 'string') {
      delete doc[fieldKey];
      logger.log('[indexing warning] '.yellow + docID.yellow + ': '.yellow
                  + fieldKey.yellow 
                  + ' field not string or array, SKIPPING'.yellow)
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
      if (fieldKey == '*') tfValues.push(k);
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
         // logger.log('tokenKey', tokenKey);
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
//    logger.log(tfValues);
    var msg = {};
    msg['status'] = '[indexed] ' + docID;
    msg['tfValues'] = tfValues;
    callback(msg);
    if (err) return logger.log('Ooops!', err);
    return;
  });
}

exports.addDocToIndex = function(indexes, indexesMultiply, batchThing, batchName, filters, callback) {
  var i,
      batch,
      docIDs,
      indexDocs;
  try {
    if (typeof batchThing === 'object') {
      batch = batchThing;
    }
    else if (typeof batchThing === 'string') {
      batch = JSON.parse(batchThing);
    }
    //index documemts in sequence so as not to block server for reads
    docIDs = Object.keys(batch);
    //this batchs tf values
    tf = [];
    indexDocs = function (i) {
      if (i < docIDs.length) {
        deleter.deleteDoc(docIDs[i], indexes, indexesMultiply, function(msgg) {
          indexDoc(indexes, docIDs[i], batch[docIDs[i]], filters, function(msg) {
            logger.log(msg.status);
            tf = tf.concat(msg.tfValues);
            indexDocs(++i);
          });
        });
      }
      else {
        calibrater.incrementallyCalibrate(indexesMultiply, reduceTF(tf), function(msg) {
          logger.log(msg);
          logger.log('[success] indexed batch: ' + batchName + '\n');
          callback('[success] indexed batch: ' + batchName + '\n');
        });
      }
    };
    indexDocs(0);
  } catch(err) {
    callback('Error parsing batch:' + batchName + '\n');
  }  
};
