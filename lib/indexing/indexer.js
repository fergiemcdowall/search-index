var TfIdf = require('natural').TfIdf;
var fs = require('fs');
var bf = require('bloomfilter');

exports.deleteDoc = function(docID, callback) {
  var delBatch = [],
      meta = readMetaDataGlobal();
  //record deleted doc
  meta['totalDocs']--;
  reverseIndex.createReadStream({
    start: 'DELETE-DOCUMENT~' + docID + '~',
    end: 'DELETE-DOCUMENT~' + docID + '~~'})
    .on('data', function(data) {
      var deleteKeys = data.value,
          i;
      //record deleted field
      meta['totalIndexedFields']--;
      
      for (i = 0; i < deleteKeys.length; i++) {
        //record deleted reverse index entry
        meta['reverseIndexSize']--;
        delBatch.push({
          type: 'del',
          key: deleteKeys[i]});
      }
    })
    .on('end', function() {
      reverseIndex.batch(delBatch, function (err) {
        if (err) return console.log('Ooops!', err);
        return;
      });
      writeMetaDataGlobal(meta);
      callback('deleted ' + docID);
    });
};


function readMetaDataGlobal() {
  var obj;
  try {
    obj = JSON.parse(fs.readFileSync('search-index.json'));
  }
  catch (e) {
    obj = {};
    obj['totalIndexedFields'] = 0;
    obj['indexedFieldNames'] = [];
    obj['totalDocs'] = 0;
    obj['reverseIndexSize'] = 0;
    obj['availableFacets'] = [];
  }
//  console.log(obj);
  return obj;
}

function writeMetaDataGlobal(obj) {
  indexMetaDataGlobal = obj;
  fs.writeFileSync('search-index.json', JSON.stringify(obj));
  return;
}


//TODO: clean up confusion between filters and factets
function indexDoc(reverseIndex, docID, doc, facets, indexMetaDataGlobal, callback) {
  //use key if found, if no key is found set filename to be key.
  var fieldBatch = [],
      id = docID,
      value = {},
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
  value['filters'] = facety;

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
      console.log('[indexing warning] '.yellow + docID.yellow + ': '.yellow
                  + fieldKey.yellow + ' field is null, SKIPPING'.yellow)
    }
    //only index fields that are strings
    else if ((typeof doc[fieldKey]) != 'string') {
      delete doc[fieldKey];
      console.log('[indexing warning] '.yellow + docID.yellow + ': '.yellow
                  + fieldKey.yellow 
                  + ' field not string or array, SKIPPING'.yellow)
    }
    else {
      //field is OK- add it to forage.composite
      compositeField += doc[fieldKey] + ' ';
    }
  }

  doc['*'] = compositeField;

  var compositeVector = '';

  for (fieldKey in doc) {
    var tfmap = {};
    if (indexMetaDataGlobal['indexedFieldNames'].indexOf(fieldKey) == -1) {
      indexMetaDataGlobal['indexedFieldNames'].push(fieldKey);
    }
    tfidf = new TfIdf();
    tfidf.addDocument(doc[fieldKey], fieldKey + '~' + id);
    var docVector = tfidf.documents[tfidf.documents.length - 1];
//    if (fieldKey == 'forage.composite') compositeVector = docVector;
      
//    console.log(docVector);
    highestFrequencyCount = 0;
    for (k in docVector) {
      if (docVector[k] > highestFrequencyCount)
        highestFrequencyCount = docVector[k];
    }
    deleteKeys = [];

//TODO: bloom should probably only for forage.composite

    //generate bloom filter
    var p = 0.1; //bloomErrorMargin
    var n = (Object.keys(docVector).length) //numberOfItemsInBloomFilter
    var bloomBits = Math.ceil((n * Math.log(p)) / Math.log(1.0 / (Math.pow(2.0, Math.log(2.0)))));
    var bloomHashFunctions = Math.round(Math.log(2.0) * bloomBits / n);
    var bloom = new bf.BloomFilter(bloomBits, bloomHashFunctions);
//    console.log(p + ' ' + n + ' ' + bloomBits + ' ' + bloomHashFunctions);

//work out facet keys for bloom filter
    for (var i = 0; i < facets.length; i++) {
      //doc has some facet values
      if (doc[facets[i]]) {
        //loop though this facet field
        for (var j = 0; j < doc[facets[i]].length; j++) {
          for (var k in docVector) {
            if (k != '__key'){
              var bloomEntry = k + '~' + facets[i] + '~'
                + doc[facets[i]][j] + '~' + fieldKey;
              bloom.add(bloomEntry);
            }
          }
        }
      }
    }
/*
    for (var k in docVector) {
      if (k != '__key'){
        bloom.add(k + '~~~forage.composite');
      }
    }
*/

    //no facets
    for (var k in docVector) {
      if (k != '__key') {
        var bloomEntry = k + '~~~' + fieldKey;
        bloom.add(bloomEntry);
      }
    }




    var bloomArray = [].slice.call(bloom.buckets)
    value['bloom'] = JSON.stringify(bloomArray);

    //wildcard token
    docVector['*'] = 1;

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
          var tf = (docVector[k] / highestFrequencyCount).toFixed(10);
          //make a tf vector for the whole document
          //if (fieldKey == 'forage.composite')
            tfmap[k] = tf;
          tokenKey = 'REVERSEINDEX~'
            + k + '~'
            + facetIndexKey[l] + '~'
            + fieldKey + "~"
            + tf + '~'
            + id;
          indexMetaDataGlobal['reverseIndexSize']++;
          fieldBatch.push({
            type: 'put',
            key: tokenKey,
            value: value});
          deleteKeys.push(tokenKey);
//          console.log(tokenKey);
        }
      }
    }




    //dump references so that docs can be deleted
    docDeleteIndexKey = 'DELETE-DOCUMENT~' + id + '~' + fieldKey;
    deleteKeys.push(docDeleteIndexKey);
    //update metadata
    indexMetaDataGlobal['totalIndexedFields']++;
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
//    console.log(fieldKey);
    
  }


  fieldBatch.push({
    type: 'put',
    key: 'DOCUMENT~' + docID + '~',
    value: JSON.stringify(doc)});

//  if (fieldKey == 'forage.composite') {
//  }


  //put key-values into database
  reverseIndex.batch(fieldBatch, function (err) {
    callback('[indexed] ' + docID);
    if (err) return console.log('Ooops!', err);
    return;
  });
}



exports.index = function(reverseIndex, batchString, batchName, filters, callback) {
  var indexMetaDataGlobal,
      i,
      batch,
      docIDs,
      indexDocs;
  if (indexMetaDataGlobal === undefined) {
    indexMetaDataGlobal = readMetaDataGlobal();
  }
  for (i = 0; i < filters.length; i++) {
    if (indexMetaDataGlobal['availableFacets'].indexOf(filters[i]) == -1) {
      indexMetaDataGlobal['availableFacets'].push(filters[i]);
    }
  }
  batch = JSON.parse(batchString);
  //index documemts in sequence so as not to block server for reads
  docIDs = Object.keys(batch);
  indexDocs = function (i) {
    if (i < docIDs.length) {
      indexDoc(reverseIndex, docIDs[i], batch[docIDs[i]], filters, indexMetaDataGlobal, function(msg) {
        console.log(msg);
        indexMetaDataGlobal['totalDocs']++;
        indexDocs(++i);
      });
    }
    else {
      writeMetaDataGlobal(indexMetaDataGlobal);
      callback('indexed batch: ' + batchName + '\n');
    }
  };
  indexDocs(0);
};
