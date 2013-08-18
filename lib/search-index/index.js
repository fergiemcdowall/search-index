var fs = require('fs'),
    levelup = require('levelup'),
    natural = require('natural'),
    path = require('path'),
    
    //Arbitrary defaults
    maxStringFieldLength = 300,  //this has a really big effect on performance for some reason...
    maxMetadataFieldLength = 20,

    //only use this global for fast reads when searching- NEVER WRITE TO IT
    indexMetaDataGlobal = readMetaDataGlobal(),
    indexOptions = {},
    reverseIndex = null,
    stopwords = require('natural').stopwords,
    TfIdf = require('natural').TfIdf,
    home = '',
    levelupIndexPath= 'index',
    indexJsonFile = 'norchindex.json';

exports.init = function(options) {
  home = options.home;
  var exists = fs.existsSync(home);
  if (!exists) {
      fs.mkdirSync(home);
  }

  levelupIndexPath = path.join(home, levelupIndexPath);
  indexJsonFile = path.join(home, indexJsonFile);
  reverseIndex = levelup(levelupIndexPath, {valueEncoding: 'json'});
};

exports.indexData = function(callback) {
  callback(readMetaDataGlobal());
};

exports.indexPeek = function(start, stop, callback) {
  var dump = '';
  reverseIndex.createReadStream({
    start:start + "~",
    end:stop + "~~"})
    .on('data', function(data) {
      dump += data.key + '<br>' + data.value + '<br><br>';
    })
    .on('end', function() {
      callback(dump);
    });
};


exports.calibrate = function(callback) {
  countDocuments({}, function(indexDocData) {
    countReverseIndex(indexDocData, function(newIndexMetaData) {
      indexMetaDataGlobal = newIndexMetaData;
      callback(newIndexMetaData);
    });
  });
};


countDocuments = function(indexMetaData, callback) {
  var indexedFieldNamesX = [],
    totalIndexedDocsTmp = [];

  indexMetaData['totalIndexedFields'] = 0;
  indexMetaData['indexedFieldNames'] = [];
  indexMetaData['totalDocs'] = 0;
  //throwaway arrays


  console.log('calibrating...');
  reverseIndex.createReadStream({
    start: 'DOCUMENT~',
    end: 'DOCUMENT~~'})
    .on('data', function(data) {
      console.log(data.key);
      indexMetaData['totalIndexedFields']++;
      totalIndexedDocsTmp.push(data.key.split('~')[1]);
      indexedFieldNamesX[data.key.split('~')[2]] = data.key.split('~')[2];
    })
    .on('end', function() {
      for(var o in indexedFieldNamesX) {
        if (indexMetaData['indexedFieldNames'].indexOf(indexedFieldNamesX[o]) == -1) {
          indexMetaData['indexedFieldNames'].push(indexedFieldNamesX[o]);
        }
      }
      indexMetaData['totalDocs'] = arrNoDupe(totalIndexedDocsTmp).length;
      callback(indexMetaData);
    });  
};

//take an array and returns a similar array with no duplicates
function arrNoDupe(a) {
    var temp = {},
      i,
      r = [],
      k;
    for (i = 0; i < a.length; i++) {
        temp[a[i]] = true;
    }
    for (k in temp) {
        r.push(k);
    }
    return r;
}

countReverseIndex = function(indexMetaData, callback) {
  var availableFacets = [];
  indexMetaData['reverseIndexSize'] = 0;
  indexMetaData['availableFacets'] = [];
  reverseIndex.createReadStream({
    start: 'REVERSEINDEX~',
    end: 'REVERSEINDEX~~'})
    .on('data', function(data) {
      console.log(data.key);
      indexMetaData['reverseIndexSize']++;
      if (data.key.split('~')[2] != 'NO') {
        availableFacets[data.key.split('~')[2]] = data.key.split('~')[2];
      }
    })
    .on('close', function() {
      for(var o in availableFacets) {
        if (indexMetaData['availableFacets'].indexOf(availableFacets[o]) == -1) {
          indexMetaData['availableFacets'].push(availableFacets[o]);
        }
      }
      callback(indexMetaData);
    }); 
};

exports.deleteDoc = function(docID, callback) {
  var delBatch = [],
      meta = readMetaDataGlobal();
  //record deleted doc
  meta['totalDocs']--;
  reverseIndex.createReadStream({
    start: 'DOCUMENT~' + docID + '~',
    end: 'DOCUMENT~' + docID + '~~'})
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
    obj = JSON.parse(fs.readFileSync(indexJsonFile));
  }
  catch (e) {
    obj = {};
    obj['totalIndexedFields'] = 0;
    obj['indexedFieldNames'] = [];
    obj['totalDocs'] = 0;
    obj['reverseIndexSize'] = 0;
    obj['availableFacets'] = [];
  }
  console.log(obj);
  return obj;
}

function writeMetaDataGlobal(obj) {
  indexMetaDataGlobal = obj;
  fs.writeFileSync(indexJsonFile, JSON.stringify(obj));
  return;
}



exports.index = function(batchString, batchName, filters, callback) {
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
      indexDoc(docIDs[i], batch[docIDs[i]], filters, indexMetaDataGlobal, function(msg) {
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



//TODO: clean up confusion between filters and factets
function indexDoc(docID, doc, facets, indexMetaDataGlobal, callback) {
  //use key if found, if no key is found set filename to be key.
  var fieldBatch = [],
      id = docID,
      value = {},
      facetValues,
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
  value['fields'] = {};

  facetValues = {};
  if (doc[facets[0]]) {
    facetValues = doc[facets[0]];
  }

  for (fieldKey in doc) {
    if( Object.prototype.toString.call(doc[fieldKey]) === '[object Array]' ) {
      value['fields'][fieldKey] = doc[fieldKey];
    } else {
      value['fields'][fieldKey] = doc[fieldKey].toString().substring(0, maxStringFieldLength);
    }
  }


  for (fieldKey in doc) {
    if (indexMetaDataGlobal['indexedFieldNames'].indexOf(fieldKey) == -1) {
      indexMetaDataGlobal['indexedFieldNames'].push(fieldKey);
    }
    tfidf = new TfIdf();
    tfidf.addDocument(doc[fieldKey], fieldKey + '~' + id);
    docVector = tfidf.documents[tfidf.documents.length - 1];
    highestFrequencyCount = 0;
    for (k in docVector) {
      if (docVector[k] > highestFrequencyCount)
        highestFrequencyCount = docVector[k];
    }
    deleteKeys = [];

    //wildcard token
    docVector['*'] = 1;

    for (k in docVector) {
      if (k != '__key') {
        facetIndexKey = ['NO~FACETING'];
        for (l = 0; l < facets.length; l++) {
          if (doc[facets[l]]) {
            thisFacetValue = doc[facets[l]];
            for (m = 0; m < thisFacetValue.length; m++) {
              facetIndexKey.push(facets[l] + '~' + thisFacetValue[m]);
            }
          } 
        }
        for (l = 0; l < facetIndexKey.length; l++) {
          tokenKey = 'REVERSEINDEX~'
            + k + '~'
            + facetIndexKey[l] + '~'
            + fieldKey + "~"
            + docVector[k] + '~'
            + highestFrequencyCount + '~'
            + (docVector[k] / highestFrequencyCount) + '~'
            + id;
          indexMetaDataGlobal['reverseIndexSize']++;
          fieldBatch.push({
            type: 'put',
            key: tokenKey,
            value: value});
          deleteKeys.push(tokenKey);
        }
      }
    }

    //dump references so that docs can be deleted
    docDeleteIndexKey = 'DOCUMENT~' + id + '~' + fieldKey;
    deleteKeys.push(docDeleteIndexKey);
    //update metadata
    indexMetaDataGlobal['totalIndexedFields']++;
    fieldBatch.push({
      type: 'put',
      key: docDeleteIndexKey,
      value: deleteKeys});
  }


  //put key-values into database
  reverseIndex.batch(fieldBatch, function (err) {
    callback('indexed ' + docID);
    if (err) return console.log('Ooops!', err);
    return;
  });
}



//rewrite so that exports.search returns a value instead of proviking a res.send()
exports.search = function (q, callback) {

  //this must be set to true for a query to be carried out
  var canSearch = true,
  //tq = Transformed Query
      tq = Object.create(q),
      k,
      indexKeys,
      i,
      j,
      filterArray,
      startKey,
      stopKey;

  //remove stopwords
  tq['query'] = [];
  for (k = 0; k < q['query'].length; k++) {
    if (stopwords.indexOf(q['query'][k]) == -1) {
      tq['query'].push(q['query'][k]);
    }
  }
  if (tq['query'].length === 0) {
    canSearch = false;
  }

  //terms to look up in the reverse index
  indexKeys = [];
  if (q['filter']) {
    //make a filter set for every term in the query
    for (i = 0; i < tq['query'].length; i++) {
      //for every filter type
      for (j in q['filter']) {
        //TAG FILTER
        if (Array.isArray(q['filter'][j])) {
          //for every filter value in the array
          filterArray = q['filter'][j];
          for (k = 0; k < filterArray.length; k++) {
            if (q['searchFields']) {
              for (j = 0; j < q['searchFields'].length; j++) {
                startKey = 'REVERSEINDEX~'
                  + tq['query'][i] + '~'
                  + j + '~'
                  + filterArray[k] + '~'
                  + q['searchFields'][j] + '~';
                stopKey = 'REVERSEINDEX~'
                  + tq['query'][i] + '~'
                  + j + '~'
                  + filterArray[k] + '~'
                  + q['searchFields'][j] + '~~';
                indexKeys.push({'startKey': startKey,
                                'stopKey': stopKey});
              }
            }
            else {
              startKey = 'REVERSEINDEX~'
                + tq['query'][i] + '~'
                + j + '~'
                + filterArray[k] + '~';
              stopKey = 'REVERSEINDEX~'
                + tq['query'][i] + '~'
                + j + '~'
                + filterArray[k] + '~~';
              indexKeys.push({'startKey': startKey,
                              'stopKey': stopKey});
            }
          }
        }
        //RANGE FILTER
        else if (q['filter'][j].start) {
          if (q['searchFields']) {
            for (j = 0; j < q['searchFields'].length; j++) {
              startKey = 'REVERSEINDEX~' 
                + tq['query'][i] + '~'
                + j + '~'
                + q['filter'][j].start + '~'
                + q['searchFields'][j] + '~';
              stopKey = 'REVERSEINDEX~'
                + tq['query'][i] + '~'
                + j + '~'
                + q['filter'][j].stop + '~'
                + q['searchFields'][j] + '~~';
              indexKeys.push({'startKey': startKey,
                              'stopKey': stopKey});
            }
          }
          else {
            startKey = 'REVERSEINDEX~' 
              + tq['query'][i] + '~'
              + j + '~'
              + q['filter'][j].start + '~';
            stopKey = 'REVERSEINDEX~'
              + tq['query'][i] + '~'
              + j + '~'
              + q['filter'][j].stop + '~~';
            indexKeys.push({'startKey': startKey,
                            'stopKey': stopKey});
          }
        }
      }
    }
  }
  else {
    for (i = 0; i < tq['query'].length; i++) {
      if (q['searchFields']) {
        for (j = 0; j < q['searchFields'].length; j++) {
          startKey = 'REVERSEINDEX~'
            + tq['query'][i] + '~NO~FACETING~'
            + q['searchFields'][j] + '~';
          stopKey = 'REVERSEINDEX~'
            + tq['query'][i] + '~NO~FACETING~'
            + q['searchFields'][j] + '~~';
          indexKeys.push({'startKey': startKey, 'stopKey': stopKey});
        }
      }
      else {
        startKey = 'REVERSEINDEX~'
          + tq['query'][i] + '~NO~FACETING~';
        stopKey = 'REVERSEINDEX~'
          + tq['query'][i] + '~NO~FACETING~~';
        indexKeys.push({'startKey': startKey, 'stopKey': stopKey});
      }
    }
  }
  if (canSearch) {
    getSearchResults(q, tq, 0, {}, {}, indexKeys, function(msg) {
      callback(msg);
    });
  }
  else callback('no results');
};


function getSearchResults (q, tq, i, docSet, idf, indexKeys, callback) {
  var queryTerms = tq['query'],
      totalIndexedFields = indexMetaDataGlobal['totalIndexedFields'],
      availableFacets = indexMetaDataGlobal['availableFacets'],
      thisQueryTerm = indexKeys[i].startKey.split('~')[1],
      offset = ~~parseInt(q['offset']),
      pageSize = ~~parseInt(q['pageSize']),
      weight = {},
      idfCount;
  if (q['weight']) {
    weight = q['weight'];
  }
  idfCount = 0;
  reverseIndex.createReadStream({
    valueEncoding: 'json',
    start: indexKeys[i].startKey,
    end: indexKeys[i].stopKey})
    .on('data', function (data) {
      idfCount++;
      var splitKey = data.key.split('~'),
          //console.log(splitKey);
          docID = splitKey[8],
          fieldName = splitKey[4],
          tf = splitKey[7];
      //assign tf-idf per field and collate fields per doc
      if (docSet[docID] == null) {
        docSet[docID] = {};
        docSet[docID]['matchedTerms'] = {};
        docSet[docID]['matchedTerms'][thisQueryTerm] = {};
        docSet[docID]['matchedTerms'][thisQueryTerm][fieldName] = tf;
        docSet[docID]['document'] = data.value.fields;
      } else if (docSet[docID]['matchedTerms'][thisQueryTerm] == null) {
        docSet[docID]['matchedTerms'][thisQueryTerm] = {};
        docSet[docID]['matchedTerms'][thisQueryTerm][fieldName] = tf;
      } else {
        docSet[docID]['matchedTerms'][thisQueryTerm][fieldName] = tf;
      }
    })
    .on('end', function () {
      var k,
          resultSet,
          facetFields,
          m,
          filterIsPresent,
          l,
          totalMatchedTerms,
          score,
          searchTerm,
          IDF,
          documentHitFields,
          W,
          TF,
          documentFacetTags,
          j,
          compare;
      //move this line?
      if (idf[thisQueryTerm]) { 
        idf[thisQueryTerm] = (idf[thisQueryTerm] + idfCount);
      } else {
        idf[thisQueryTerm] = idfCount;
      }

      if (i < (indexKeys.length - 1)) {
        getSearchResults(q, tq, ++i, docSet, idf, indexKeys, callback);
      }
      else {
        //idf generation in here

        for (k in idf) {
          idf[k] = Math.log(totalIndexedFields / idf[k]);
        }

        //generate resultset with tfidf
        resultSet = {};
        resultSet['idf'] = idf;
        resultSet['query'] = q;
        resultSet['transformedQuery'] = tq;
        resultSet['totalHits'] = 0;
        resultSet['facets'] = {};
        facetFields = [];

        if (q['facets']){
          facetFields = q['facets'];
        }
        else {
          facetFields = availableFacets; 
        }
        
        for (m = 0; m < facetFields.length; m++) {
          resultSet['facets'][facetFields[m]] = {};
        }

        resultSet['hits'] = [];

        docSetLoop:
        for (j in docSet) {
          //deal with filtering


          for (k in q.filter) {
            filterIsPresent = false;
            for (l = 0; l < q.filter[k].length; l++) {
              //if the filter field is missing- drop hit
              if (docSet[j].document[k] === undefined)
                continue docSetLoop;
              //if the filter value is present- mark as true
              if (docSet[j].document[k].indexOf(q.filter[k][l]) != -1)
                filterIsPresent = true;
            }
            //if this is still false, the hit did not contain the
            //right filter field value anywhere in the filter field
            //array
            if (!filterIsPresent) {
              continue docSetLoop;
            }
          }


          totalMatchedTerms = Object.keys(docSet[j]['matchedTerms']).length;
          if (totalMatchedTerms < queryTerms.length) {
            continue docSetLoop;
          }
          else {
            hit = docSet[j];
            hit['document']['id'] = j;
            score = 0;
            for (k in idf) {
              searchTerm = k;
              IDF = idf[k];
              documentHitFields = hit['matchedTerms'][k];
              for (l in documentHitFields) {
                //weighting
                W = 1;
                if (weight[l]) {
                  W = parseInt(weight[l]);
                }
                TF = documentHitFields[l];
                score += (TF * IDF * W);
              }
              hit['score'] = score;
            }
            //faceting
            for (m = 0; m < facetFields.length; m++) {
              if (hit.document[facetFields[m]]) {
                documentFacetTags = hit.document[facetFields[m]];
                for (var n = 0; n < documentFacetTags.length; n++) {
                  if (!resultSet.facets[facetFields[m]][documentFacetTags[n]]) {
                    resultSet.facets[facetFields[m]][documentFacetTags[n]] = 0;
                  }
                  resultSet.facets[facetFields[m]][documentFacetTags[n]]++;
                }
              }
            }
            resultSet['hits'].push(hit);
          }
        }
        
        //array sort function
        compare = function(a,b) {
          if (a.score < b.score)
            return 1;
          if (a.score > b.score)
            return -1;
          return 0;
        };
        resultSet['totalHits'] = resultSet.hits.length;
        resultSet.hits = resultSet.hits.sort(compare)
          .slice(offset, (offset + pageSize));
        callback(resultSet);
      }
    });
};

