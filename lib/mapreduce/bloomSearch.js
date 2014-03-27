/*
Using least frequent term and bloom filters, generate a resultset
*/

var totalDocsInIndex = 1000 //TODO- find out the best way to do this

var async = require('async');
var scontext = require('search-context');
var bf = require('bloomfilter');

//array sort function
var sorter = function(a,b) {
  if (a.score < b.score)
    return 1;
  if (a.score > b.score)
    return -1;
  return 0;
}



exports.search = function (reverseIndex, keySet, docFreqs, q, cleanQuery, filterKeySet, callback) {
  var hits = [];
  var totalHits = 0;
  var lookUpFinished = false;
  var leastFrequentKey;
  var leastFrequentKeyFrequency;  
  var otherKeys = [];
  var facets = {};

  for (var i = 0; i < q['facets'].length; i++) {
    facets[q['facets'][i]] = {};
  }

  //index item queue
  var queue = async.queue(function (hit, callback) {
    ++totalHits;
    var score = 0;
    //work out score
    for (field in cleanQuery.query) {
      var factor = 1;
      if (q.weight) if (q.weight[field]) factor = q.weight[field];
      for (var term in hit.tf[field]) {
        score += factor*hit.tf[field][term];
      }
    }
    debugger;
    hit.score = score;
    if (hits.length < q.pageSize) {
      hits.push(hit);
      hits.sort(sorter);
    }
    else if (hit.score < hits[q.pageSize-1].score) {
      hits.pop();
      hits.push(hit);
      hits.sort(sorter);
    }
    //faceting
    //loop through requested facet categories
    for (var i = 0; i < q['facets'].length; i++) {
      //check if thsi hit has facets in the requested category
      if (hit.facetValues[q['facets'][i]]) {
        var facetCat = q['facets'][i];
        //loop through all facets in requested cat
        for (var j = 0; j < hit.facetValues[facetCat].length; j++){
          var thisFacetValue = hit.facetValues[facetCat][j];
          if (facets[facetCat][thisFacetValue] == undefined)
            facets[facetCat][thisFacetValue] = 1;
          else
            facets[facetCat][thisFacetValue]++;
        }
      }
    }
    delete hit.facetValues;
    callback();
  }, 1000);


  //glue doc to result
  var fetchAndGlueDocument = function (item, callback) {
    reverseIndex.get('DOCUMENT~' + item.id + '~', function (err, value) {
      item['document'] = JSON.parse(value);
      //teaser generation

      if (q.teaser) {
        try {
          item.document['teaser'] = 
            scontext(item.document[q.teaser],
                     cleanQuery.query[q.teaser], 400,
                     function hi (string) {
                       return '<span class="sc-em">' + string + '</span>'
                     });
        } catch (e) {
          console.log('error with teaser generation: ' + e)
        }
      }
      delete item['document']['forage.composite'];
    
      callback(null, item);
    });
  }

  var sendResultSet = function() {
    var resultSet = {};
//    console.log('totalHits: ' + totalHits);
    resultSet['query'] = '';
    resultSet['totalHits'] = '';
    resultSet['transformedQuery'] = '';
    resultSet['facets'] = '';
    resultSet['hits'] = hits;
    async.map(resultSet.hits, fetchAndGlueDocument, function(err){
      resultSet['query'] = q;
      resultSet['totalHits'] = totalHits;
      resultSet['transformedQuery'] = cleanQuery;
      resultSet['facets'] = facets;
      callback(resultSet);
    });
  }

  for (var k in docFreqs) {
    otherKeys.push(k);
    if ((!leastFrequentKey) || (docFreqs[k] < leastFrequentKeyFrequency)) {
      leastFrequentKeyFrequency = docFreqs[k];
      leastFrequentKey = k;
    }
  }
  otherKeys.splice(otherKeys.indexOf(leastFrequentKey), 1);

  var idf = {};

  for (var field in cleanQuery.query) {
    for (var i = 0; i < cleanQuery.query[field].length; i++) {
      var term = cleanQuery.query[field][i];
      idf[term]
        = Math.log(totalDocsInIndex / docFreqs[term + '~~~' + field]);
    }
  }
/*
  console.log('idf: ' + JSON.stringify(idf));
  console.log('otherKeys: ' + otherKeys);
  console.log('docFreqs: ' + JSON.stringify(docFreqs));
  console.log('leastFrequentKey: ' + leastFrequentKey);
*/
  var vectorsRequested = 0;
  var vectorsProcessed = null;
  var reverseIndexFetchCursor = 0;
  var reverseIndexFetchFinalCount = null;
  reverseIndex.createReadStream({
    valueEncoding: 'json',
    reverse: true,
    start: 'REVERSEINDEX~' + leastFrequentKey + '~~',
    end: 'REVERSEINDEX~' + leastFrequentKey + '~'})
    .on('data', function (data) {
      ++reverseIndexFetchCursor;
      var keyFields = data.key.split('~');
      var fieldName = keyFields[4]; //'title' or 'body' for example
      var docID = keyFields[6];
      var bloom = new bf.BloomFilter(JSON.parse(data.value.bloom), 3);
      var bloomFilterTrue = true;
      for (var i = 0; i < otherKeys.length; i++) {
        bloomFilterTrue = bloom.test(otherKeys[i]);
      }
      var filterSatisfied = true;
      for (var i = 0; i < filterKeySet.length; i++) {
        if (data.value.filters.indexOf(filterKeySet[i]) == -1)
          filterSatisfied = false;
      }
      if (bloomFilterTrue && filterSatisfied) {
        getVectorField = function (counter, hit) {
          hit.id = docID;
          if (counter == Object.keys(cleanQuery.query).length) {
//            console.log(hit);
            queue.push(hit, function (err) {
//              console.log('reverseIndexFetchCursor: ' + reverseIndexFetchCursor);
//              console.log('reverseIndexFetchFinalCount: ' + reverseIndexFetchFinalCount);
              vectorsProcessed++;
              if (reverseIndexFetchCursor == reverseIndexFetchFinalCount)
                sendResultSet();
            });
            return;
          }
          if (!hit.tf) hit.tf = {}; 
          var field = Object.keys(cleanQuery.query)[counter];
          hit.tf[field] = {};
          var vectorKey = 'VECTOR~' + field + '~' + docID + '~';
          reverseIndex.get(vectorKey, function (err, val){
//            console.log('looking up vectorKey ' + vectorKey);
            var termIsInVector = [];
            for (var i = 0; i < cleanQuery.query[field].length; i++) {
              var term = cleanQuery.query[field][i];
              if (val.vector[term]) {
                termIsInVector.push(true);
                hit.tf[field][term] = val.vector[term];
              }
              else {
                termIsInVector.push(false);
              }
            }
            if (termIsInVector.indexOf(false) == -1) {
              hit.facetValues = val.facetValues;
              getVectorField(++counter, hit);
            }
            else {vectorsProcessed++};
          });
        }
        vectorsRequested++;
        getVectorField(0, {});
      }
    }).on('end', function () {
      reverseIndexFetchFinalCount = reverseIndexFetchCursor;
      if (vectorsRequested == vectorsProcessed) sendResultSet();
    });
}



