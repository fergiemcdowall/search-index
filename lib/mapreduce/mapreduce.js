/*
Using least frequent term and bloom filters, generate a resultset
*/
var async = require('async');
var scontext = require('search-context');

//array sort function
var sorter = function(a,b) {
  if (a.score < b.score)
    return 1;
  if (a.score > b.score)
    return -1;
  return 0;
}

//see if id is present in sorted array
var inSortedArray = function (id, arr) {
  //performance: these values could be precalculated
  var rangeFloor = 0, rangeCeil = 0;
  if(typeof arr != "undefined")
    rangeCeil = arr.length;
  var medianIndex = Math.floor(rangeCeil/2);
  while ((rangeCeil - rangeFloor) > 0) {
    //this is the matching id
    if (id == arr[medianIndex]) {
      return true;
    }
    //this is the last possible ID and it does not match
    else if ((rangeCeil - rangeFloor) == 1) {
      return false;
    }
    //this id is greater than the element median index: adjust up
    else if (String(id) > String(arr[medianIndex])) {
      rangeFloor = medianIndex;
      medianIndex += Math.ceil((rangeCeil - rangeFloor)/2);
    }
    //this id is less than the element median index: adjust down
    else if (String(id) < String(arr[medianIndex])) {
      rangeCeil = medianIndex;
      medianIndex -= Math.ceil((rangeCeil - rangeFloor)/2);
    }
  }
  //no match
  return false;
}


exports.search = function (reverseIndex,
                           reverseIndexMultiply,
                           docFreqs,
                           q,
                           cleanQuery,
                           filterKeySet,
                           allDocsIDsInResultSet,
                           totalDocsInIndex,
                           facets,
                           callback) {
  var hits = [];
  var totalHits;

  if(typeof allDocsIDsInResultSet != "undefined")
    totalHits = allDocsIDsInResultSet.length;
  else
    totalHits = 0;

  var lookUpFinished = false;
  var leastFrequentKey;
  var leastFrequentKeyFrequency;  
  var otherKeys = [];

  var offset = 0;
  if (q['offset']) offset = parseInt(q['offset']);

  var pageSize = 100;
  if (q['pageSize']) pageSize = parseInt(q['pageSize']);

  for (var k in docFreqs) {
    otherKeys.push(k);
    if ((!leastFrequentKey) || (docFreqs[k] < leastFrequentKeyFrequency)) {
      leastFrequentKeyFrequency = docFreqs[k];
      leastFrequentKey = k;
    }
  }
  otherKeys.splice(otherKeys.indexOf(leastFrequentKey), 1);

  var seekLimit = -1;
  if ((Object.keys(docFreqs).length == 1) && (leastFrequentKeyFrequency > (pageSize + offset))) {
    seekLimit = Math.ceil((offset + pageSize) * 1.2);
  }

  //glue doc to result
  var fetchAndGlueDocument = function (item, callback) {
    reverseIndex.get('DOCUMENT~' + item.id + '~', function (err, value) {
      item['document'] = JSON.parse(value);
      //teaser generation
      var terms = q['query']['*'];
      if (q['query'][q.teaser])
        terms = q['query'][q.teaser];
      if (q.teaser && item.document[q.teaser]) {
        try {
          item.document['teaser'] = 
            scontext(item.document[q.teaser],
                     terms, 400,
                     function hi (string) {
                       return '<span class="sc-em">' + string + '</span>'
                     });
        } catch (e) {
          console.log('error with teaser generation: ' + e)
        }
      }
      delete item['document']['*'];
    
      callback(null, item);
    });
  }

  var sendResultSet = function() {
    var resultSet = {};
    resultSet['query'] = '';
    resultSet['totalHits'] = '';
    resultSet['transformedQuery'] = '';
    resultSet['facets'] = facets;
    resultSet['hits'] = hits.slice(offset, (offset + pageSize));
    async.map(resultSet.hits, fetchAndGlueDocument, function(err){
      resultSet['query'] = q;
      resultSet['totalHits'] = totalHits;
      resultSet['transformedQuery'] = cleanQuery;
      resultSet['facets'] = facets;
      return callback(resultSet);
    });
  }

  var idf = {};

  for (var field in cleanQuery.query) {
    for (var i = 0; i < cleanQuery.query[field].length; i++) {
      var term = cleanQuery.query[field][i];
      idf[term]
        = Math.log(totalDocsInIndex / docFreqs['TF~' + field + '~' + term + '~~']);
    }
  }


  //if a key has a frequency of 0, since search-index only supports ANDing,
  //the result set is empty
  if (docFreqs[leastFrequentKey] == 0) {
    return sendResultSet();
  }



}
