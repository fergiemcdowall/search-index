/*
Using least frequent term and bloom filters, generate a resultset
*/
var async = require('async');
var bf = require('bloomfilter');
var scontext = require('search-context');

//array sort function
var sorter = function(a,b) {
  if (a.score < b.score)
    return 1;
  if (a.score > b.score)
    return -1;
  return 0;
}



exports.search = function (reverseIndex, reverseIndexMultiply, docFreqs, q, cleanQuery, filterKeySet, totalDocsInIndex, facets, callback) {
  var hits = [];
  var totalHits = 0;
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
      //default to forage.composite
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

/*
  console.log('idf: ' + JSON.stringify(idf));
  console.log('otherKeys: ' + otherKeys);
  console.log('docFreqs: ' + JSON.stringify(docFreqs));
  console.log('leastFrequentKey: ' + leastFrequentKey);
*/

  //if a key has a frequency of 0, since forage only supports ANDing,
  //the result set is empty
  if (docFreqs[leastFrequentKey] == 0) {
    sendResultSet();
  }

  var docsWithBloomAndFilterTrue = [];
  var startKey = 'REVERSEINDEX~' + leastFrequentKey.substring(3, leastFrequentKey.length) + '~';
  var endKey = 'REVERSEINDEX~' + leastFrequentKey.substring(3, leastFrequentKey.length) + '~~';
  reverseIndex.createReadStream({
    valueEncoding: 'json',
    limit: seekLimit,
    start: startKey,
    end: endKey})
    .on('data', function (data) {
      var keyFields = data.key.split('~');
      var fieldName = keyFields[4]; //'title' or 'body' for example
      var docID = keyFields[6];
      var bloom = new bf.BloomFilter(JSON.parse(data.value.bloom), 3);
      var bloomFilterTrue = true;
      for (var i = 0; i < otherKeys.length; i++) {
//        console.log('testing bloom for ' + otherKeys[i] + ' is ' + bloom.test(otherKeys[i]));
        bloomFilterTrue = bloom.test(otherKeys[i]);
      }
      var filterSatisfied = true;
      for (var i = 0; i < filterKeySet.length; i++) {
        if (data.value.filters.indexOf(filterKeySet[i]) == -1)
          filterSatisfied = false;
      }
      if (bloomFilterTrue && filterSatisfied) {
//        docsWithBloomAndFilterTrue.push('VECTOR~' + fieldName + '~' + docID + '~');

        //possibly not wise to always return all vectors <- possible
        //performance saving for fielded queries
        docsWithBloomAndFilterTrue.push('VECTOR~*fielded~' + docID + '~');
      }
      return;
    }).on('error', function (err) {
      console.log('Oh my!', err)
    }).on('end', function () {
      reverseIndexMultiply.get(docsWithBloomAndFilterTrue, function(err, data) {
//        console.log(data);
        bloomValidation:
        for (key in data) {
          var termIsInVector = [];
          var hit = {};
          hit.id = key.split('~')[2];
          hit.score = 0;
          hit.tf = {};
          hit.tf[field] = {};
          for (var i = 0; i < cleanQuery.query[field].length; i++) {
            var fieldWeight = 1;
            var queryTerm = cleanQuery.query[field][i];
            if (!data[key][field].vector[queryTerm]) continue bloomValidation;
            //field weighting
            for (var weightedField in cleanQuery.weight) {
              if (weightedField in data[key]) {
                
/*                console.log('field: ' + weightedField);
                console.log('weight: ' + cleanQuery.weight[weightedField]);
                console.log('WEEEEEEEEEEEEIGHT ' + data[key][weightedField].vector[cleanQuery.query[field][i]]);
                console.log(data[key][weightedField]);
*/
                if (data[key][weightedField].vector[queryTerm])
                  fieldWeight = cleanQuery.weight[weightedField];
              }
            }
            if (hit.id == 351) debugger;
            hit.tf[field][queryTerm]
              = data[key][field].vector[queryTerm];
            hit.score += (hit.tf[field][queryTerm] * fieldWeight);
            
          }
          hits.push(hit);
        }
        if ((Object.keys(docFreqs).length == 1) && (leastFrequentKeyFrequency > (pageSize + offset))) {
          totalHits = docFreqs[Object.keys(docFreqs)[0]];
        }
        else {
          totalHits = hits.length;
        }
        hits.sort(sorter);
        sendResultSet();
      });
    });
}
