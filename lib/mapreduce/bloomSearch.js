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



exports.search = function (reverseIndex, reverseIndexMultiply, keySet, docFreqs, q, cleanQuery, filterKeySet, totalDocsInIndex, callback) {
  var hits = [];
  var totalHits = 0;
  var lookUpFinished = false;
  var leastFrequentKey;
  var leastFrequentKeyFrequency;  
  var otherKeys = [];
  var facets = {};

  //init facets
  if (q['facets']) {
    for (var i = 0; i < q['facets'].length; i++) {
      facets[q['facets'][i]] = {};
    }
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
    var pageSize = 1000;
    var offset = 0;
    if (q['offset']) offset = parseInt(q['offset']);
    if (q['pageSize']) pageSize = parseInt(q['pageSize']);
    resultSet['query'] = '';
    resultSet['totalHits'] = '';
    resultSet['transformedQuery'] = '';
    resultSet['facets'] = '';
    resultSet['hits'] = hits.slice(offset, (offset + pageSize));
    async.map(resultSet.hits, fetchAndGlueDocument, function(err){
      resultSet['query'] = q;
      resultSet['totalHits'] = totalHits;
      resultSet['transformedQuery'] = cleanQuery;
      resultSet['facets'] = facets;
      return callback(resultSet);
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
        = Math.log(totalDocsInIndex / docFreqs['TF~' + term + '~~~' + field]);
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
  reverseIndex.createReadStream({
    valueEncoding: 'json',
    start: 'REVERSEINDEX~' + leastFrequentKey.substring(3, leastFrequentKey.length) + '~',
    end: 'REVERSEINDEX~' + leastFrequentKey.substring(3, leastFrequentKey.length) + '~~'})
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
        bloomValidation:
        for (key in data) {
          //console.log(data[key]);
          var termIsInVector = [];
          var hit = {};
          hit.id = key.split('~')[2];
          hit.score = 0;
          hit.tf = {};
          hit.tf[field] = {};
          for (var i = 0; i < cleanQuery.query[field].length; i++) {
            var fieldWeight = 1;
            if (!data[key][field].vector[cleanQuery.query[field][i]]) continue bloomValidation;
            //field weighting
            for (var weightedField in cleanQuery.weight) {
              if (weightedField in data[key]) {
                
/*                console.log('field: ' + weightedField);
                console.log('weight: ' + cleanQuery.weight[weightedField]);
                console.log('WEEEEEEEEEEEEIGHT ' + data[key][weightedField].vector[cleanQuery.query[field][i]]);
                console.log(data[key][weightedField]);
*/
                if (data[key][weightedField].vector[cleanQuery.query[field][i]])
                  fieldWeight = cleanQuery.weight[weightedField];
              }
            }
            hit.tf[field][cleanQuery.query[field][i]]
              = data[key][field].vector[field];
            hit.score += (hit.tf[field][cleanQuery.query[field][i]] * fieldWeight);
            
          }

          if (q['facets']) {
            for (var i = 0; i < q['facets'].length; i++) {
              //check if this hit has facets in the requested category
              if (data[key][field].facetValues[q['facets'][i]]) {
                var facetCat = q['facets'][i];
                //loop through all facets in requested cat
                for (var j = 0; j < data[key][field].facetValues[facetCat].length; j++){
                  var thisFacetValue = data[key][field].facetValues[facetCat][j];
                  if (facets[facetCat][thisFacetValue] == undefined)
                    facets[facetCat][thisFacetValue] = 1;
                  else
                    facets[facetCat][thisFacetValue]++;
                }
              }
            }
          }

          hits.push(hit);
        }
        totalHits = hits.length;
        console.log(totalHits);
        hits.sort(sorter);
        sendResultSet();
      });
    });
}
