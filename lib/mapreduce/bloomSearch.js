/*
Using least frequent term and bloom filters, generate a resultset
*/

var totalDocsInIndex = 100000 //TODO- find out the best way to do this

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



exports.search = function (reverseIndex, docFreqs, q, cleanQuery, filterKeySet, callback) {
  var hits = [];
  var totalHits = 0;
  var vectorsRequested = 0;
  var vectorsProcessed = 0;
  var queueWorkersPushed = 0;
  var queueWorkersFinished = 0;
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
    var tf = {};
    hit.score = 0;
    for (var i = 0; i < cleanQuery.query['forage.composite'].length; i++) {
      tf[cleanQuery.query['forage.composite'][i]] =
        hit.vector[cleanQuery.query['forage.composite'][i]];
      hit.score += parseFloat(tf[cleanQuery.query['forage.composite'][i]]);
    }
    delete hit.vector;
    hit.relevance = {};
    hit.relevance.tf = tf;
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
      if (q['teaser']) {
        try {
          debugger;
          item.document['teaser'] = 
            scontext(item.document[q['teaser']],
                     cleanQuery.query['forage.composite'], 400,
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
  for (var i = 0; i < cleanQuery.query['forage.composite'].length; i++) {
    idf[cleanQuery.query['forage.composite'][i]]
      = Math.log(totalDocsInIndex / cleanQuery.query['forage.composite'][i]);
  }

  console.log('otherKeys: ' + otherKeys);
  console.log('docFreqs: ' + JSON.stringify(docFreqs));
  console.log('leastFrequentKey: ' + leastFrequentKey);

  reverseIndex.createReadStream({
    valueEncoding: 'json',
    reverse: true,
    start: 'REVERSEINDEX~' + leastFrequentKey + '~~',
    end: 'REVERSEINDEX~' + leastFrequentKey + '~'})
    .on('data', function (data) {
//      console.log(data.key);
      var keyFields = data.key.split('~');
      var JSONValue = undefined;
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
        var bloomValidated = false;
        vectorsRequested++;
        reverseIndex.get('VECTOR~forage.composite~' + keyFields[6] + '~', function (err, value){
          vectorsProcessed++;
          var hit = {};
          //test for intersections with other keys
          for (var i = 0; i < otherKeys.length; i++) {
            if (value.vector[otherKeys[i].split('~')[0]]) {
              bloomValidated = true;
            }
            else {
              bloomValidated = false;
              break;
            }
          }


          if (bloomValidated) {
            hit.id = keyFields[6];
            hit.vector = value.vector;
            hit.facetValues = value.facetValues;
            queueWorkersPushed++;
            queue.push(hit, function (err) {
              queueWorkersFinished++;
/*
                console.log(lookUpFinished + ' '
                            + vectorsRequested + ' '
                            + vectorsProcessed + ' '
                            + queueWorkersFinished + ' '
                            + queueWorkersPushed + ' queuepushed');
*/
              if (lookUpFinished
                  && (vectorsRequested == vectorsProcessed)
                  && (queueWorkersFinished == queueWorkersPushed)) {
                sendResultSet();
              }

            });
          }
          if (lookUpFinished
              && (vectorsRequested == vectorsProcessed)
              && (queueWorkersFinished == queueWorkersPushed)) {
            sendResultSet();
          }
        });
      }
    }).on('end', function () {
      lookUpFinished = true;
      if ((vectorsRequested == vectorsProcessed)
          && (queueWorkersFinished == queueWorkersPushed)) {
        sendResultSet();
      }
    });
}
