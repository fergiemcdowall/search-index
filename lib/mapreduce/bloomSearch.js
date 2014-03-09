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



exports.search = function (reverseIndex, docFreqs, q, cleanQuery, callback) {
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

  //index item queue
  var queue = async.queue(function (hit, callback) {
    ++totalHits;
    var tf = {};
    hit.score = 0;
    for (var i = 0; i < cleanQuery.query.length; i++) {
      tf[cleanQuery.query[i]] = hit.vector[cleanQuery.query[i]];
      hit.score += parseFloat(tf[cleanQuery.query[i]]);
    }
    delete hit['vector'];
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
    callback();
  }, 1000);


  //glue doc to result
  var fetchAndGlueDocument = function (item, callback) {
    reverseIndex.get('DOCUMENT~' + item.id + '~', function (err, value) {
      item['document'] = JSON.parse(value);
      //teaser generation
      if (q['teaser']) {
        try {
          item['document']['teaser'] = 
            scontext(item['document'][q['teaser']],
                     queryTerms, 400,
                     function hi (string) {
                       return '<span class="sc-em">' + string + '</span>'
                     });
        } catch (e) {}
      }
      delete item['document']['forage.composite'];
      callback(null, item);
    });
  }

  var sendResultSet = function() {
    var resultSet = {};
    resultSet['query'] = q;
    resultSet['totalHits'] = totalHits;
    resultSet['transformedQuery'] = cleanQuery;
    resultSet['hits'] = hits;
    console.log('totalHits: ' + totalHits);
    async.map(resultSet.hits, fetchAndGlueDocument, function(err){
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
  for (var i = 0; i < cleanQuery.query.length; i++) {
    idf[cleanQuery.query[i]] = Math.log(totalDocsInIndex / cleanQuery.query[i]);
  }

  reverseIndex.createReadStream({
    valueEncoding: 'json',
    start: 'REVERSEINDEX~' + leastFrequentKey + '~~~forage.composite~',
    end: 'REVERSEINDEX~' + leastFrequentKey + '~~~forage.composite~~'})
    .on('data', function (data) {
//      console.log(data.key);
      var keyFields = data.key.split('~');
      var JSONValue = undefined;
      var bloom = new bf.BloomFilter(JSON.parse(data.value.bloom), 3);
      var bloomFilterTrue = true;
      for (var i = 0; i < otherKeys.length; i++) {
        bloomFilterTrue = bloom.test(otherKeys[i]);
      }
      if (bloomFilterTrue) {
        var bloomValidated = false;
        vectorsRequested++;
        reverseIndex.get('VECTOR~forage.composite~' + keyFields[6] + '~', function (err, value){
          vectorsProcessed++;
          var hit = {};
          for (var i = 0; i < otherKeys.length; i++) {
            if (value[otherKeys[i]]) {
              bloomValidated = true;
            }
            else {
              bloomValidated = false;
              break;
            }
          }
          if (bloomValidated) {
            hit.id = keyFields[6];
            hit.vector = value;
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
