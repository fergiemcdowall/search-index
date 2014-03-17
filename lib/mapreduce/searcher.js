var stopwords = require('natural').stopwords;
var simpleOneTokenSearch = require('./simpleOneTokenSearch.js');
var fullSearch = require('./fullSearch.js');
var bloomSearch = require('./bloomSearch.js');

exports.search = function (reverseIndex, docFreqIndex, q, callback) {

  var cleanQuery = {};
  var canSearch = true;
  var keySet = [];

  //remove stopwords
  cleanQuery['query'] = {};
  cleanQuery['query']['forage.composite'] = [];
  for (k = 0; k < q['query']['*'].length; k++) {
    if (stopwords.indexOf(q['query']['*'][k]) == -1) {
      cleanQuery['query']['forage.composite'].push(q['query']['*'][k]);
    }
  }
  if (cleanQuery['query']['forage.composite'].length == 0) {
    canSearch = false;
  }
  console.log(cleanQuery);
  pickSearch = function(docFreqs) {

    defaultSearch = 'STANDARD-BLOOM';
    searchType = defaultSearch;

    //Check for single token no navs query
    if (keySet.length == 1)
      searchType = 'SINGLE-KEY-SEARCH';
    else
      searchType = 'STANDARD-BLOOM';

    console.log(searchType);
  
    if (searchType == 'SINGLE-KEY-SEARCH') {
      simpleOneTokenSearch.search(reverseIndex, docFreqs, docFreqIndex, q, cleanQuery, function(msg) {
        callback(msg);
      });
    }
    else if (searchType == 'STANDARD-BLOOM') {
      bloomSearch.search(reverseIndex, docFreqs, q, cleanQuery, filterKeySet, function(msg) {
        callback(msg);
      });
    }
    else {
      fullSearch.search(reverseIndex, docFreqIndex, q, function(msg) {
        callback(msg);
      });
    }
  }


  //generate keyset
//  var filter = [];
//  if (q.filter) filter = filter.concat(q.filter);
  console.log('filter: ' + JSON.stringify(q.filter));
  for (var j = 0; j < cleanQuery.query['forage.composite'].length; j++) {
    if (q.filter) {
      for (k in q.filter) {
        console.log('filter[k]: ' + q.filter[k]);
        for (var i = 0; i < q.filter[k].length; i++) {
          keySet.push(cleanQuery.query['forage.composite'][j] + '~'
                      + k + '~' + q.filter[k][i] + '~forage.composite');
        }
      }
    }
    else {
      keySet.push(cleanQuery.query['forage.composite'][j] + '~~~forage.composite');
      //
    }
  }
  console.log('keySet: ' + keySet);

  var filterKeySet = [];
  for (filterCat in q.filter) {
    for (var i = 0; i < q.filter[filterCat].length; i++) {
      filterKeySet.push(filterCat + '~' + q.filter[filterCat][i]);
    } 
  }
  console.log('filterKeySet: ' + filterKeySet);
  
  //Check document frequencies
  var docFreqs = {};
  var docFreq = function (keySet, i) {
    docFreqIndex.createReadStream({
      valueEncoding: 'json',
      limit: 20,
      start: keySet[i],
      end: keySet[i] + '~'})
      .on('data', function (data) {
        docFreqs[data.key] = data.value;
      })
      .on('error', function (err) {
        console.log('Oh my!', err)
      })
      .on('end', function () {
        if (++i < keySet.length)
          docFreq(keySet, i);  
        else pickSearch(docFreqs);
      });
  }
  docFreq(keySet, 0);
};
