var stopwords = require('natural').stopwords;
var simpleOneTokenSearch = require('./simpleOneTokenSearch.js');
var fullSearch = require('./fullSearch.js');
var bloomSearch = require('./bloomSearch.js');
var totalDocs = 0;

//used on startup
exports.setTotalDocs = function (td) {
  totalDocs = td;
}

exports.search = function (reverseIndex, docFreqIndex, q, callback) {

  var cleanQuery = {};
  var canSearch = true;
  var keySet = [];

  //remove stopwords
  cleanQuery['query'] = {};
  for (var searchField in q['query']) {
    var cleanQueryFieldToken = searchField;
//    if (searchField == '*') cleanQueryFieldToken = 'forage.composite'
    cleanQuery['query'][cleanQueryFieldToken] = [];
    for (var k = 0; k < q['query'][searchField].length; k++) {
      cleanQuery['query'][cleanQueryFieldToken].push(q['query'][searchField][k]);
    }
    //TODO: does this make sense?
    if (cleanQuery['query'][cleanQueryFieldToken].length == 0) {
      canSearch = false;
    }
  }
  console.log('cleanQuery: ' + JSON.stringify(cleanQuery));

  pickSearch = function(docFreqs) {
    defaultSearch = 'STANDARD-BLOOM';
    searchType = defaultSearch;


    //Check for single token no navs query
/*
    if (keySet.length == 1)
      searchType = 'SINGLE-KEY-SEARCH';
    else
      searchType = 'STANDARD-BLOOM';
*/
//    console.log(searchType);
  
    if (searchType == 'SINGLE-KEY-SEARCH') {
      simpleOneTokenSearch.search(reverseIndex, docFreqs, docFreqIndex, q, cleanQuery, function(msg) {
        callback(msg);
      });
    }
    else if (searchType == 'STANDARD-BLOOM') {
      bloomSearch.search(reverseIndex, keySet, docFreqs, q, cleanQuery, filterKeySet, totalDocs, function(msg) {
        callback(msg);
      });
    }
    else {
      fullSearch.search(reverseIndex, docFreqIndex, q, function(msg) {
        callback(msg);
      });
    }
  }


//  var filter = [];
//  if (q.filter) filter = filter.concat(q.filter);

  var filterKeySet = [];
  for (k in q.filter) {
    for (var i = 0; i < q.filter[k].length; i++) {
      filterKeySet.push(k + '~' + q.filter[k][i]);
    }
  }
//  console.log('filterKeySet: ' + filterKeySet);
//  console.log('filter: ' + JSON.stringify(q.filter));


  //generate keyset
  for (var queryField in cleanQuery.query) {
    for (var j = 0; j < cleanQuery.query[queryField].length; j++) {
      if (q.filter) {
        for (k in q.filter) {
//          console.log('filter[k]: ' + q.filter[k]);
          for (var i = 0; i < q.filter[k].length; i++) {
            keySet.push(cleanQuery.query[queryField][j] + '~'
                        + k + '~' + q.filter[k][i] + '~' + queryField);
          }
        }
      }
      else {
        keySet.push(cleanQuery.query[queryField][j] + '~~~' + queryField);
      }
    }
  }
//  console.log('keySet: ' + keySet);

  var filterKeySet = [];
  for (filterCat in q.filter) {
    for (var i = 0; i < q.filter[filterCat].length; i++) {
      filterKeySet.push(filterCat + '~' + q.filter[filterCat][i]);
    } 
  }
//  console.log('filterKeySet: ' + filterKeySet);
  
  //Check document frequencies
  //this should be a .get
  var docFreqs = {};

  var docFreq = function (keySet, i) {
    docFreqIndex.get(keySet[i], function (err, value) {
      docFreqs[keySet[i]] = value;
      if (err) if (err.notFound) console.log('KEY ' + keySet[i] + ' NOT FOUND IN DICTIONARY');
      if (++i < keySet.length) docFreq(keySet, i);  
      else pickSearch(docFreqs);
    })
  }
  docFreq(keySet, 0);
};
