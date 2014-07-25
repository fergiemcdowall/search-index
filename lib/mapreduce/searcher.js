var stopwords = require('natural').stopwords;
var simpleOneTokenSearch = require('./simpleOneTokenSearch.js');
var fullSearch = require('./fullSearch.js');
var bloomSearch = require('./bloomSearch.js');
var logger = require('../logger.js');
var totalDocs = 0;


//used on startup
exports.setTotalDocs = function (td) {
  totalDocs = td;
}

exports.search = function (indexes, indexesMultiply, q, callback) {
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
  
  if (q.weight) {
    cleanQuery['weight'] = q.weight;
  }

  logger.debug('cleanQuery: ' + JSON.stringify(cleanQuery));

  pickSearch = function(docFreqs) {
    bloomSearch.search(indexes, indexesMultiply, docFreqs, q, cleanQuery, filterKeySet, totalDocs, function(msg) {
      callback(msg);
    });
  }
  

//  var filter = [];
//  if (q.filter) filter = filter.concat(q.filter);

  var filterKeySet = [];
  for (k in q.filter) {
    for (var i = 0; i < q.filter[k].length; i++) {
      filterKeySet.push(k + '~' + q.filter[k][i]);
    }
  }
//  logger.debug('filterKeySet: ' + filterKeySet);
//  logger.debug('filter: ' + JSON.stringify(q.filter));


  //generate keyset
  for (var queryField in cleanQuery.query) {
    for (var j = 0; j < cleanQuery.query[queryField].length; j++) {
      if (q.filter) {
        for (k in q.filter) {
//          logger.debug('filter[k]: ' + q.filter[k]);
          for (var i = 0; i < q.filter[k].length; i++) {
            keySet.push('TF~' + cleanQuery.query[queryField][j] + '~'
                        + k + '~' + q.filter[k][i] + '~' + queryField);
          }
        }
      }
      else {
        keySet.push('TF~' + cleanQuery.query[queryField][j] + '~~~' + queryField);
      }
    }
  }
//  logger.debug('keySet: ' + keySet);

  var filterKeySet = [];
  for (filterCat in q.filter) {
    for (var i = 0; i < q.filter[filterCat].length; i++) {
      filterKeySet.push(filterCat + '~' + q.filter[filterCat][i]);
    } 
  }
//  logger.debug('filterKeySet: ' + filterKeySet);

  //Check document frequencies
  var docFreqs = {};
  var tfsets = [];
  var totalHits = 0;
  indexesMultiply.get(keySet, function(err, data) {
    for (k in data) {
      if (data[k] == null) {
        //return a zero result
        docFreqs[keySet[i]] = 0;
        logger.debug('KEY ' + k + ' NOT FOUND IN DICTIONARY');
      }
      else {
        totalHits = data[k].length;
        docFreqs[k] = data[k].length;
        tfsets.push(data[k]);
      }
    }
//    console.log(tfsets);
    var intersection = tfsets[0];
    for (var i = 1; i < tfsets.length; i++) {
      intersection = intersectionDestructive(intersection, tfsets[i]);
    }
    totalHits = intersection.length;
    console.log('totalHits: ' + totalHits);
//    console.log('tntersection: ' + intersection);
    pickSearch(docFreqs);    
  });

};

function intersectionDestructive(a, b) {
  var result = new Array();
  while( a.length > 0 && b.length > 0 ) {
     if      (a[0] < b[0]){ a.shift(); }
     else if (a[0] > b[0]){ b.shift(); }
     else /* they're equal */ {
       result.push(a.shift());
       b.shift();
     }
  }
  return result;
}
