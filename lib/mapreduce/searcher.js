var stopwords = require('natural').stopwords;
var simpleOneTokenSearch = require('./simpleOneTokenSearch.js');
var fullSearch = require('./fullSearch.js');
var bloomSearch = require('./bloomSearch.js');

exports.search = function (reverseIndex, docFreqIndex, q, callback) {

  var canBloomSearch = true;
  var cleanQuery = {};
  var canSearch = true;
  var singleTokenNoNavs = false;

  //remove stopwords
  cleanQuery['query'] = [];
  for (k = 0; k < q['query'].length; k++) {
    if (stopwords.indexOf(q['query'][k]) == -1) {
      cleanQuery['query'].push(q['query'][k]);
    }
  }
  if (cleanQuery['query'].length === 0) {
    canSearch = false;
  }

  pickSearch = function() {
    console.log(docFreqs);
    //Check for single token no navs query
    if ((cleanQuery['query'].length == 1) && (!q['navs'])) {
      var singleTokenNoNavs = true;
    }
        
    if (singleTokenNoNavs) {
      simpleOneTokenSearch.search(reverseIndex, docFreqIndex, q, cleanQuery, function(msg) {
        callback(msg);
      });
    }
    else if (canBloomSearch) {
      bloomSearch.search(reverseIndex, docFreqs, q, cleanQuery, function(msg) {
        callback(msg);
      });
    }
    else {
      fullSearch.search(reverseIndex, docFreqIndex, q, function(msg) {
        callback(msg);
      });
    }
  }



  //Check document frequencies
  var docFreqs = {}
  var docFreq = function (qa, i) {
    docFreqIndex.get(qa[i], function (err, value) {
      docFreqs[qa[i]] = value;
      if (++i < qa.length)
        docFreq(qa, i);
      else
        pickSearch();
    });
  }
  docFreq(cleanQuery['query'], 0);

};
