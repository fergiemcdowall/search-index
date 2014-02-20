var fs = require('fs'),
level = require('level'),
indexPeek = require('./indexing/indexPeek.js'),
matcher = require('./matchers/matcher.js'),
calibrater = require('./indexing/calibrater.js'),
indexer = require('./indexing/indexer.js'),
searcher = require('./mapreduce/searcher.js'),
reverseIndex = level('si', {valueEncoding: 'json'});
docFreqIndex = level('df', {valueEncoding: 'json'});


exports.indexPeek = function(start, stop, callback) {
  indexPeek.indexPeek(start, stop, reverseIndex, function(msg) {
    callback(msg);
  });
};


exports.generateMatcher = function(callback) {
  matcher.generateMatcher(reverseIndex, function(msg) {
    callback(msg);
  });
}


exports.matcher = function(beginsWith, callback) {
  matcher.matcher(beginsWith, function(msg) {
    callback(msg);
  });
}


exports.calibrate = function(callback) {
  calibrater.calibrate(reverseIndex, docFreqIndex, function(msg) {
    callback(msg);
  });
};


exports.deleteDoc = function(docID, callback) {
  indexer.deleteDoc(reverseIndex, docID, function(msg) {
    callback(msg);
  });
};


exports.index = function(batchString, batchName, filters, callback) {
  indexer.index(reverseIndex, batchString, batchName, filters, function(msg) {
    callback(msg);
  });
};


exports.search = function (q, callback) {
  searcher.search(reverseIndex, docFreqIndex, q, function(msg){
    callback(msg);
  });
};

