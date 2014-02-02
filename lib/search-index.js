var fs = require('fs'),
level = require('level'),
indexPeek = require('./indexPeek.js'),
matcher = require('./matcher.js'),
calibrater = require('./calibrater.js'),
indexer = require('./indexer.js'),
searcher = require('./searcher.js'),
reverseIndex = level('si', {valueEncoding: 'json'});


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
  calibrater.calibrate(reverseIndex, function(msg) {
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
  searcher.search(reverseIndex, q, function(msg){
    callback(msg);
  });
};

